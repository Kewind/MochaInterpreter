module Interpreter where

import AbsMocha
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error
import qualified Data.Map as Map
import ErrM
import Misc
import Types

getStartingFunction :: Semantics FuncType
getStartingFunction = do
    func <- getFuncByIdent $ Ident "main"
    if (checkStartingFunctionType func) then
        return func
    else
        throwError "Main function must return Int!"

checkStartingFunctionType :: FuncType -> Bool
checkStartingFunctionType (FuncType (FuncDef Int _ _ _) _) = True
checkStartingFunctionType _ = False

transProgram :: Program -> Semantics Env
transProgram (Program funcDecls) = do
    env <- ask
    env <- local (const env) (addBuiltinFuncs builtinFuncs)
    env <- local (const env) (transFunDecls funcDecls)
    env <- local (const env) (updateFunDefs funcDecls)
    (FuncType (FuncDef tp ident args body) funcEnv) <- local (const env) getStartingFunction
    env <- local (const env) (transStmt (BStmt body))
    return env

transFunDecls :: [Func] -> Semantics Env
transFunDecls (x:xs) = do
    env <- addFunDef x
    local (const env) (transFunDecls xs)
transFunDecls [] = do
    env <- ask
    return env

transFunBody :: [Stmt] -> Semantics Env
transFunBody (x:xs) = do
    env <- transStmt x
    local (const env) (transFunBody xs)
transFunBody [] = do
    env <- ask
    return env

transStmt :: Stmt -> Semantics Env
transStmt Empty = do
    env <- ask
    return env
transStmt (BStmt (Block (x:xs))) = do
    env <- ask
    let newEnv = Env { vEnv = Map.empty, fEnv = Map.empty, outerEnv = env }
    resultEnv <- local (const newEnv) (transBlock (BStmt (Block (x:xs))))
    return $ outerEnv resultEnv
    where
        transBlock (BStmt (Block (x:xs))) = do
            env <- transStmt x
            wasReturn <- local (const env) checkIfWasReturn
            if (wasReturn == True) then do
                value <- local (const env) getReturnValue
                outerenv <- local (const $ outerEnv env) (addReturnValue value)
                venv <- asks vEnv
                fenv <- asks fEnv
                let newEnv = Env { vEnv = venv, fEnv = fenv, outerEnv = outerenv }
                return newEnv
            else
                local (const env) (transBlock (BStmt (Block xs)))
        transBlock (BStmt (Block [])) = do
            env <- ask
            return env
transStmt (BStmt (Block ([]))) = do
    env <- ask
    return env
transStmt (Decl tp (item:xs)) = case item of
    (Init ident expr) -> do
        value <- transExpr expr
        if (areSameType tp value) then do
            env <- addInitTypeVar tp ident value
            local (const env) (transStmt (Decl tp xs))
        else
            throwError $ "Can't assign " ++ (show ident) ++ " to a different type!"
    (NoInit ident) -> do
        env <- addNoInitTypeVar tp ident
        local (const env) (transStmt (Decl tp xs))
transStmt (Decl tp []) = do
    env <- ask
    return env
transStmt (FuncDecl type_ ident args block) = do
    env <- transFunDecls [(FuncDef type_ ident args block)]
    return env
transStmt (Ass ident expr) = do
    env <- ask
    value <- transExpr expr
    updateVarValue ident value
    return env
transStmt (Ret expr) = do
    value <- transExpr expr
    env <- addReturnValue value
    return env
transStmt VRet = do
    -- env <- addReturnValue NULL
    env <- ask
    return env
transStmt (Cond expr stmt) = do
    condition <- transExpr expr
    case condition of
        (V_BOOL cond) -> do
            env <- ask
            if (cond == True) then transStmt stmt
            else return env
        _ -> throwError "If condition must be of Boolean type!"
transStmt (CondElse expr stmt1 stmt2) = do
    condition <- transExpr expr
    case condition of
        (V_BOOL cond) -> do
            env <- ask
            if (cond == True) then transStmt stmt1
            else transStmt stmt2
        _ -> throwError "If condition must be of Boolean type!"
transStmt (While expr stmt) = do
    condition <- transExpr expr
    case condition of
        (V_BOOL cond) -> do
            if (cond == True) then do
                env <- transStmt stmt
                local (const env) (transStmt (While expr stmt))
            else do
                env <- ask
                return env
        _ -> throwError "While condition must be of Boolean type!"
transStmt (ForTo ident expr1 expr2 stmt) = transForStmt False ident expr1 expr2 stmt
transStmt (ForDownto ident expr1 expr2 stmt) = transForStmt True ident expr1 expr2 stmt
transStmt (SExp expr) = do
    env <- ask
    transExpr expr
    return env

transForStmt :: Bool -> Ident -> Expr -> Expr -> Stmt -> Semantics Env
transForStmt isDownto ident expr1 expr2 stmt = do
    varExists <- checkIfVarExists ident
    i <- transExpr expr1
    j <- transExpr expr2
    case (i,j) of
        ((V_INT i),(V_INT j)) -> do
            if (varExists == False) then do
                env2 <- addVar ident (V_INT i)
                newEnv <- local (const env2) (loopForStmt isDownto ident i j stmt)
                removeVar ident
            else do
                x <- getValueByIdent ident
                env <- ask
                newEnv <- local (const env) (loopForStmt isDownto ident i j stmt)
                updateVarValue ident x
                return env
        (_,_) -> throwError "For condition must be of Int type!"

loopForStmt :: Bool -> Ident -> Integer -> Integer -> Stmt -> Semantics Env
loopForStmt isDownto ident fromInt toInt stmt = do
    if (isDownto == True && fromInt >= toInt) then do
        updateVarValue ident (V_INT fromInt)
        env <- transStmt stmt
        local (const env) (loopForStmt isDownto ident (fromInt - 1) toInt stmt)
    else if (isDownto == False && fromInt <= toInt) then do
        updateVarValue ident (V_INT fromInt)
        env <- transStmt stmt
        local (const env) (loopForStmt isDownto ident (fromInt + 1) toInt stmt)
    else do
        env <- ask
        return env

transArgs :: [Expr] -> Semantics [ValueType]
transArgs args = do mapM (\s -> transExpr s) args

transExpr :: Expr -> Semantics ValueType
transExpr (EVar ident) = getValueByIdent ident
transExpr (ELitInt x) = return $ V_INT x
transExpr (EString s) = return $ V_STRING s
transExpr ELitTrue = return $ V_BOOL True
transExpr ELitFalse = return $ V_BOOL False
transExpr (EApp ident args) = do
    func <- getFuncByIdent ident
    argValues <- transArgs args
    if (isBuiltinFunc func) then do
        value <- invokeBuiltinFunc func argValues
        return value
    else do
        let (FuncType (FuncDef tp ident arg block) funcEnv) = func
        --let newEnv = Env { vEnv = Map.empty, fEnv = Map.empty, outerEnv = funcEnv }
        newEnv <- local (const funcEnv) (addArgsToEnv arg argValues)
        env <- local (const newEnv) (transStmt $ BStmt block)
        wasReturn <- local (const env) checkIfWasReturn
        if (wasReturn == True) then do
            case tp of
                Void -> throwError "Void functions can't return a value!"
                _ -> do
                    returnValue <- local (const env) getReturnValue
                    addReturnValue returnValue
                    return returnValue
        else
            case tp of
                Void -> do
                    return NULL
                _ -> throwError "There needs to be a return statement!"

transExpr (Neg expr) = do
    e <- transExpr expr
    case e of
        V_INT i -> return $ V_INT $ negate i
        _ -> throwError $ "Error while handling Negate operation with " ++ (show e)
transExpr (Not expr) = do
    e <- transExpr expr
    case e of
        V_BOOL b -> return $ V_BOOL $ not b
        _ -> throwError $ "Error: " ++ (show e) ++ ". You can only use Not with bool values!"
transExpr (EMul expr1 Times expr2) = do
    e1 <- transExpr expr1
    e2 <- transExpr expr2
    case (e1, e2) of
        (V_INT i1, V_INT i2) -> return $ V_INT $ i1 * i2
        _ -> throwError $ "Error: " ++ (show e1) ++ " and " ++
                (show e2) ++ ". You can only multiply integers!"
transExpr (EMul expr1 Div expr2) = do
    e1 <- transExpr expr1
    e2 <- transExpr expr2
    case (e1, e2) of
        (V_INT i1, V_INT i2) ->
            if (i2 == 0) then throwError "Can't divide by 0"
            else return $ V_INT $ div i1 i2
        _ -> throwError $ "Error: " ++ (show e1) ++ " and " ++
                (show e2) ++ ". You can only divide integer by integer"
transExpr (EMul expr1 Mod expr2) = do
    e1 <- transExpr expr1
    e2 <- transExpr expr2
    case (e1, e2) of
        (V_INT i1, V_INT i2) ->
            if (i2 == 0) then throwError "Error: Can't mod by 0"
            else return $ V_INT $ mod i1 i2
        _ -> throwError $ "Error: " ++ (show e1) ++ " and " ++
                (show e2) ++ ". Mod operation must be used on integers!"
transExpr (EAdd expr1 Plus expr2) = do
    e1 <- transExpr expr1
    e2 <- transExpr expr2
    case (e1, e2) of
        (V_INT i1, V_INT i2) -> return $ V_INT $ i1 + i2
        (V_STRING s1, V_STRING s2) -> return $ V_STRING $ s1 ++ s2
        _ -> throwError $ "Error: " ++ (show e1) ++ " and " ++
                (show e2) ++ ". You can only add two integers or two strings"
transExpr (EAdd expr1 Minus expr2) = do
    e1 <- transExpr expr1
    e2 <- transExpr expr2
    case (e1, e2) of
        (V_INT i1, V_INT i2) -> return $ V_INT $ i1 - i2
        _ -> throwError $ "Error: " ++ (show e1) ++ " and " ++
                (show e2) ++ ". You can only substract integers"
transExpr (ERel expr1 relop expr2) = do
    e1 <- transExpr expr1
    e2 <- transExpr expr2
    case (e1, e2) of
        (V_INT i1, V_INT i2) -> transIntRelOp relop i1 i2
        (V_BOOL b1, V_BOOL b2) -> transBoolRelOp relop b1 b2
        (V_STRING s1, V_STRING s2) -> transStrRelOp relop s1 s2
        _ -> throwError $ "Error: " ++ (show e1) ++ " and " ++
                (show e2) ++ ". You can only compare same types"
transExpr (EAnd expr1 expr2) = transBoolOp (&&) expr1 expr2
transExpr (EOr expr1 expr2) = transBoolOp (||) expr1 expr2

transBoolOp :: (Bool -> Bool -> Bool) -> Expr -> Expr -> Semantics ValueType
transBoolOp op expr1 expr2 =  do
    e1 <- transExpr expr1
    e2 <- transExpr expr2
    case (e1, e2) of
        (V_BOOL b1, V_BOOL b2) -> return $ V_BOOL $ op b1 b2
        (_, _) -> throwError $ "Error: " ++ (show e1) ++ " and " ++
                (show e2) ++ ". Bool operations are only allowed for bool types"

transIntRelOp :: RelOp -> Integer -> Integer -> Semantics ValueType
transIntRelOp op a b = case op of
  LTH -> return $ V_BOOL $ a < b
  LE -> return $ V_BOOL $ a <= b
  GTH -> return $ V_BOOL $ a > b
  GE -> return $ V_BOOL $ a >= b
  EQU -> return $ V_BOOL $ a == b
  NE -> return $ V_BOOL $ a /= b

transBoolRelOp :: RelOp -> Bool -> Bool -> Semantics ValueType
transBoolRelOp op a b = case op of
    EQU -> return $ V_BOOL $ a == b
    NE -> return $ V_BOOL $ a /= b
    _ -> throwError "Error: When comparing bools you can only check equality or inequality"

transStrRelOp :: RelOp -> String -> String -> Semantics ValueType
transStrRelOp op a b = case op of
    EQU -> return $ V_BOOL $ a == b
    NE -> return $ V_BOOL $ a /= b
    _ -> throwError "Error: When comparing strings you can only check equality or inequality"
