module Misc where

import AbsMocha
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error
import Types

funReturnVar :: Ident
funReturnVar = Ident "=__RETURN__VALUE__"

builtinFuncs :: [Func]
builtinFuncs = [(FuncDef Void (Ident "print") [] (Block [])),
                (FuncDef Void (Ident "toString") [] (Block [])),
                (FuncDef Void (Ident "fromString") [] (Block []))]

addBuiltinFuncs :: [Func] -> Semantics Env
addBuiltinFuncs (x:xs) = do
    venv <- asks vEnv
    fenv <- asks fEnv
    outerenv <- asks outerEnv
    let emptyEnv = Env { vEnv = Map.empty, fEnv = Map.empty,
                        outerEnv = outerenv}
    let (FuncDef _ ident _ _) = x
    let func = FuncType x emptyEnv
    let newEnv = Env { vEnv = venv, fEnv = (Map.insert ident func fenv),
                        outerEnv = outerenv}
    local (const newEnv) (addBuiltinFuncs xs)
addBuiltinFuncs [] = do
    env <- ask
    return env

isBuiltinFunc :: FuncType -> Bool
isBuiltinFunc (FuncType x _) = elem x builtinFuncs

invokeBuiltinFunc :: FuncType -> [ValueType] -> Semantics ValueType
invokeBuiltinFunc (FuncType (FuncDef tp (Ident "print") args block) _) values = do
    liftIO $ putStrLn finalString
    return NULL
    where
        valuesStrings = map (show) values
        finalString = concat valuesStrings
invokeBuiltinFunc (FuncType (FuncDef tp (Ident "toString") args block) _) (x:xs) =
    if (xs /= []) then
        throwError "toString should only have one argument!"
    else
        return $ V_STRING $ show x
invokeBuiltinFunc (FuncType (FuncDef tp (Ident "fromString") args block) _) ((V_STRING x):xs) =
    if (xs /= []) then
        throwError "fromString should only have one argument!"
    else
        case x of
            "True" -> return $ V_BOOL True
            "False" -> return $ V_BOOL False
            s -> do
                let i = read s :: Integer
                return $ V_INT i
invokeBuiltinFunc _ _ =
    throwError "Incorrect call for builtin function!"

getNewLocation :: ValueType -> Semantics Loc
getNewLocation value = do
    s <- gets (Map.lookup (LOC 0))
    case s of
        Just (V_INT newLocation) -> do
            modify $ Map.insert (LOC newLocation) value
            modify $ Map.insert (LOC 0) $ V_INT (newLocation + 1)
            return $ LOC newLocation
        Nothing -> throwError "Error when trying to assign new location"

getValueByIdent :: Ident -> Semantics ValueType
getValueByIdent ident = do
    env <- ask
    case env of
        None -> throwError $ (show ident) ++ " is not defined!"
        _ -> do
            venv <- asks vEnv
            case Map.lookup ident venv of
                Just loc -> do
                    Just value <- gets $ Map.lookup loc
                    return value
                Nothing -> do
                    outerenv <- asks outerEnv
                    value <- local (const outerenv) (getValueByIdent ident)
                    return value

getFuncByIdent :: Ident -> Semantics FuncType
getFuncByIdent ident = do
    env <- ask
    case env of
        None -> throwError $ "Can't find function " ++ show ident
        _ -> do
            fenv <- asks fEnv
            case Map.lookup ident fenv of
                Just func -> return func
                Nothing -> do
                    outerenv <- asks outerEnv
                    func <- local (const outerenv) (getFuncByIdent ident)
                    return func

addVar :: Ident -> ValueType -> Semantics Env
addVar ident value = do
    venv <- asks vEnv
    case Map.lookup ident venv of
        Just _ -> throwError $ show ident ++ " is already defined!"
        Nothing -> do
            fenv <- asks fEnv
            outerenv <- asks outerEnv
            newLoc <- getNewLocation value
            let newEnv = Env { vEnv = (Map.insert ident newLoc venv), fEnv = fenv,
                            outerEnv = outerenv}
            return newEnv

removeVar :: Ident -> Semantics Env
removeVar ident = do
    venv <- asks vEnv
    fenv <- asks fEnv
    outerenv <- asks outerEnv
    let env = Env { vEnv = (Map.delete ident venv), fEnv = fenv, outerEnv = outerenv}
    return env

checkIfVarExists :: Ident -> Semantics Bool
checkIfVarExists ident = do
    venv <- asks vEnv
    return $ Map.member ident venv

updateVarValue :: Ident -> ValueType -> Semantics ()
updateVarValue ident value = do
    env <- ask
    case env of
        None -> throwError $ "Can't update variable " ++ (show ident) ++ ", it doesn't exist!"
        _ -> do
            venv <- asks vEnv
            case Map.lookup ident venv of
                Just loc -> do
                    modify (Map.insert loc value)
                Nothing -> do
                    outerenv <- asks outerEnv
                    local (const outerenv) (updateVarValue ident value)

addOrUpdateVar :: Ident -> ValueType -> Semantics Env
addOrUpdateVar ident value = do
    venv <- asks vEnv
    if (Map.member ident venv) then do
        env <- ask
        updateVarValue ident value
        return env
    else
        addVar ident value

addInitTypeVar :: Type -> Ident -> ValueType -> Semantics Env
addInitTypeVar tp ident value = do
    case tp of
        Int -> addVar ident value
        Str -> addVar ident value
        Bool -> addVar ident value
        _ -> throwError $ "Can't add variable of type " ++ show tp

addNoInitTypeVar :: Type -> Ident -> Semantics Env
addNoInitTypeVar tp ident = do
    case tp of
        Int -> addVar ident (V_INT 0)
        Str -> addVar ident (V_STRING "")
        Bool -> addVar ident (V_BOOL False)
        _ -> throwError $ "Can't add variable of type " ++ show tp

addFunDef :: Func -> Semantics Env
addFunDef (FuncDef tp ident args body) = do
    fenv <- asks fEnv
    if (Map.member ident fenv) then
        throwError "Function names must be unique!"
    else do
        env <- ask
        venv <- asks vEnv
        outerenv <- asks outerEnv
        let funcEnv = Env { vEnv = Map.empty, fEnv = (Map.insert ident
            (FuncType (FuncDef tp ident args body) funcEnv) Map.empty),
            outerEnv = env }
        let newEnv = Env { vEnv = venv, fEnv = (Map.insert ident
            (FuncType (FuncDef tp ident args body) funcEnv) fenv),
            outerEnv = outerenv}
        return newEnv

updateFunDef :: Func -> Semantics Env
updateFunDef (FuncDef tp ident args body) = do
    venv <- asks vEnv
    fenv <- asks fEnv
    if (Map.member ident fenv) then do
        env <- ask
        outerenv <- asks outerEnv
        let updatedFunDef = FuncType (FuncDef tp ident args body) env
        let newEnv = Env { vEnv = venv, fEnv = (Map.insert ident updatedFunDef fenv),
                            outerEnv = outerenv}
        return newEnv
    else do
        throwError $ "Function " ++ (show ident) ++ " doesn't exist!"

updateFunDefs :: [Func] -> Semantics Env
updateFunDefs (x:xs) = do
    env <- updateFunDef x
    local (const env) (updateFunDefs xs)
updateFunDefs [] = do
    env <- ask
    return env

addArgsToEnv :: [Arg] -> [ValueType] -> Semantics Env
addArgsToEnv ((Arg tp ident):xs) (y:ys) = do
    if (areSameType tp y) then do
        env <- addOrUpdateVar ident y
        newEnv <- local (const env) (addArgsToEnv xs ys)
        return newEnv
    else
        throwError "Argument types must match function parameter types!"
addArgsToEnv [] [] = do
    env <- ask
    return env
addArgsToEnv _ _ = throwError "Incorrect number of arguments in a function call!"

checkIfWasReturn :: Semantics Bool
checkIfWasReturn = do
    venv <- asks vEnv
    return $ Map.member funReturnVar venv

getReturnValue :: Semantics ValueType
getReturnValue = do
    var <- getValueByIdent funReturnVar
    return var

addReturnValue :: ValueType -> Semantics Env
addReturnValue value = addVar funReturnVar value

printEnv :: Env -> Semantics ()
printEnv env = liftIO $ putStrLn ("Environment vars: " ++ (show $ vEnv env) ++ "; funs: "
    ++ show (Map.keys (fEnv env)))

printDebug :: String -> Semantics ()
printDebug s = liftIO $ putStrLn s

getStmtsFromBlock :: Block -> [Stmt]
getStmtsFromBlock (Block s) = s

areSameType :: Type -> ValueType -> Bool
areSameType (Int) (V_INT _) = True
areSameType (Str) (V_STRING _) = True
areSameType (Bool) (V_BOOL _) = True
areSameType _ _ = False
