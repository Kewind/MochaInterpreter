module Types where

import AbsMocha
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error

type Semantics = ReaderT Env (StateT St (ErrorT String IO))

type VEnv = Map.Map Ident Loc
type FEnv = Map.Map Ident FuncType

data Env = Env {
    vEnv :: VEnv,
    fEnv :: FEnv,
    outerEnv :: Env
} | None

type St = Map.Map Loc ValueType

data Loc = LOC Integer deriving (Show, Eq, Ord)

data FuncType = FuncType Func Env

data ValueType
    = NULL
    | V_INT Integer
    | V_BOOL Bool
    | V_STRING String
    | V_FUNC FuncType
    deriving (Eq, Ord)

instance Eq FuncType where
    (FuncType (FuncDef _ ident1 _ _) _) ==
          (FuncType (FuncDef _ ident2 _ _) _) = ident1 > ident2

instance Ord FuncType where
    (FuncType (FuncDef _ ident1 _ _) _) <=
        (FuncType (FuncDef _ ident2 _ _) _) = ident1 <= ident2

instance Show ValueType where
    show (NULL) = "null"
    show (V_INT i) = show i
    show (V_BOOL b) = show b
    show (V_STRING s) = show s
    show (V_FUNC ((FuncType (FuncDef _ ident _ _) _))) = show ident
