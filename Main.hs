module Main where


import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error

import AbsMocha
import LexMocha
import ParMocha
import Interpreter
import PrintMocha
import Types
import ErrM

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = if v > 1 then putStrLn s else return ()

runFile :: Verbosity -> ParseFun Program -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: Verbosity -> ParseFun Program -> String -> IO ()
run v p s = let ts = myLLexer s in case p ts of
           Bad s    -> do putStrLn "\nParse              Failed...\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          putStrLn s
                          exitFailure
           Ok  prog -> do runProgram prog
                          exitSuccess

runProgram :: Program -> IO ()
runProgram program = do
    let startState = Map.singleton (LOC 0) (V_INT 1)
    let startEnv = Env { vEnv = Map.empty, fEnv = Map.empty, outerEnv = None }
    eval <- runErrorT ( execStateT ( runReaderT ( transProgram program ) startEnv ) startState )
    case eval of
        Left err -> putStrLn $ "Error: " ++ show err
        Right ok -> return ()

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree
 = do
      putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
      file -> mapM_ (runFile 2 pProgram) file
      _ -> putStrLn "Failed to run the interpreter!"
