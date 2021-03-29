{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parser
import Codegen
import Emit
import LLVM.Pretty (ppllvm)
import Control.Monad.Trans
import qualified Data.Text.Lazy.IO as T
import System.IO
import System.Environment
import System.Console.Haskeline

import Data.String
import qualified LLVM.AST as AST
import LLVM.Context
import LLVM.Module
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Type  as A.T
import qualified Syntax as S

initModule :: AST.Module
initModule = emptyModule "mycooljit"

process :: AST.Module -> String -> IO (Maybe AST.Module)
process modo source = do
  let res = parseToplevel source
  case res of
    Left err -> print err >> return Nothing
    Right ex -> do
      print ex
      ast <- codegen modo ex
      print ast
      return $ Just ast

processFile :: String -> IO (Maybe AST.Module)
processFile fname = readFile fname >>= process initModule

repl :: IO ()
repl = runInputT defaultSettings (loop initModule)
  where
  loop mod = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> do
        modn <- liftIO $ process mod input
        
        case modn of
          Just modn -> do
                        liftIO $ T.putStrLn $ ppllvm modn
                        loop modn
          Nothing -> loop mod

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> repl
    [fname] -> processFile fname >> return ()

fun =  AST.Module "<string>" "<string>" Nothing Nothing [AST.GlobalDefinition G.functionDefaults {
         G.returnType = A.T.void,
         G.name = AST.UnName 0,
         G.parameters = ([], False),
         G.alignment = 1,
         G.section = Just "foo"
       }]