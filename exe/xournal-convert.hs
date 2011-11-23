module Main where

import System.Console.CmdArgs

import Application.XournalConvert.Type
import Application.XournalConvert.Command

main :: IO () 
main = do 
  putStrLn "xournal-convert"
  param <- cmdArgs mode

  commandLineProcess param