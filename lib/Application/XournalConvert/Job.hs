{-# LANGUAGE ScopedTypeVariables #-}

module Application.XournalConvert.Job where

import System.Directory 
import Data.Xournal.Simple
-- import Text.Xournal.Parse
import Text.Xournal.Parse.Enumerator 
import Application.XournalConvert.Convert.MakeSVG

-- | 

startMakeSVG :: FilePath -> Maybe FilePath -> IO () 
startMakeSVG fname mdest = do 
  exojcontent <- parseXournal fname 
  case exojcontent of 
    Left err -> putStrLn $ "parsing error : " ++ err 
    Right xojcontent -> do 
      cdir <- getCurrentDirectory 
      let dest = maybe cdir id mdest 
      makeSVGFromXournal xojcontent fname dest 
      return ()


  -- xojcontent <- checkIfBinary fname >>= \b -> 
  --                 ifThenElse b (read_xojgz fname) (read_xournal fname)

