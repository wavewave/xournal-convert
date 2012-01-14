{-# LANGUAGE ScopedTypeVariables #-}

module Application.XournalConvert.Job where

import System.Directory 
import Data.Xournal.Simple
import Text.Xournal.Parse
import Application.XournalConvert.Convert.MakeSVG


-- startJob :: IO () 
-- startJob = do 
--   putStrLn "job started"


startMakeSVG :: FilePath -> Maybe FilePath -> IO () 
startMakeSVG fname mdest = do 
  xojcontent <- checkIfBinary fname >>= \b -> 
                  ifThenElse b (read_xojgz fname) (read_xournal fname)
  cdir <- getCurrentDirectory 
  let dest = maybe cdir id mdest 
  makeSVGFromXournal xojcontent fname dest 
  return ()

