module Application.XournalConvert.Command where

import Application.XournalConvert.Type
import Application.XournalConvert.Job

commandLineProcess :: Xournal_convert -> IO ()
commandLineProcess Test = do 
  putStrLn "test called"
  startJob
