module Application.XournalConvert.Command where

import Application.XournalConvert.ProgType
import Application.XournalConvert.Job

commandLineProcess :: Xournal_convert -> IO ()
commandLineProcess Test = do 
  putStrLn "test called"
  startJob
