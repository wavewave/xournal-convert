{-# LANGUAGE DeriveDataTypeable #-}

module Application.XournalConvert.ProgType where 

import System.Console.CmdArgs

data Xournal_convert = Test 
                     | MakeSVG { xojfile :: FilePath
                               , dest :: Maybe FilePath 
                               }
              deriving (Show,Data,Typeable)

test :: Xournal_convert
test = Test 

makeSVG :: Xournal_convert
makeSVG = MakeSVG { xojfile = "test.xoj" &= typ "FILENAME" &= argPos 0 
                  , dest = Nothing &= typ "DESTINATION" 
                  }

mode :: Xournal_convert 
mode = modes [test, makeSVG]

