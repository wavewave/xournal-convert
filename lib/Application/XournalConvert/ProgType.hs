{-# LANGUAGE DeriveDataTypeable #-}

module Application.XournalConvert.ProgType where 

import System.Console.CmdArgs

data Xournal_convert = Test 
                     | MakeSVG { xojfile :: FilePath
                               }
              deriving (Show,Data,Typeable)

test :: Xournal_convert
test = Test 

makeSVG :: Xournal_convert
makeSVG = MakeSVG { xojfile = "test.xoj" &= typ "FILENAME" &= argPos 0 
                  }


mode = modes [test, makeSVG]

