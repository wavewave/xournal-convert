{-# LANGUAGE DeriveDataTypeable #-}

module Application.XournalConvert.Type where 

import System.Console.CmdArgs

data Xournal_convert = Test 
              deriving (Show,Data,Typeable)

test :: Xournal_convert
test = Test 

mode = modes [test]

