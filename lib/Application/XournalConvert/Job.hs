{-# LANGUAGE ScopedTypeVariables #-}

module Application.XournalConvert.Job where

import Data.List 
import Graphics.Xournal.Render.Simple
import Graphics.Rendering.Cairo

import Data.Xournal.Simple
import Text.Xournal.Parse

import Text.StringTemplate

import System.Directory 
import System.FilePath
import System.IO

import Paths_xournal_convert

render1 :: [(String,String)] -> String -> String
render1 attribs tmpl = render . setManyAttrib attribs . newSTMP $ tmpl

svgFileName :: String -> Int -> String 
svgFileName fname_wo_ext pnum =
  fname_wo_ext ++ "_Page_" ++ show pnum <.> "svg"


startJob :: IO () 
startJob = do 
  putStrLn "job started"




startMakeSVG :: FilePath -> Maybe FilePath -> IO () 
startMakeSVG fname mdest = do 
  xojcontent <- read_xournal fname 
  cdir <- getCurrentDirectory 
  let dest = maybe cdir id mdest 

  -- setCurrentDirectory dest 
  let --  (fname_wo_ext,_fname_ext) = splitExtension fname 
      fnamebase = takeBaseName fname 

  let pages = xoj_pages xojcontent
      names = map (svgFileName fnamebase) [1..]
      namePages = zip names pages 
  let Dim w h = page_dim (head pages)
  let svgoutfn x = withSVGSurface (dest </> fst x) w h (\s -> renderWith s (cairoDrawPage (snd x)))

  mapM_ svgoutfn namePages
  makeHtmlJavascriptPage (dest </> "index.html") $ zip [1..] (map fst namePages)
  putStrLn "test ended"

onePageTemplate :: String 
onePageTemplate = "<p><div class=\"page\"> <a name=\"$page$\"> <img src=\"$filename$\" width=100% /> </a> </div> </p>\n\n"

onerule :: String 
onerule = "<hr /> \n"

makeHtmlJavascriptPage :: FilePath -> [(Int,String)] -> IO ()
makeHtmlJavascriptPage fname names = do
  putStrLn $ "writing  " ++ fname
  putStrLn $ show names 
  templateDir <- getDataDir >>= return . (</> "template")
  indexhtmlst <- readFile (templateDir </> "index.html.st")
  let mkstr :: (Int,String) -> String 
      mkstr (p,n) = flip render1 onePageTemplate [ ("filename", "." </> n) 
                                                 , ("page", show p) ]
      bodystr = intercalate onerule . map mkstr $ names
  let str = flip render1 indexhtmlst [ ("body", bodystr) ] 
  withFile fname WriteMode $ \h -> do 
    hPutStr h str
    
