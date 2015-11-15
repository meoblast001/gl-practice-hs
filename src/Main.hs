module Main where

import Data.StateVar
import Graphics.UI.GLUT.Begin
import Graphics.UI.GLUT.Callbacks
import Graphics.UI.GLUT.Initialization
import Graphics.UI.GLUT.Window
import Programs.Simple as Simple
import Programs.GLRect as GLRect
import System.Environment

main :: IO ()
main = do
  progName <- getProgName
  allArgs <- getArgs
  otherArgs <- initialize progName allArgs
  case displayFunction otherArgs of
    Just displayFunction' -> do
      initialDisplayMode $= [SingleBuffered, RGBAMode]
      window <- createWindow "GL Practice in Haskell"
      displayCallback $= displayFunction'
      reshapeCallback $= reshapeFunction otherArgs
      mainLoop
    Nothing -> putStrLn "Please provide function."

displayFunction :: [String] -> Maybe (DisplayCallback)
displayFunction ("simple":xs) = Just Simple.display
displayFunction ("glrect":xs) = Just GLRect.display
displayFunction (_:xs) = displayFunction xs
displayFunction [] = Nothing

reshapeFunction :: [String] -> Maybe (ReshapeCallback)
reshapeFunction ("simple":xs) = Nothing
reshapeFunction ("glrect":xs) = Just GLRect.reshape
reshapeFunction (_:xs) = reshapeFunction xs
reshapeFunction [] = Nothing
