module Main where

import Data.StateVar
import Graphics.GL.Core32
import Graphics.UI.GLUT.Begin
import Graphics.UI.GLUT.Callbacks
import Graphics.UI.GLUT.Initialization
import Graphics.UI.GLUT.Window
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
      mainLoop
    Nothing -> putStrLn "Please provide function."

displayFunction :: [String] -> Maybe (IO ())
displayFunction ("simple":xs) = Just displaySimple
displayFunction (_:xs) = displayFunction xs
displayFunction [] = Nothing

displaySimple :: IO ()
displaySimple = do
  glClear GL_COLOR_BUFFER_BIT
  glFlush
