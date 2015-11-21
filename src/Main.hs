-- |
-- Module: Main
-- Copyright: (C) 2015 Braden Walters
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

module Main where

import Data.IORef
import Data.StateVar
import Graphics.UI.GLUT.Begin
import Graphics.UI.GLUT.Callbacks
import Graphics.UI.GLUT.Initialization
import Graphics.UI.GLUT.Window
import Programs.Simple as Simple
import Programs.GLRect as GLRect
import Programs.GLRectAnimated as GLRectAnimated
import Programs.Points as Points
import System.Environment

main :: IO ()
main = do
  progName <- getProgName
  allArgs <- getArgs
  -- Intialise the GLUT program and get the callback functions for the current
  -- mode.
  callbacks <- initialize progName allArgs >>= callbackFunctions
  case callbacks of
    (Just displayCallback', reshapeCallback', timerData) -> do
      initialDisplayMode $= [DoubleBuffered, RGBAMode]
      window <- createWindow "GL Practice in Haskell"
      displayCallback $= displayCallback'
      reshapeCallback $= reshapeCallback'
      case timerData of
        Just (timeout, timerCallback) -> addRepeatingTimer timeout timerCallback
        Nothing -> return ()
      mainLoop
    -- If no display callback, end the program.
    _ -> error "Please provide function."

-- |Given a list of program arguments, find the name of the program to be run,
-- and return its display, reshape, and timer callbacks, if they exist. If the
-- display callback is Nothing, then a program was not found.
callbackFunctions :: [String] ->
                     IO (Maybe DisplayCallback, Maybe ReshapeCallback,
                         Maybe (Timeout, TimerCallback))
callbackFunctions ("simple":xs) = return (Just Simple.display, Nothing, Nothing)
callbackFunctions ("glrect":xs) =
  return (Just GLRect.display, Just GLRect.reshape, Nothing)
callbackFunctions ("glrectanimated":xs) = do
  ref <- newIORef (0.0, 0.0, True, True)
  return (Just (GLRectAnimated.display ref), Just GLRectAnimated.reshape,
          Just (33, GLRectAnimated.timer ref))
callbackFunctions ("points":xs) =
  return (Just Points.display, Just Points.reshape, Nothing)
callbackFunctions (_:xs) = callbackFunctions xs
callbackFunctions [] = return (Nothing, Nothing, Nothing)

-- |Add a GLUT timer which will set itself again after it is triggered.
addRepeatingTimer :: Timeout -> TimerCallback -> IO ()
addRepeatingTimer timeout callback =
  callback >> addTimerCallback timeout (addRepeatingTimer timeout callback)
