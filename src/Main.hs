-- |
-- Module: Main
-- Copyright: (C) 2015 Braden Walters
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

module Main where

import Data.Functor
import qualified Data.HashSet as HS
import Data.Maybe
import Data.IORef
import Data.List (stripPrefix)
import Data.StateVar
import Graphics.UI.GLUT.Begin
import Graphics.UI.GLUT.Callbacks
import Graphics.UI.GLUT.Initialization
import Graphics.UI.GLUT.Window
import Programs.Simple as Simple
import Programs.GLRect as GLRect
import Programs.GLRectAnimated as GLRectAnimated
import Programs.Points as Points
import Programs.Pointsz as Pointsz
import Programs.Lines as Lines
import Programs.LStrips as LStrips
import Programs.LinesW as LinesW
import Programs.LStipple as LStipple
import Programs.Triangle as Triangle
import System.Environment

main :: IO ()
main = do
  progName <- getProgName
  allArgs <- getArgs
  -- Intialise the GLUT program and get the callback functions for the current
  -- mode.
  callbacks <- initialize progName allArgs >>= callbackFunctions
  case callbacks of
    (Just displayCallback', reshapeCallback', timerData,
     keyboardMouseCallback') -> do
      initialDisplayMode $= [DoubleBuffered, RGBAMode]
      window <- createWindow "GL Practice in Haskell"
      displayCallback $= displayCallback'
      reshapeCallback $= reshapeCallback'
      case timerData of
        Just (timeout, timerCallback) -> addRepeatingTimer timeout timerCallback
        Nothing -> return ()
      keyboardMouseCallback $= keyboardMouseCallback'
      mainLoop
    -- If no display callback, end the program.
    _ -> error "Please provide function."

-- |Given a list of program arguments, find the name of the program to be run,
-- and return its display, reshape, timer, and keyboard callbacks, if they
-- exist. If the display callback is Nothing, then a program was not found.
callbackFunctions :: [String] ->
                     IO (Maybe DisplayCallback, Maybe ReshapeCallback,
                         Maybe (Timeout, TimerCallback),
                         Maybe KeyboardMouseCallback)
callbackFunctions ("simple":xs) =
  return (Just Simple.display, Nothing, Nothing, Nothing)
callbackFunctions ("glrect":xs) =
  return (Just GLRect.display, Just GLRect.reshape, Nothing, Nothing)
callbackFunctions ("glrectanimated":xs) = do
  ref <- newIORef (0.0, 0.0, True, True)
  return (Just (GLRectAnimated.display ref), Just GLRectAnimated.reshape,
          Just (33, GLRectAnimated.timer ref), Nothing)
callbackFunctions ("points":xs) =
  return (Just Points.display, Just Points.reshape, Nothing, Nothing)
callbackFunctions ("pointsz":xs) =
  return (Just Pointsz.display, Just Pointsz.reshape, Nothing, Nothing)
callbackFunctions ("lines":xs) =
  return (Just Lines.display, Just Lines.reshape, Nothing, Nothing)
callbackFunctions ("lstrips":xs) =
  return (Just LStrips.display, Just LStrips.reshape, Nothing, Nothing)
callbackFunctions ("linesw":xs) =
  return (Just LinesW.display, Just LinesW.reshape, Nothing, Nothing)
callbackFunctions ("lstipple":xs) =
  return (Just LStipple.display, Just LStipple.reshape, Nothing, Nothing)
callbackFunctions ("triangle":xs) = do
  longArgs <- longArgSet
  ref <- newIORef (0.0, 0.0)
  return (Just $ Triangle.display longArgs ref, Just Triangle.reshape, Nothing,
          Just $ Triangle.keymouse ref)
callbackFunctions (_:xs) = callbackFunctions xs
callbackFunctions [] = return (Nothing, Nothing, Nothing, Nothing)

-- |Get all long arguments (arguments starting with "--").
longArgSet :: IO (HS.HashSet String)
longArgSet =
  getArgs >>= return . HS.fromList . catMaybes . map (stripPrefix "--")

-- |Add a GLUT timer which will set itself again after it is triggered.
addRepeatingTimer :: Timeout -> TimerCallback -> IO ()
addRepeatingTimer timeout callback =
  callback >> addTimerCallback timeout (addRepeatingTimer timeout callback)
