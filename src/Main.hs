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
import System.Environment

main :: IO ()
main = do
  progName <- getProgName
  allArgs <- getArgs
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
    _ -> putStrLn "Please provide function."

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
callbackFunctions (_:xs) = callbackFunctions xs
callbackFunctions [] = return (Nothing, Nothing, Nothing)

addRepeatingTimer :: Timeout -> TimerCallback -> IO ()
addRepeatingTimer timeout callback =
  callback >> addTimerCallback timeout (addRepeatingTimer timeout callback)
