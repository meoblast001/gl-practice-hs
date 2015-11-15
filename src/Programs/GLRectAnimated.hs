module Programs.GLRectAnimated (display, reshape, timer) where

import Data.IORef
import Data.StateVar
import Graphics.GL.Standard21
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.UI.GLUT.Callbacks.Global
import Graphics.UI.GLUT.Callbacks.Window
import Graphics.UI.GLUT.Window

display :: IORef (Float, Float, Bool, Bool) -> DisplayCallback
display ioref = do
  (x, y, xstep, ystep) <- readIORef ioref
  glClear GL_COLOR_BUFFER_BIT
  glColor3f 1.0 0.0 0.0
  glRectf x y (x + rSize) (y - rSize)
  swapBuffers

reshape :: ReshapeCallback
reshape (Size width 0) = reshape (Size width 1)
reshape (Size width height) = do
  glViewport 0 0 (fromIntegral width) (fromIntegral height)
  glMatrixMode GL_PROJECTION
  glLoadIdentity
  let aspectRatio = (fromIntegral width) / (fromIntegral height)
  if width <= height
    then glOrtho (-100.0) 100.0 ((-100.0) / aspectRatio) (100.00 / aspectRatio)
                 1.0 (-1.0)
    else glOrtho ((-100.0) * aspectRatio) (100.0 * aspectRatio) (-100.0) 100.0
                 1.0 (-1.0)
  glMatrixMode GL_MODELVIEW
  glLoadIdentity

timer :: IORef (Float, Float, Bool, Bool) -> TimerCallback
timer ioref = do
  (x, y, xstep, ystep) <- readIORef ioref
  (Size width height) <- get windowSize
  let width' = fromIntegral width / 2.0
      height' = fromIntegral height / 2.0
      xstepNew = if x > width' - rSize || x < (-width') then not xstep
                                                        else xstep
      ystepNew = if y > height' || y < (-height') + rSize then not ystep
                                                          else ystep
      xNew = x + (if xstepNew then stepSize else (-stepSize))
      yNew = y + (if ystepNew then stepSize else (-stepSize))
  writeIORef ioref (xNew, yNew, xstepNew, ystepNew)
  postRedisplay Nothing

rSize :: Num a => a
rSize = 50

stepSize :: Num a => a
stepSize = 1
