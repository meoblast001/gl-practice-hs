module Programs.GLRectAnimated (display, reshape, timer) where

import Data.Functor
import Data.IORef
import Data.StateVar
import Graphics.GL.Standard21
import Graphics.GL.Types
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.UI.GLUT.Callbacks.Global
import Graphics.UI.GLUT.Callbacks.Window
import Graphics.UI.GLUT.Window

type PositionInfo = (Float, Float, Bool, Bool)

data SizeF = SizeF Float Float

display :: IORef PositionInfo -> DisplayCallback
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
    then glOrtho (-100) 100 ((-100) / aspectRatio) (100 / aspectRatio)
                 1.0 (-1.0)
    else glOrtho ((-100) * aspectRatio) (100 * aspectRatio) (-100) 100
                 1.0 (-1.0)
  glMatrixMode GL_MODELVIEW
  glLoadIdentity

timer :: IORef PositionInfo -> TimerCallback
timer ioref = do
  newPositionInfo <- readIORef ioref >>= checkChangeDirection
  writeIORef ioref newPositionInfo
  postRedisplay Nothing

checkChangeDirection :: PositionInfo -> IO PositionInfo
checkChangeDirection (x, y, xstep, ystep) = do
  (SizeF width height) <- glSize
  let xstepNew = if x > width - rSize || x < (-width) then not xstep
                                                      else xstep
      ystepNew = if y > height || y < (-height) + rSize then not ystep
                                                        else ystep
      xNew = x + stepOffset xstepNew
      yNew = y + stepOffset ystepNew
  return (xNew, yNew, xstepNew, ystepNew)

glSize :: IO SizeF
glSize = do
  (SizeF width height) <- sizeToFloat <$> get windowSize
  let aspectRatio = width / height
  if width <= height then return $ SizeF 100 (100 / aspectRatio)
                     else return $ SizeF (100 * aspectRatio) 100

rSize :: GLfloat
rSize = 50

stepSize :: GLfloat
stepSize = 1

stepOffset :: Bool -> GLfloat
stepOffset step = if step then stepSize else (-stepSize)

sizeToFloat :: Size -> SizeF
sizeToFloat (Size width height) =
  SizeF (fromIntegral width) (fromIntegral height)
