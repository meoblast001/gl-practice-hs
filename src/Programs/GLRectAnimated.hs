module Programs.GLRectAnimated (display, reshape, timer) where

import Data.IORef
import Data.StateVar
import Graphics.GL.Standard21
import Graphics.GL.Types
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.UI.GLUT.Callbacks.Global
import Graphics.UI.GLUT.Callbacks.Window
import Graphics.UI.GLUT.Window

type PositionInfo = (Float, Float, Bool, Bool)

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
      (Size glWidth glHeight) = fixedGlSize
      glWidth' = fromIntegral glWidth
      glHeight' = fromIntegral glHeight
  if width <= height
    then glOrtho (-glWidth') glWidth' ((-glHeight') / aspectRatio)
                 (glHeight' / aspectRatio) 1.0 (-1.0)
    else glOrtho ((-glWidth') * aspectRatio) (glWidth' * aspectRatio)
                 (-glHeight') glHeight' 1.0 (-1.0)
  glMatrixMode GL_MODELVIEW
  glLoadIdentity

timer :: IORef PositionInfo -> TimerCallback
timer ioref = do
  positionInfo <- readIORef ioref
  size <- get windowSize
  writeIORef ioref (checkChangeDirection positionInfo)
  postRedisplay Nothing

checkChangeDirection :: PositionInfo -> PositionInfo
checkChangeDirection (x, y, xstep, ystep) =
  let (Size width height) = fixedGlSize
      width' = fromIntegral width
      height' = fromIntegral height
      xstepNew = if x > width' - rSize || x < (-width') then not xstep
                                                        else xstep
      ystepNew = if y > height' || y < (-height') + rSize then not ystep
                                                          else ystep
      xNew = x + stepOffset xstepNew
      yNew = y + stepOffset ystepNew
  in (xNew, yNew, xstepNew, ystepNew)

fixedGlSize :: Size
fixedGlSize = Size 100 100

rSize :: GLfloat
rSize = 50

stepSize :: GLfloat
stepSize = 1

stepOffset :: Bool -> GLfloat
stepOffset step = if step then stepSize else (-stepSize)
