-- |
-- Module: Programs.GLRectAnimated
-- Copyright: (C) 2015 Braden Walters
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

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

-- |Info about position of the rectangle. First two elements are the X and Y
-- position of the rectangle respectively. The next two elements are true if the
-- X and Y positions are climbing (+), or false if they are falling (-).
type PositionInfo = (Float, Float, Bool, Bool)

-- |Floating point version of Graphics.Rendering.OpenGL.GL.CoordTrans.Size
data SizeF = SizeF Float Float

-- |Called when the frame will be rendered.
display :: IORef PositionInfo -> DisplayCallback
display ioref = do
  (x, y, xstep, ystep) <- readIORef ioref
  glClear GL_COLOR_BUFFER_BIT
  glColor3f 1.0 0.0 0.0
  glRectf x y (x + rSize) (y - rSize)
  swapBuffers

-- |Called when the window is reshaped. Adjust the clipping volume to match
-- the aspect ratio so that the scene does not appear skewed.
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

-- |Called when the program's timer times out. Move the animation so that the
-- position of the rectangle will be different at the next render.
timer :: IORef PositionInfo -> TimerCallback
timer ioref = do
  newPositionInfo <- readIORef ioref >>= progressAnimation >>= checkBounds
  writeIORef ioref newPositionInfo
  postRedisplay Nothing

-- |Progresses the animation. If the rectangle reaches the edge of the screen,
-- change it's direction on that axis.
progressAnimation :: PositionInfo -> IO PositionInfo
progressAnimation (x, y, xstep, ystep) = do
  (SizeF width height) <- glSize
  let xstepNew = if x > width - rSize || x < (-width) then not xstep
                                                      else xstep
      ystepNew = if y > height || y < (-height) + rSize then not ystep
                                                        else ystep
      xNew = x + stepOffset xstepNew
      yNew = y + stepOffset ystepNew
  return (xNew, yNew, xstepNew, ystepNew)

-- |Checks that the rectangle does not fall outside of the screen. This can
-- happen when the window size is reduced. If this is the case, places the
-- rectangle back in the screen.
checkBounds :: PositionInfo -> IO PositionInfo
checkBounds (x, y, xstep, ystep) = do
  (SizeF width height) <- glSize
  let xNew
        | x > width - rSize + stepOffset xstep = width - rSize - 1
        | x < -(width + stepOffset xstep) = (-width) - 1
        | otherwise = x
      yNew
        | y > height + stepOffset ystep = height - 1
        | y < -(height - rSize + stepOffset ystep) = (-height) + rSize - 1
        | otherwise = y
  return (xNew, yNew, xstep, ystep)

-- |Gets the size of the visible viewport in GL coordinates.
glSize :: IO SizeF
glSize = do
  (SizeF width height) <- sizeToFloat <$> get windowSize
  let aspectRatio = width / height
  if width <= height then return $ SizeF 100 (100 / aspectRatio)
                     else return $ SizeF (100 * aspectRatio) 100

-- |Size of the rectangle in both dimensions.
rSize :: GLfloat
rSize = 50

-- |Amount of units that the character moves per frame.
stepSize :: GLfloat
stepSize = 1

-- |Given the step direction of an axis, return the amount of units that should
-- be added to the current position on that axis.
stepOffset :: Bool -> GLfloat
stepOffset step = if step then stepSize else (-stepSize)

-- |Given a Size structure, return a similar structure containing floats.
sizeToFloat :: Size -> SizeF
sizeToFloat (Size width height) =
  SizeF (fromIntegral width) (fromIntegral height)
