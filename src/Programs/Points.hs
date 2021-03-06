-- |
-- Module: Programs.Points
-- Copyright: (C) 2015 Braden Walters
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

module Programs.Points (display, reshape) where

import Data.IORef
import Graphics.GL.Standard21
import Graphics.GL.Types
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.UI.GLUT.Callbacks.Global
import Graphics.UI.GLUT.Callbacks.Window
import Graphics.UI.GLUT.Window

-- |Called when the frame will be rendered.
display :: DisplayCallback
display = do
  -- Clear the colour buffer and set the colour.
  glClear GL_COLOR_BUFFER_BIT
  glColor3f 0.0 1.0 0.0
  -- Set the rotation (45 degrees X and Y) on a new layer in the matrix stack.
  glPushMatrix
  glRotatef 45.0 1.0 0.0 0.0
  glRotatef 45.0 0.0 1.0 0.0
  -- Draw all of the points using a recursive function.
  glBegin GL_POINTS
  drawPoints (6 * pi) (-50)
  glEnd
  -- Pop rotation from matrix stack and render.
  glPopMatrix
  swapBuffers
  where
    -- |Recursive function which draws points in a spiral until the angle
    -- reaches zero. Angle is reduced and Z value is increased at each call.
    drawPoints angle z
      | angle <= 0 = return ()
      | otherwise = do
        let x = 50 * sin angle
            y = 50 * cos angle
        glVertex3f x y z
        drawPoints (angle - 0.1) (z + 0.5)

-- |Called when the window is reshaped. Adjust the clipping volume to match
-- the aspect ratio so that the scene does not appear skewed.
reshape :: ReshapeCallback
reshape (Size width 0) = reshape (Size width 1)
reshape (Size width height) = do
  glViewport 0 0 (fromIntegral width) (fromIntegral height)
  glMatrixMode GL_PROJECTION
  glLoadIdentity
  let aspectRatio = fromIntegral width / fromIntegral height
  if width <= height
    then glOrtho (-nRange) nRange ((-nRange) / aspectRatio)
                 (nRange / aspectRatio) (-nRange) nRange
    else glOrtho ((-nRange) * aspectRatio) (nRange * aspectRatio) (-nRange)
                 nRange nRange (-nRange)
  glMatrixMode GL_MODELVIEW
  glLoadIdentity

-- |Range of camera in each direction.
nRange :: GLdouble
nRange = 100
