-- |
-- Module: Programs.LStrips
-- Copyright: (C) 2015 Braden Walters
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

module Programs.LStrips (display, Points.reshape) where

import Graphics.GL.Standard21
import Graphics.GL.Types
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.UI.GLUT.Callbacks.Global
import Graphics.UI.GLUT.Callbacks.Window
import Graphics.UI.GLUT.Window
import qualified Programs.Points as Points

-- |Called when the frame will be rendered.
display :: DisplayCallback
display = do
  -- Clear the colour buffer and set the colour.
  glClear GL_COLOR_BUFFER_BIT
  glColor3f 0.0 1.0 0.0
  -- Set the rotation (75 degrees X) on a new layer in the matrix stack.
  glPushMatrix
  glRotatef 75.0 1.0 0.0 0.0
  -- Draw the line strip using a recursive function.
  glBegin GL_LINE_STRIP
  drawLineStrip (6 * pi) (-50)
  glEnd
  -- Pop rotation from matrix stack and render.
  glPopMatrix
  swapBuffers
  where
    drawLineStrip angle z
      | angle <= 0 = return ()
      | otherwise = do
        let x = 50 * sin angle
            y = 50 * cos angle
        glVertex3f x y z
        drawLineStrip (angle - 0.1) (z + 0.5)
