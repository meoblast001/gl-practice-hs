-- |
-- Module: Programs.Lines
-- Copyright: (C) 2015 Braden Walters
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

module Programs.Lines (display, GLRect.reshape) where

import Graphics.GL.Standard21
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.UI.GLUT.Callbacks.Window
import Graphics.UI.GLUT.Window
import qualified Programs.GLRect as GLRect

-- |Called when the frame will be rendered.
display :: DisplayCallback
display = do
  -- Clear the colour buffer.
  glClear GL_COLOR_BUFFER_BIT
  -- Draw using the recursive drawing function.
  glBegin GL_LINES
  drawLines pi
  glEnd
  -- Render.
  swapBuffers
  where
    -- |Recursive function which draws lines from the origin out 50 unites,
    -- spaced 9 degrees apart, and the corresponding line on the other side (180
    -- degrees) until the angle reaches 0 or lower.
    drawLines angle
      | angle <= 0 = return ()
      | otherwise = do
        -- Top half of the circle.
        let x1 = 50 * sin angle
            y1 = 50 * cos angle
        -- Bottom half of the circle.
            x2 = 50 * sin (angle + pi)
            y2 = 50 * cos (angle + pi)
        glVertex3f x1 y1 0.0
        glVertex3f x2 y2 0.0
        -- Recurse.
        drawLines (angle - (pi / 20))
