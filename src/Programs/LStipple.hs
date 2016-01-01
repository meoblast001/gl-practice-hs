-- |
-- Module: Programs.LStipple
-- Copyright: (C) 2016 Braden Walters
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

module Programs.LStipple (display, Points.reshape) where

import Graphics.GL.Standard21
import Graphics.GL.Types
import Graphics.UI.GLUT.Callbacks.Window
import Graphics.UI.GLUT.Window
import qualified Programs.Points as Points

-- |Called when the frame will be rendered.
display :: DisplayCallback
display = do
  -- Clear the colour buffer.
  glClear GL_COLOR_BUFFER_BIT
  -- Enable line stipples.
  glEnable GL_LINE_STIPPLE
  -- Recursively render lines.
  drawLines (-90.0) 90.0 1
  -- Render.
  swapBuffers
  where
    -- |Recursively draw lines, increasing on the Y axis and increasing the line
    -- stipple factor for each line.
    drawLines y maxY factor
      | y > maxY = return ()
      | otherwise = do
        -- Set line stipple with the given factor (pixels per stipple bit) and
        -- the pattern.
        glLineStipple factor pattern
        -- Draw a line.
        glBegin GL_LINES
        glVertex2f (-80.0) y
        glVertex2f 80.0 y
        glEnd
        -- Recurse.
        drawLines (y + 20) maxY (factor + 1)

-- |Pattern to use with the line strip (0101010101010101).
pattern :: GLushort
pattern = 0x5555
