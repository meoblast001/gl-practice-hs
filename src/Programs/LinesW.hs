-- |
-- Module: Programs.LinesW
-- Copyright: (C) 2015 Braden Walters
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

module Programs.LinesW (display, Points.reshape) where

import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
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
  -- Clear the colour buffer.
  glClear GL_COLOR_BUFFER_BIT
  -- Get the smallest and largest line widths.
  (lowSize, highSize) <- getSizes
  -- Draw the lines with increasing width with a recursive function
  drawLinesWithWidths (-90) 90 lowSize highSize
  -- Render.
  swapBuffers
  where
    -- |Recursive function which takes a Y value, a max Y value, a line width,
    -- and a max line width and draws lines with the given Y value and line
    -- width, increasing until the max Y value is reached, while clamping at the
    -- max line width (stops growing when max is reached).
    drawLinesWithWidths y maxY lineWidth maxLineWidth
      | y > maxY = return ()
      | otherwise = do
        -- Set the current line width.
        glLineWidth lineWidth
        -- Draw a line from -80 to 80 (with a screen range from -100 to 100).
        glBegin GL_LINES
        glVertex2f (-80) y
        glVertex2f 80 y
        glEnd
        -- Recurse.
        let clampedLineWidth = if lineWidth + 1 <= maxLineWidth
                               then lineWidth + 1 else lineWidth
        drawLinesWithWidths (y + 20) maxY clampedLineWidth maxLineWidth

-- |Get smallest and largest line widths.
getSizes :: IO (GLfloat, GLfloat)
getSizes = do
  sizes <- allocaArray 2 (\ptr -> glGetFloatv GL_LINE_WIDTH_RANGE ptr >>
                                  peekArray 2 ptr)
  return (sizes !! 0, sizes !! 1)
