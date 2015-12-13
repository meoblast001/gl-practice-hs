-- |
-- Module: Programs.Lines
-- Copyright: (C) 2015 Braden Walters
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

module Programs.Lines (display, reshape) where

import Graphics.GL.Standard21
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.UI.GLUT.Callbacks.Window
import Graphics.UI.GLUT.Window

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
    then glOrtho (-100.0) 100.0 ((-100.0) / aspectRatio) (100.00 / aspectRatio)
                 1.0 (-1.0)
    else glOrtho ((-100.0) * aspectRatio) (100.0 * aspectRatio) (-100.0) 100.0
                 1.0 (-1.0)
  glMatrixMode GL_MODELVIEW
  glLoadIdentity
