-- |
-- Module: Programs.GLRect
-- Copyright: (C) 2015 Braden Walters
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

module Programs.GLRect (display, reshape) where

import Graphics.GL.Standard21
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.UI.GLUT.Callbacks.Window
import Graphics.UI.GLUT.Window

-- |Called when the frame will be rendered.
display :: DisplayCallback
display = do
  glClear GL_COLOR_BUFFER_BIT
  glColor3f 1.0 0.0 0.0
  glRectf (-25.0) 25.0 25.0 (-25.0)
  swapBuffers

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
