-- |
-- Module: Programs.Scissor
-- Copyright: (C) 2016 Braden Walters
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

module Programs.Scissor (display, Points.reshape) where

import Graphics.GL.Standard21
import Graphics.GL.Types
import Graphics.UI.GLUT.Callbacks.Window
import Graphics.UI.GLUT.Window
import qualified Programs.Points as Points

-- |Called when the frame will be rendered.
display :: DisplayCallback
display = do
  -- Clear the screen to the colour blue
  glClearColor 0.0 0.0 1.0 0.0
  glClear GL_COLOR_BUFFER_BIT

  -- Set the scissor to a smaller, red sub-region.
  glClearColor 1.0 0.0 0.0 0.0
  glScissor 100 100 600 400
  glEnable GL_SCISSOR_TEST
  glClear GL_COLOR_BUFFER_BIT

  -- Finally, set the scissor to a smaller green sub-region.
  glClearColor 0.0 1.0 0.0 0.0
  glScissor 200 200 400 200
  glClear GL_COLOR_BUFFER_BIT

  -- Disable scissor testing (default state).
  glDisable GL_SCISSOR_TEST

  -- Render.
  swapBuffers
