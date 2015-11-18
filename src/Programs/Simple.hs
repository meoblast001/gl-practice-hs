-- |
-- Module: Programs.Simple
-- Copyright: (C) 2015 Braden Walters
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

module Programs.Simple (display) where

import Graphics.GL.Standard21
import Graphics.UI.GLUT.Callbacks.Window
import Graphics.UI.GLUT.Window

-- |Called when the frame will be rendered.
display :: DisplayCallback
display = do
  glClear GL_COLOR_BUFFER_BIT
  swapBuffers
