module Programs.Simple (display) where

import Graphics.GL.Standard21
import Graphics.UI.GLUT.Callbacks.Window
import Graphics.UI.GLUT.Window

display :: DisplayCallback
display = do
  glClear GL_COLOR_BUFFER_BIT
  swapBuffers
