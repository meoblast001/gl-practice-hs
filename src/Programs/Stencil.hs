-- |
-- Module: Programs.Stencil
-- Copyright: (C) 2016 Braden Walters
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

module Programs.Stencil
( display
, GLRectAnimated.reshape
, GLRectAnimated.timer
) where

import Data.Bits
import Data.IORef
import Graphics.GL.Standard21
import Graphics.GL.Types
import Graphics.UI.GLUT.Callbacks.Window
import Graphics.UI.GLUT.Window
import qualified Programs.GLRectAnimated as GLRectAnimated

-- |Called when the frame will be rendered.
display :: IORef GLRectAnimated.PositionInfo -> DisplayCallback
display ioref = do
  -- Window colour is blue.
  glClearColor 0.0 0.0 1.0 0.0
  -- Enable stencil testing with a clear value of 0.
  glClearStencil 0
  glEnable GL_STENCIL_TEST
  -- Clear buffers.
  glClear (GL_COLOR_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT)

  -- First, draw a spiral which fails the stencil test, but increments the value
  -- in the stencil buffer.
  glStencilFunc GL_NEVER 0x0 0x0
  glStencilOp GL_INCR GL_INCR GL_INCR
  glBegin GL_LINE_STRIP
  drawSpiral 0 200 0.1
  glEnd

  -- Now draw a red bouncing square which passes the stencil test everywhere
  -- where the value is not 0x1 (where the spiral is drawn).
  glStencilFunc GL_NOTEQUAL 0x1 0x1
  glStencilOp GL_KEEP GL_KEEP GL_KEEP
  (x, y, xstep, ystep) <- readIORef ioref
  glRectf x y (x + rSize) (y - rSize)

  -- Render.
  swapBuffers
  where
    -- |Draw a spiral from angle to maxAngle.
    drawSpiral angle maxAngle radius
      | angle >= maxAngle = return ()
      | otherwise = do
        glVertex2f (radius * cos angle) (radius * sin angle)
        drawSpiral (angle + 0.1) maxAngle (radius + 0.2)

-- |Size of the rectangle in both dimensions.
rSize :: GLfloat
rSize = 50
