-- |
-- Module: Programs.Pointsz
-- Copyright: (C) 2015 Braden Walters
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

module Programs.Pointsz (display, Points.reshape) where

import Data.IORef
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
  -- Clear the colour buffer and set the colour.
  glClear GL_COLOR_BUFFER_BIT
  glColor3f 0.0 1.0 0.0
  -- Get the lowest point size, highest point size, and granularity (step size)
  -- of point sizes.
  (lowSize, highSize, step) <- getSizesAndStep
  -- Set the rotation (45 degrees X and Y) on a new layer in the matrix stack.
  glPushMatrix
  glRotatef 45.0 1.0 0.0 0.0
  glRotatef 45.0 0.0 1.0 0.0
  -- Draw all of the points using a recursive function.
  drawPoints (6 * pi) (-50) lowSize highSize step
  -- Pop rotation from matrix stack and render.
  glPopMatrix
  swapBuffers
  where
    -- |Recursive function which draws points in a spiral until the angle
    -- reaches zero. Angle is reduced, Z value is increased, and point size is
    -- increased at each call. Point size is no longer increased when the
    -- maximum size is reached.
    drawPoints angle z pointSize maxPointSize granularity
      | angle <= 0 = return ()
      | otherwise = do
        let x = 50 * sin angle
            y = 50 * cos angle
        glPointSize pointSize
        glBegin GL_POINTS
        glVertex3f x y z
        glEnd
        let nextPointSize = if pointSize + granularity > maxPointSize
                            then pointSize else pointSize + granularity
        -- Recurse.
        drawPoints (angle - 0.1) (z + 0.5) nextPointSize maxPointSize
                   granularity

-- |Get the lowest point size, the highest point size, and the granularity.
getSizesAndStep :: IO (GLfloat, GLfloat, GLfloat)
getSizesAndStep = do
  sizes <- allocaArray 2 (\ptr -> glGetFloatv GL_POINT_SIZE_RANGE ptr >>
                                  peekArray 2 ptr)
  step <- alloca (\ptr -> glGetFloatv GL_POINT_SIZE_GRANULARITY ptr >> peek ptr)
  return (sizes !! 0, sizes !! 1, step)
