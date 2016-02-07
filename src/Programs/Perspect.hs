-- |
-- Module: Programs.Atom
-- Copyright: (C) 2016 Braden Walters
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

module Programs.Perspect (display, reshape, timer) where

import Data.Bits
import Data.IORef
import Foreign.Marshal.Array
import Graphics.GL.Standard21
import Graphics.GL.Types
import Graphics.GLU.Functions
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.UI.GLUT.Callbacks.Global
import Graphics.UI.GLUT.Callbacks.Window
import Graphics.UI.GLUT.Objects
import Graphics.UI.GLUT.Window

-- |Called when the frame will be rendered.
display :: IORef (Float, Float) -> DisplayCallback
display ioref = do
  (moonRotation, earthRotation) <- readIORef ioref
  glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
  glEnable GL_DEPTH_TEST
  -- Save the matrix state and perform rotations.
  glMatrixMode GL_MODELVIEW
  glPushMatrix
  -- Translate the entire scene out into view.
  glTranslatef 0.0 0.0 (-300.0)

  -- Sun material colour is yellow.
  glColor3ub 255 255 0
  -- Draw sun without lighting.
  glDisable GL_LIGHTING
  renderObject Solid (Sphere' 15.0 15 15)
  -- Light positioned at sun.
  glEnable GL_LIGHTING
  -- Enable colour tracking to set material properties from colour.
  glEnable GL_COLOR_MATERIAL
  glColorMaterial GL_FRONT GL_AMBIENT_AND_DIFFUSE
  -- Enable and set light 0.
  glEnable GL_LIGHT0
  withArray [0.0, 0.0, 0.0, 1.0] $ glLightfv GL_LIGHT0 GL_POSITION

  -- Rotate to earth rotation.
  glRotatef earthRotation 0.0 1.0 0.0
  -- Draw the earth.
  glColor3ub 0 0 255
  glTranslatef 105.0 0.0 0.0
  renderObject Solid (Sphere' 15.0 15 15)

  -- From earth coordinates, rotate to the moon's position and draw it.
  glColor3ub 200 200 200
  glRotatef moonRotation 0.0 1.0 0.0
  glTranslatef 30.0 0.0 0.0
  renderObject Solid (Sphere' 6.0 15 15)

  -- Restore the matrix state.
  glPopMatrix
  -- Render.
  swapBuffers

-- |Called when the program's timer times out. Move the animation so that the
-- earth and moon move forward.
timer :: IORef (Float, Float) -> TimerCallback
timer ioref = do
  (moonRotation, earthRotation) <- readIORef ioref
  -- Step moon and earth rotation forward.
  let newEarthRotation = if earthRotation > 360
                         then 0.0 else earthRotation + 5.0
      newMoonRotation = if moonRotation > 360 then 0.0 else moonRotation + 15.0
  writeIORef ioref (newEarthRotation, newMoonRotation)
  postRedisplay Nothing

-- |Called when the window is reshaped. Adjust the clipping volume to match
-- the aspect ratio so that the scene does not appear skewed.
reshape :: ReshapeCallback
reshape (Size width 0) = reshape (Size width 1)
reshape (Size width height) = do
  glViewport 0 0 (fromIntegral width) (fromIntegral height)
  glMatrixMode GL_PROJECTION
  glLoadIdentity
  let aspectRatio = fromIntegral width / fromIntegral height
  -- Set a perspective projection using a 45 degree Y field of view, the aspect
  -- ratio, and near and far clipping of 1 and 425 respectively.
  gluPerspective 45.0 aspectRatio 1.0 425.0
  glMatrixMode GL_MODELVIEW
  glLoadIdentity
