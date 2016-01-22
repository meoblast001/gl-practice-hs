-- |
-- Module: Programs.Atom
-- Copyright: (C) 2016 Braden Walters
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

module Programs.Atom (display, reshape, timer) where

import Data.Bits
import Data.IORef
import Graphics.GL.Standard21
import Graphics.GL.Types
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.UI.GLUT.Callbacks.Global
import Graphics.UI.GLUT.Callbacks.Window
import Graphics.UI.GLUT.Objects
import Graphics.UI.GLUT.Window
import qualified Programs.Points as Points

-- |Called when the frame will be rendered.
display :: IORef Float -> DisplayCallback
display ioref = do
  -- Need depth testing.
  glEnable GL_DEPTH_TEST
  -- Clear the colour and depth buffers.
  glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
  -- Switch to the modelview matrix and reset it.
  glMatrixMode GL_MODELVIEW
  glLoadIdentity
  -- Translate the scene into view.
  glTranslatef 0.0 0.0 (-100.0)

  -- Red nucleus.
  glColor3ub 255 0 0
  renderObject Solid (Sphere' 10.0 15 15)

  -- Yellow electrons.
  glColor3ub 255 255 0

  -- Get base rotation of electrons.
  firstElectronRot <- readIORef ioref

  -- First electron.
  glPushMatrix
  glRotatef firstElectronRot 0.0 1.0 0.0
  glTranslatef 90.0 0.0 0.0
  renderObject Solid (Sphere' 6.0 15 15)
  glPopMatrix

  -- Second electron.
  glPushMatrix
  glRotatef 45.0 0.0 0.0 1.0
  glRotatef firstElectronRot 0.0 1.0 0.0
  glTranslatef (-70.0) 0.0 0.0
  renderObject Solid (Sphere' 6.0 15 15)
  glPopMatrix

  -- Third electron.
  glPushMatrix
  glRotatef (-45.0) 0.0 0.0 1.0
  glRotatef firstElectronRot 0.0 1.0 0.0
  glTranslatef 0.0 0.0 60.0
  renderObject Solid (Sphere' 6.0 15 15)
  glPopMatrix

  -- Render.
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
    then glOrtho (-nRange) nRange ((-nRange) / aspectRatio)
                 (nRange / aspectRatio) (-nRange) nRange
    else glOrtho ((-nRange) * aspectRatio) (nRange * aspectRatio) (-nRange)
                 nRange nRange (-nRange)
  glMatrixMode GL_MODELVIEW
  glLoadIdentity

-- |Range of camera in each direction.
nRange :: GLdouble
nRange = 200

-- |Called when the program's timer times out. Move the animation so that the
-- electrons move forward.
timer :: IORef Float -> TimerCallback
timer ioref = do
  rotation <- readIORef ioref
  let unclampedNewRotation = rotation + 10.0
      newRotation = if unclampedNewRotation > 360
                    then 0.0 else unclampedNewRotation
  writeIORef ioref newRotation
  postRedisplay Nothing
