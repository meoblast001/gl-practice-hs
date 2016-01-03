-- |
-- Module: Programs.Triangle
-- Copyright: (C) 2016 Braden Walters
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

module Programs.Triangle (display, Points.reshape, keymouse) where

import Control.Monad
import Data.Bits
import qualified Data.HashSet as HS
import Data.IORef
import Graphics.GL.Standard21
import Graphics.UI.GLUT.Callbacks.Window
import Graphics.UI.GLUT.Window
import qualified Programs.Points as Points

type RotationInfo = (Float, Float)

-- |Called when the frame will be rendered.
display :: HS.HashSet String -> IORef RotationInfo -> DisplayCallback
display longArgs ioRef = do
  -- Set up the display.
  setupDisplay
  -- Clear colour and depth buffer.
  glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
  -- If option "cull" is given, enable culling.
  if "cull" `HS.member` longArgs
    then glEnable GL_CULL_FACE
    else glDisable GL_CULL_FACE
  -- If option "depth" is given, enable depth testing.
  if "depth" `HS.member` longArgs
    then glEnable GL_DEPTH_TEST
    else glDisable GL_DEPTH_TEST
  -- If option "outline" is given, display the back side of polygons in
  -- wireframe.
  if "outline" `HS.member` longArgs
    then glPolygonMode GL_BACK GL_LINE
    else glPolygonMode GL_BACK GL_FILL
  -- Set rotation.
  (xRot, yRot) <- readIORef ioRef
  glPushMatrix
  glRotatef xRot 1.0 0.0 0.0
  glRotatef yRot 0.0 1.0 0.0
  -- Draw cone.
  glBegin GL_TRIANGLE_FAN
  glVertex3f 0.0 0.0 75.0 -- Shared vertex.
  drawRestOfTriangleFans 0.0 (2 * pi + 0.05) 0
  glEnd
  -- Draw bottom of cone.
  glBegin GL_TRIANGLE_FAN
  glVertex2f 0.0 0.0
  drawRestOfTriangleFans 0.0 (2 * pi + 0.05) 0
  glEnd
  -- Pop changes from matrix stack.
  glPopMatrix
  -- Render.
  swapBuffers
  where
    -- |After the shared vertex has been drawn, draw the rest of the triangle
    -- cone.
    drawRestOfTriangleFans angle maxAngle pivot
      | angle > maxAngle = return ()
      | otherwise = do
        let x = 50 * sin angle
            y = 50 * cos angle
        -- Alternate colour of each vertex and therefore each triangle.
        if pivot `mod` 2 == 0
          then glColor3f 0.0 1.0 0.0
          else glColor3f 1.0 0.0 0.0
        glVertex2f x y
        -- Recurse.
        drawRestOfTriangleFans (angle + pi / 8) maxAngle (pivot + 1)

-- |Sets up the display.
setupDisplay :: IO ()
setupDisplay = do
  -- Background colour is black.
  glClearColor 0.0 0.0 0.0 1.0
  -- Drawing colour is green.
  glColor3f 0.0 1.0 0.0
  -- Polygons use the colour of their last specified vertex.
  glShadeModel GL_FLAT
  -- The front side of a polygon is the side for which the vertices are
  -- specified in clockwise order.
  glFrontFace GL_CW

-- |Called when any keyboard or mouse event occurs.
keymouse :: IORef RotationInfo -> KeyboardMouseCallback
keymouse ioRef (SpecialKey specialKey) Down _ _ =
  case specialKey of
    KeyLeft ->
      readIORef ioRef >>= return . mapSnd (+ 5) >>= writeIORef ioRef >>
      postRedisplay Nothing
    KeyUp ->
      readIORef ioRef >>= return . mapFst (+ 5) >>= writeIORef ioRef >>
      postRedisplay Nothing
    KeyRight ->
      readIORef ioRef >>= return . mapSnd (subtract 5) >>= writeIORef ioRef >>
      postRedisplay Nothing
    KeyDown ->
      readIORef ioRef >>= return . mapFst (subtract 5) >>= writeIORef ioRef >>
      postRedisplay Nothing
keymouse _ _ _ _ _ = return ()

-- |Map over the first element of a tuple.
mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)

-- |Map over the second element of a tuple.
mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)
