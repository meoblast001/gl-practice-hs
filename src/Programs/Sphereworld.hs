-- |
-- Module: Programs.Sphereworld
-- Copyright: (C) 2016 Braden Walters
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

module Programs.Sphereworld (setup, display, Perspect.reshape, keymouse) where

import Data.Bits
import Data.IORef
import Control.Lens
import Foreign.Marshal.Array
import Foreign.Ptr
import Graphics.GL.Standard21
import Graphics.GL.Types
import Graphics.UI.GLUT.Callbacks.Global
import Graphics.UI.GLUT.Callbacks.Window
import Graphics.UI.GLUT.Objects
import Graphics.UI.GLUT.Window
import Linear.Matrix
import Linear.Quaternion
import Linear.Vector
import Linear.V3
import Linear.V4
import qualified Programs.Perspect as Perspect
import System.Random

-- |Set up data necessary to run program.
setup :: IO [(Float, Float)]
setup = do
  -- Blue background.
  glClearColor 0.0 0.0 0.5 1.0
  -- Everything is wireframe.
  glPolygonMode GL_FRONT_AND_BACK GL_LINE
  -- Randomly generate sphere positions.
  randGenX <- newStdGen
  let randomX = randomRs (-20, 20) randGenX
  randGenY <- newStdGen
  let randomZ = randomRs (-20, 20) randGenY
  return $ take numSpheres $ zip randomX randomZ

-- |Called when the frame will be rendered.
display :: IORef (M44 GLfloat) -> [(Float, Float)] -> DisplayCallback
display ioref spherePos = do
  -- Background colour somewhat blue.
  glClearColor 0.0 0.0 0.5 1.0
  -- Draw everything as wireframe.
  glPolygonMode GL_FRONT_AND_BACK GL_LINE

  glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
  glPushMatrix

  -- Apply the camera transformation.
  invertedMatrix <- inv44 <$> readIORef ioref
  withM44AsPtr invertedMatrix $ \inversePtr ->
    glLoadMatrixf inversePtr

  -- Draw the ground and spheres.
  drawGround
  drawSpheres spherePos

  glPopMatrix
  -- Render.
  swapBuffers

-- |Draw the ground as a grid.
drawGround :: DisplayCallback
drawGround = do
  glBegin GL_LINES
  doDrawGround (-20.0) 20.0 1.0 (-0.4)
  glEnd
  where
    doDrawGround currentLine extent step y
      | currentLine <= extent = do
        -- Line along Z axis.
        glVertex3f currentLine y extent
        glVertex3f currentLine y (-extent)
        -- Line along X axis.
        glVertex3f extent y currentLine
        glVertex3f (-extent) y currentLine
        -- Recurse.
        doDrawGround (currentLine + step) extent step y
      | otherwise = return ()

-- |Draw spheres at locations given in the parameter.
drawSpheres :: [(Float, Float)] -> DisplayCallback
drawSpheres [] = return ()
drawSpheres ((x, y):xs) = do
  glPushMatrix
  glTranslatef x 0.0 y
  renderObject Solid $ Sphere' 0.1 13 26
  glPopMatrix
  drawSpheres xs

-- |Temporarily create a pointer of GLfloats from a matrix.
withM44AsPtr :: M44 GLfloat -> (Ptr GLfloat -> IO a) -> IO a
withM44AsPtr (V4 (V4 c1 c5 c9 c13)
                 (V4 c2 c6 c10 c14)
                 (V4 c3 c7 c11 c15)
                 (V4 c4 c8 c12 c16)) =
  withArray [c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15,
             c16]

-- |Called when any keyboard or mouse event occurs.
keymouse :: IORef (M44 GLfloat) -> KeyboardMouseCallback
keymouse ioref (SpecialKey specialKey) Down _ _ =
  case specialKey of
    KeyLeft -> do
      transformation <- readIORef ioref
      writeIORef ioref (transformation !*! rotateY 0.1)
      postRedisplay Nothing
    KeyUp -> do
      transformation <- readIORef ioref
      let localVector = V3 0.0 0.0 (-1.0)
          worldVector = (transformation ^. _m33) !* localVector
      writeIORef ioref $ over translation (^+^ worldVector) transformation
      postRedisplay Nothing
    KeyRight -> do
      transformation <- readIORef ioref
      writeIORef ioref (transformation !*! rotateY (-0.1))
      postRedisplay Nothing
    KeyDown -> do
      transformation <- readIORef ioref
      let localVector = V3 0.0 0.0 1.0
          worldVector = (transformation ^. _m33) !* localVector
      writeIORef ioref $ over translation (^+^ worldVector) transformation
      postRedisplay Nothing
keymouse _ _ _ _ _ = return ()

-- |Create a rotation of x radians and return this rotation as a transformation
-- matrix.
rotateY :: GLfloat -> M44 GLfloat
rotateY angle = mkTransformation (axisAngle (V3 0.0 1.0 0.0) angle) (V3 0 0 0)

-- |Number of spheres to draw.
numSpheres :: Int
numSpheres = 50
