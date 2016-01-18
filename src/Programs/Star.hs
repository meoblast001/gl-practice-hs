-- |
-- Module: Programs.Star
-- Copyright: (C) 2016 Braden Walters
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

module Programs.Star (display, Points.reshape) where

import qualified Data.HashSet as HS
import Graphics.GL.Standard21
import Graphics.GL.Types
import Graphics.UI.GLUT.Callbacks.Window
import Graphics.UI.GLUT.Window
import qualified Programs.Points as Points

-- |Called when the frame will be rendered.
display :: HS.HashSet String -> DisplayCallback
display longArgs = do
  let edgeFlag = if "edgeFlag" `HS.member` longArgs then GL_TRUE else GL_FALSE

  -- Background colour is black.
  glClearColor 0.0 0.0 0.0 1.0
  -- Drawing colour is green.
  glColor3f 0.0 1.0 0.0
  -- Only show edges.
  glPolygonMode GL_FRONT_AND_BACK GL_LINE
  -- Clear colour and depth buffer.
  glClear GL_COLOR_BUFFER_BIT

  glBegin GL_TRIANGLES

  -- Top spike.
  glEdgeFlag edgeFlag
  glVertex2f (-20.0) 0.0
  glEdgeFlag GL_TRUE
  glVertex2f 20.0 0.0
  glVertex2f 0.0 40.0

  -- Left spike.
  glVertex2f (-20.0) 0.0
  glVertex2f (-60.0) (-20.0)
  glEdgeFlag edgeFlag
  glVertex2f (-20.0) (-40.0)
  glEdgeFlag GL_TRUE

  -- Bottom spike.
  glVertex2f (-20.0) (-40.0)
  glVertex2f 0.0 (-80.0)
  glEdgeFlag edgeFlag
  glVertex2f 20.0 (-40.0)
  glEdgeFlag GL_TRUE


  -- Right spike.
  glVertex2f 20.0 (-40.0)
  glVertex2f 60.0 (-20.0)
  glEdgeFlag edgeFlag
  glVertex2f 20.0 0.0
  glEdgeFlag GL_TRUE

  -- Centre square as two triangles.
  glEdgeFlag edgeFlag
  glVertex2f (-20.0) 0.0
  glVertex2f (-20.0) (-40.0)
  glVertex2f 20.0 0.0

  glVertex2f (-20.0) (-40.0)
  glVertex2f 20.0 (-40.0)
  glVertex2f 20.0 0.0
  glEdgeFlag GL_TRUE

  glEnd

  -- Render.
  swapBuffers
