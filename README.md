### OpenGL Practice in Haskell

This repository contains Haskell translations of programs from the
[OpenGL SuperBible Fourth Edition](http://www.openglsuperbible.com/) with minor
modifications. All of these programs are found in the same executable. Simply
pass the name of the desired program to the executable as a command line
argument.

Programs include:
 * simple
 * glrect
 * glrectanimated
 * points
 * pointsz
 * lines
 * lstrips
 * linesw
 * lstipple
 * triangles
   * `--cull` - Cull surfaces.
   * `--depth` - Enable depth testing.
   * `--outline` - Only display wireframe of back surfaces.
 * star
   * `--edgeFlag` - Enables the edge flag which shows non-border lines.
 * scissor
 * stencil
 * atom
 * perspect
 * sphereworld
