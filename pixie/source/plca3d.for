      SUBROUTINE PLCA3D
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw a calorimeter 3D view outline.
C-
C-   Modified 16-MAR-1992   N. Oshima( Just for 3D View. )
C-   Created  10-JUL-1990   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PXPARA.INC'
C----------------------------------------------------------------------
C-
C--- draw X, Y, Z axes
      CALL PXLWID(2)
      CALL PXCOLR('YEL')
      CALL J3MOVE(0., 0., 0.)
      CALL JR3DRA(240., 0., 0.)
      CALL JSIZE(10.,10.)
      CALL J3STRG('X')
      CALL J3MOVE(0., 0., 0.)
      CALL JR3DRA(0., 240., 0.)
      CALL JSIZE(10.,10.)
      CALL J3STRG('Y')
      CALL J3MOVE(0., 0., 0.)
      CALL JR3DRA(0., 0., 380.)
      CALL JSIZE(10.,10.)
      CALL JBASE( 0., 0., 1.)
      CALL JPLANE( 0., 1., 0.)
      CALL J3STRG('Z')
C-
C--- BEAM PIPE
      CALL PXCOLR('FOR')
      CALL JCIRCL(0., 0. , 370., 2.5, 0)
      CALL JCIRCL(0., 0. ,-370., 2.5, 0)
      CALL J3MOVE(0., 2.5,-370.)
      CALL JR3DRA(0., 0. , 740.)
      CALL J3MOVE(0.,-2.5,-370.)
      CALL JR3DRA(0., 0. , 740.)
C-
C--- MAIN RING
      CALL PXCOLR('FOR')
      CALL JCIRCL(-32.97, 206.9, 370., 9.05, 0)
      CALL JCIRCL(-32.97, 206.9,-370., 9.05, 0)
      CALL JLSTYL(2)
      CALL J3MOVE(-32.97, 215.95,-370.)
      CALL JR3DRA(  0.  ,  0.   , 740.)
      CALL J3MOVE(-32.97, 197.85,-370.)
      CALL JR3DRA(  0.  ,  0.   , 740.)
      CALL JLSTYL(0)
C-
  999 RETURN
      END
