      SUBROUTINE PU_2DRECT(XWLD1,YWLD1,XWLD2,YWLD2,COLOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw the rectangle in X-Y plane.
C-
C-   Inputs  : XWIND1,YWIND1 [F] - One corner of the rectangle and
C-             XWIND2,YWIND2 [F] - the diagonally opposite corner. Both
C-                                 points are World Coordinate.
C-             COLOR         [C*]- Color for the rectangle
C-
C-   Modified 01-JUL-1991   N. Oshima ( for Multi Viewports )
C-   Created  19-JUN-1991   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL          XWLD1,YWLD1,XWLD2,YWLD2
      CHARACTER*(*) COLOR
      REAL          BUFFER(85)
C----------------------------------------------------------------------
C-
      CALL JVSAVE(BUFFER)
C-
      CALL JRIGHT(.TRUE.)
      CALL JVUPNT(0., 0., 0.)
      CALL JUPVEC(0., 1., 0.)
      CALL JNORML(0., 0., -1.)
C-
      CALL JOPEN
      CALL PXCOLR(COLOR)
      CALL JRECT(XWLD1,YWLD1,XWLD2,YWLD2)
      CALL JCLOSE
      CALL JVLOAD(BUFFER)
  999 RETURN
      END
