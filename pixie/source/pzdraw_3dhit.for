      SUBROUTINE PZDRAW_3DHIT(XPOS,YPOS,ZPOS,SIZE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : draw a hit in 3D display (a hit with 6 legs)
C-
C-   Inputs  : XPOS,YPOS,ZPOS: the hit location in (X,Y,Z)
C-             SIZE: size for hit drawing
C-   Outputs : none
C-
C-   Created   9-JAN-1992   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    XPOS, YPOS, ZPOS
      REAL    SIZE
C----------------------------------------------------------------------
C
               CALL J3MOVE(XPOS-SIZE,YPOS,ZPOS)
               CALL J3DRAW(XPOS+SIZE,YPOS,ZPOS)
               CALL J3MOVE(XPOS,YPOS-SIZE,ZPOS)
               CALL J3DRAW(XPOS,YPOS+SIZE,ZPOS)
               CALL J3MOVE(XPOS,YPOS,ZPOS-SIZE)
               CALL J3DRAW(XPOS,YPOS,ZPOS+SIZE)
C
  999 RETURN
      END
