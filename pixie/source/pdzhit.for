      SUBROUTINE PDZHIT(ZPOS,RPOS,IFDWIR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : draw a CDC hit in R-Z view
C-
C-   Inputs  : ZPOS: hit position in Z
C-             RPOS: hit position in R
C-             IFDWIR: flag to draw wires
C-   Outputs : 
C-
C-   Created  18-OCT-1989   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IFDWIR
      REAL    ZPOS, RPOS, SIZHIT, ZPOS1, ZPOS2
      PARAMETER( SIZHIT = 0.5 )
C----------------------------------------------------------------------
C  
C    draw a short line for a hit when the wires are not drawn
C    draw a "X" for a hit when the wires are drawn
C
      IF (IFDWIR .NE. 0) THEN
        CALL JCMARK(5)
        CALL JJUST(2,2)
        CALL JMARK(ZPOS, RPOS)
      ELSE
        ZPOS1 = ZPOS - SIZHIT
        ZPOS2 = ZPOS + SIZHIT
        CALL JMOVE(ZPOS1, RPOS)
        CALL JDRAW(ZPOS2, RPOS)
      ENDIF
  999 RETURN
      END
