      SUBROUTINE DAREA_CORRECT(AREA,CORRECTED)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Correct pulse area based on pressure 
C-
C-   Inputs  : CDLOCA COMMON BLOCK, DENVADJ COMMON BLOCK
C- 
C-   Outputs : AREA,CORRECTED
C-   Controls:
C-
C-   Created  24-MAY-1993   Paul Rubinov
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:CDLOCA.INC'
      INCLUDE 'D0$INC:DENVADJ.INC'
C----------------------------------------------------------------------
      REAL    AREA
      LOGICAL CORRECTED
C----------------------------------------------------------------------
      IF ((ABS(GFAC_INNER-1.).GE.0.5).OR.(ABS(GFAC_OUTER-1.).GE.0.5))
     &  GOTO 900
      IF (.NOT.CORRECTION_MADE) GOTO 900
      IF ((WIRE.EQ.0).OR.(WIRE.EQ.6)) THEN
C
C  OUTER WIRES
        AREA = AREA * GFAC_OUTER
      ENDIF
      IF ((WIRE.LE.5).AND.(WIRE.GE.1)) THEN
C  INNER WIRES
        AREA = AREA * GFAC_INNER
      ENDIF
      IF (WIRE.GE.7) THEN
C  DELAY LINES
        AREA = AREA
      ENDIF
      CORRECTED=.TRUE.
      GOTO 999
C
  900 CORRECTED=.FALSE.
  999 RETURN
      END
