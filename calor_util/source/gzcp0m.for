      INTEGER FUNCTION GZCP0M()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds link to zebra bank CP0M 
C-                          (calorimeter calib level-0 ped moments)
C-
C-   Inputs  : none
C-   Outputs : Link to bank
C-   Controls: none
C-
C-   Created   5-MAR-1994   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCP0M.LINK'
      INTEGER GZCPL0,LCPL0
C----------------------------------------------------------------------
      GZCP0M = 0
      LCPL0 = GZCPL0()
      IF ( LCPL0.GT.0 ) THEN
        IF (IC(LCPL0-IZCP0M).GE.IZCP0M) GZCP0M = LC(LCPL0-IZCP0M)
      ENDIF
C
  999 RETURN
      END
