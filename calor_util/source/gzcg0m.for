      INTEGER FUNCTION GZCG0M()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds link to zebra bank CG0M 
C-                          (calorimeter calib level-0 gains moments)
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
      INCLUDE 'D0$LINKS:IZCG0M.LINK'
      INTEGER GZCGL0,LCGL0
C----------------------------------------------------------------------
      GZCG0M = 0
      LCGL0 = GZCGL0()
      IF ( LCGL0.GT.0 ) THEN
        IF (IC(LCGL0-IZCG0M).GE.IZCG0M) GZCG0M = LC(LCGL0-IZCG0M)
      ENDIF
C
  999 RETURN
      END
