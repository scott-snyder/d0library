      INTEGER FUNCTION GZCP0B()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds link to zebra bank CP0B 
C-                          (calorimeter calib level-0 pedestal bad channels)
C-
C-   Inputs  : none
C-   Outputs : Link to bank
C-   Controls: none
C-
C-   Created  13-APR-1994   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCP0B.LINK'
      INTEGER GZCGL0,LCGL0
C----------------------------------------------------------------------
      GZCP0B = 0
      LCGL0 = GZCGL0()
      IF ( LCGL0.GT.0 ) THEN
        IF (IC(LCGL0-IZCP0B).GE.IZCP0B) GZCP0B = LC(LCGL0-IZCP0B)
      ENDIF
C
  999 RETURN
      END
