      INTEGER FUNCTION GZCG0B()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds link to zebra bank CG0B 
C-                          (calorimeter calib level-0 gains bad channels)
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
      INCLUDE 'D0$LINKS:IZCG0B.LINK'
      INTEGER GZCGL0,LCGL0
C----------------------------------------------------------------------
      GZCG0B = 0
      LCGL0 = GZCGL0()
      IF ( LCGL0.GT.0 ) THEN
        IF (IC(LCGL0-IZCG0B).GE.IZCG0B) GZCG0B = LC(LCGL0-IZCG0B)
      ENDIF
C
  999 RETURN
      END
