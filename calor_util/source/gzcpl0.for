      INTEGER FUNCTION GZCPL0()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds link to zebra bank CPL0
C-                            Level-0 channels - pedestals
C-   Returned value  : Link to bank
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   5-MAR-1994   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCPL0.LINK'
      INTEGER GZCPDH
C----------------------------------------------------------------------
      GZCPL0 = 0
      LCPDH = GZCPDH()
      IF ( LCPDH.GT.0 ) THEN
        GZCPL0 = LC(LCPDH-IZCPL0)
      ENDIF
  999 RETURN
      END
