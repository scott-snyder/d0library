      INTEGER FUNCTION GZCPTR()

C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds link to zebra bank CPTR
C-                            Calorimeter trigger pedestals
C-   Returned value  : Link to bank
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  15-SEP-1993   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCPTR.LINK'
      INTEGER GZCPDH
C----------------------------------------------------------------------
      GZCPTR = 0
      LCPDH = GZCPDH()
      IF ( LCPDH.GT.0 ) THEN
        GZCPTR = LC(LCPDH-IZCPTR)
      ENDIF
  999 RETURN
      END
