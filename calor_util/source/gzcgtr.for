      INTEGER FUNCTION GZCGTR()

C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds link to zebra bank CGTR
C-                            Calorimeter trigger gains
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
      INCLUDE 'D0$LINKS:IZCGTR.LINK'
      INTEGER GZCGNH
C----------------------------------------------------------------------
      GZCGTR = 0
      LCGNH = GZCGNH()
      IF ( LCGNH.GT.0 ) THEN
        GZCGTR = LC(LCGNH-IZCGTR)
      ENDIF
  999 RETURN
      END
