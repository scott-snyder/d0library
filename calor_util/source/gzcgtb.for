      INTEGER FUNCTION GZCGTB()

C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds link to zebra bank CGTB
C-                            Calorimeter trigger gains bad channels
C-   Returned value  : Link to bank
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  13-JUL-1994   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCGTR.LINK'
      INCLUDE 'D0$LINKS:IZCGTB.LINK'
      INTEGER GZCGNH,GZCGTR,LCGTR
C----------------------------------------------------------------------
      GZCGTB = 0
      LCGNH = GZCGNH()
      IF ( LCGNH.GT.0 ) THEN
        LCGTR = LC(LCGNH-IZCGTR)
        IF ( LCGTR.GT.0 ) THEN
          GZCGTB = LC(LCGTR-IZCGTB)
        ENDIF
      ENDIF
  999 RETURN
      END
