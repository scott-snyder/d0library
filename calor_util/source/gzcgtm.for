      INTEGER FUNCTION GZCGTM()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds link to zebra bank CGTM 
C-                          (calib calorimeter trigger gain moments)
C-
C-   Inputs  : none
C-   Outputs : Link to bank
C-   Controls: none
C-
C-   Created  19-OCT-1993   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCGTM.LINK'
      INTEGER GZCGTR,LCGTR
C----------------------------------------------------------------------
      GZCGTM = 0
      LCGTR = GZCGTR()
      IF ( LCGTR.GT.0 ) THEN
        IF (IC(LCGTR-IZCGTM).GE.IZCGTM) GZCGTM = LC(LCGTR-IZCGTM)
      ENDIF
C
  999 RETURN
      END
