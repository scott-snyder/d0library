      INTEGER FUNCTION GZCPTM()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds link to zebra bank CPTM 
C-                          (calib calorimeter trigger ped moments)
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
      INCLUDE 'D0$LINKS:IZCPTM.LINK'
      INTEGER GZCPTR,LCPTR
C----------------------------------------------------------------------
      GZCPTM = 0
      LCPTR = GZCPTR()
      IF ( LCPTR.GT.0 ) THEN
        IF (IC(LCPTR-IZCPTM).GE.IZCPTM) GZCPTM = LC(LCPTR-IZCPTM)
      ENDIF
C
  999 RETURN
      END
