      INTEGER FUNCTION GZCLZ1()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds link to zebra bank CLZ1 
C-                          (calib ICD laser - x1)
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
      INCLUDE 'D0$LINKS:IZCLZ1.LINK'
      INTEGER GZCGLZ,LCGLZ
C----------------------------------------------------------------------
      GZCLZ1 = 0
      IF ( LCGNH.GT.0 ) THEN
        LCGLZ = GZCGLZ()
        IF ( LCGLZ.GT.0 ) THEN
          IF (IC(LCGLZ-IZCLZ1).GE.IZCLZ1) GZCLZ1 = LC(LCGLZ-IZCLZ1)
        ENDIF
      ENDIF
C
  999 RETURN
      END
