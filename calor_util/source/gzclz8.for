      INTEGER FUNCTION GZCLZ8()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds link to zebra bank CLZ8 
C-                          (calib ICD laser - x8)
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
      INCLUDE 'D0$LINKS:IZCLZ8.LINK'
      INTEGER GZCGLZ,LCGLZ
C----------------------------------------------------------------------
      GZCLZ8 = 0
      IF ( LCGNH.GT.0 ) THEN
        LCGLZ = GZCGLZ()
        IF ( LCGLZ.GT.0 ) THEN
          IF (IC(LCGLZ-IZCLZ8).GE.IZCLZ8) GZCLZ8 = LC(LCGLZ-IZCLZ8)
        ENDIF
      ENDIF
C
  999 RETURN
      END
