      INTEGER FUNCTION GZCLB1()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds link to zebra bank CLB1 
C-                          (calib ICD laser bad channels)
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
      INCLUDE 'D0$LINKS:IZCLB1.LINK'
      INTEGER GZCLZ1,LCLZ1
C----------------------------------------------------------------------
      GZCLB1 = 0
      LCLZ1 = GZCLZ1()
      IF ( LCLZ1.GT.0 ) THEN
        IF (IC(LCLZ1-IZCLB1).GE.IZCLB1) GZCLB1 = LC(LCLZ1-IZCLB1)
      ENDIF
C
  999 RETURN
      END
