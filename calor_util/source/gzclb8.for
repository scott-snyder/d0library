      INTEGER FUNCTION GZCLB8()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds link to zebra bank CLB8 
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
      INCLUDE 'D0$LINKS:IZCLB8.LINK'
      INTEGER GZCLZ8,LCLZ8
C----------------------------------------------------------------------
      GZCLB8 = 0
      LCLZ8 = GZCLZ8()
      IF ( LCLZ8.GT.0 ) THEN
        IF (IC(LCLZ8-IZCLB8).GE.IZCLB8) GZCLB8 = LC(LCLZ8-IZCLB8)
      ENDIF
C
  999 RETURN
      END
