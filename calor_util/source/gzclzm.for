      INTEGER FUNCTION GZCLZM()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds link to zebra bank CLZM 
C-                          (calib ICD laser moments)
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
      INCLUDE 'D0$LINKS:IZCLZM.LINK'
      INTEGER GZCLZ1,LCLZ1
C----------------------------------------------------------------------
      GZCLZM = 0
      LCLZ1 = GZCLZ1()
      IF ( LCLZ1.GT.0 ) THEN
        IF (IC(LCLZ1-IZCLZM).GE.IZCLZM) GZCLZM = LC(LCLZ1-IZCLZM)
      ENDIF
C
  999 RETURN
      END
