      INTEGER FUNCTION GZCLZC()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds link to zebra bank CLZC 
C-                          (calib ICD laser - corrected gains)
C-
C-   Inputs  : none
C-   Outputs : Link to bank
C-   Controls: none
C-
C-   Created   2-NOV-1993   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCLZC.LINK'
      INTEGER GZCGLZ,LCGLZ
C----------------------------------------------------------------------
      GZCLZC = 0
      IF ( LCGNH.GT.0 ) THEN
        LCGLZ = GZCGLZ()
        IF ( LCGLZ.GT.0 ) THEN
          IF (IC(LCGLZ-IZCLZC).GE.IZCLZC) GZCLZC = LC(LCGLZ-IZCLZC)
        ENDIF
      ENDIF
C
  999 RETURN
      END
