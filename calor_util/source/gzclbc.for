      INTEGER FUNCTION GZCLBC()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds link to zebra bank CLBC 
C-                          (calib ICD laser corrected-gains bad channels)
C-
C-   Inputs  : none
C-   Outputs : Link to bank
C-   Controls: none
C-
C-   Created   2-NOV-1994   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCLBC.LINK'
      INTEGER GZCLZC,LCLZC
C----------------------------------------------------------------------
      GZCLBC = 0
      LCLZC = GZCLZC()
      IF ( LCLZC.GT.0 ) THEN
        IF (IC(LCLZC-IZCLBC).GE.IZCLBC) GZCLBC = LC(LCLZC-IZCLBC)
      ENDIF
C
  999 RETURN
      END
