      FUNCTION GZTPDH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns link to current TPDH bank
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   5-OCT-1990   JFG
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$LINKS:IZTPDH.LINK'
      INCLUDE 'D0$LINKS:IZSTRD.LINK'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER GZTPDH,GZSTRD
      GZTPDH = 0
        LSTRD = GZSTRD()
      IF ( LSTRD .GT. 0 ) THEN
        GZTPDH = LC(LSTRD - IZTPDH)
      ENDIF
  999 RETURN
      END
