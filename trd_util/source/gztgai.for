      INTEGER FUNCTION GZTGAI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : returns the link to the gain bank.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   6-NOV-1991   JFG
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$LINKS:IZTGAI.LINK'
      INCLUDE 'D0$LINKS:IZSTRD.LINK'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER GZSTRD
      GZTGAI = 0
        LSTRD = GZSTRD()
      IF ( LSTRD .GT. 0 ) THEN
        GZTGAI = LC(LSTRD - IZTGAI)
      ENDIF
  999 RETURN
      END
