      INTEGER FUNCTION GZTGCH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Gets the link to first TGCH bank.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  23-OCT-1991   Jean-Francois Glicenstein
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTGCH.LINK'
      INCLUDE 'D0$LINKS:IZTGAI.LINK'
      INTEGER  GZSTRD
C
C--   INITIALIZE
      GZTGCH = 0
C
      IF (LTGAI.LE.0) THEN
       LSTRD = GZSTRD()
       IF (LSTRD.GT.0) THEN
        LTGAI = LC(LSTRD - IZTGAI)
       ENDIF
      ENDIF
C
      IF (LTGAI.GT.0) GZTGCH = LC(LTGAI - IZTGCH)      
  999 RETURN
      END
