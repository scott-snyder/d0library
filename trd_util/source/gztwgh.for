      FUNCTION GZTWGH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Returns link to current TWGH bank 
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
      INCLUDE 'D0$LINKS:IZTWGH.LINK'
      INCLUDE 'D0$LINKS:IZTGAI.LINK'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER GZTWGH,GZSTRD
      GZTWGH = 0
      IF ( LTGAI .GT. 0 ) THEN
        GZTWGH = LC(LTGAI - IZTWGH)
      ELSE
        LSTRD = GZSTRD()
        IF (LSTRD.GT.0) THEN
         LTGAI = LC(LSTRD-IZTGAI)
          IF (LTGAI.GT.0) THEN
           GZTWGH = LC(LTGAI - IZTWGH)
          ENDIF
        ENDIF
      ENDIF
  999 RETURN
      END
