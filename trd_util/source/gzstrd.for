      FUNCTION GZSTRD()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank STRD 
C-
C-   Returned value  : pointer to Zebra bank STRD
C-
C-   Created   6-JUN-1990   J.Fr.Glicenstein   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSTRD.LINK'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER LSTPC,GZSTRD
C----------------------------------------------------------------------
      GZSTRD = 0
      IF ( LSTRD .GT. 0 ) THEN
        GZSTRD = LSTRD
      ELSE
        IF ( LSTPH .GT. 0 ) THEN
          LSTPC = LC( LSTPH - IZSTPC)
          IF ( LSTPC .GT. 0 ) 
     &       GZSTRD = LC( LSTPC - IZSTRD )
        ENDIF
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
