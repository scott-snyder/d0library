      INTEGER FUNCTION GZSCDC()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank SCDC 
C-
C-   Returned value  : pointer to Zebra bank SCDC 
C-
C-   Created  19-APR-1989   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSCDC.LINK'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER LSTPC
C----------------------------------------------------------------------
      GZSCDC = 0
      IF ( LSCDC .GT. 0 ) THEN
        GZSCDC = LSCDC
      ELSE
        IF ( LSTPH .GT. 0 ) THEN
          LSTPC = LC( LSTPH - IZSTPC)
          IF ( LSTPC .GT. 0 ) 
     &       GZSCDC = LC( LSTPC - IZSCDC )
        ENDIF
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
