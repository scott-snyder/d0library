      FUNCTION GZL0_CRATE()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  13-SEP-1993   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZL0_CRATE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZTRGR.LINK'
C
      INTEGER LTRGR,LCRATE,LCRATE0
      INTEGER GZFIND_CRATE
      EXTERNAL GZFIND_CRATE
C
      LOGICAL PRODUC,PRODFL
      LOGICAL FIRST
C
      SAVE FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        PRODFL = PRODUC()
        FIRST=.FALSE.
      ENDIF
      GZL0_CRATE = 0
C
C  fetch location in TRGR bank of L0 crate, crate 01
C
      LTRGR = LQ(LHEAD-IZTRGR)
      IF (LTRGR .EQ. 0) THEN
        IF (.NOT.PRODFL) CALL ERRMSG('LEVEL0-no-TRGR1','GZL0_CRATE',
     &                                 'TRGR bank not found','W')
        GOTO 999
      ENDIF
      LCRATE = GZFIND_CRATE('TRGR',LTRGR,1)
      IF (LCRATE .LE. 0) THEN
        IF (.NOT.PRODFL) CALL ERRMSG('LEVEL0-no-Crt01','GZL0_CRATE',
     &                            'Crate 01 data not found','W')
        GOTO 999
      ENDIF
C
      GZL0_CRATE = LCRATE - 1
C
C----------------------------------------------------------------------
  999 RETURN
      END
