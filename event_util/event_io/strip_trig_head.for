      FUNCTION STRIP_TRIG_HEAD()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      strip events after reading header record
C-      calls STRIP_TRIG if USER_HEADER is true, otherwise do nothing
C-
C-   Created   2-JUN-1992   Serban Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL STRIP_TRIG_HEAD,STRIP_TRIG
      LOGICAL USE_HEADER,FIRST
      INTEGER STATUS
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL EZPICK('STRIP_RCP')
        CALL EZGET('USE_HEADER',USE_HEADER,STATUS)
        CALL EZRSET
      ENDIF
      STRIP_TRIG_HEAD=.TRUE.
      IF ( USE_HEADER ) THEN
        CALL STRIP_DONE(.FALSE.)
        STRIP_TRIG_HEAD=STRIP_TRIG()
        CALL STRIP_DONE(.TRUE.)
      ENDIF
  999 RETURN
      END
