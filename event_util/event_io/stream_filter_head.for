      FUNCTION STREAM_FILTER_HEAD()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      strip events after reading header record
C-      calls STREAM_FILTER if USE_HEADER is true, otherwise do nothing
C-
C-   Created   2-JUN-1992   Serban Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL STREAM_FILTER_HEAD,STREAM_FILTER
      LOGICAL USE_HEADER,FIRST
      INTEGER STATUS
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL EZPICK('STREAM_FILTER_RCP')
        CALL EZGET('USE_HEADER',USE_HEADER,STATUS)
        CALL EZRSET
      ENDIF
      STREAM_FILTER_HEAD=.TRUE.
      IF ( USE_HEADER ) THEN
        CALL STREAM_DONE(.FALSE.)
        STREAM_FILTER_HEAD=STREAM_FILTER()
        CALL STREAM_DONE(.TRUE.)
      ENDIF
  999 RETURN
      END
