      SUBROUTINE ZCOGNG(OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : check if this event is not related with any
C-                         Jet trigger
C-
C-   Inputs  : none
C-   Outputs : OK: true - this event is not related with any Jet trigger
C-
C-   Created   2-SEP-1992   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NTRIG, ITBIT, TRIG_BITS(32)
      INTEGER IER
      LOGICAL OK, L1BIT_PASSED, TRIGOK
      LOGICAL FIRST, EZERROR
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      OK = .TRUE.
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL VZERO(TRIG_BITS(1),32)
        CALL EZPICK('VERTEX_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('VERTEX','ZCOGNG',
     &       'Unable to find bank VERTEX_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('NTRIG',NTRIG,IER)
        CALL EZGET('TRIG_BITS(1)',TRIG_BITS(1),IER)
        CALL EZRSET
      ENDIF
C
      IF (NTRIG .LE. 0) RETURN
      DO 100 ITBIT = 1, NTRIG
        TRIGOK = L1BIT_PASSED(TRIG_BITS(ITBIT))
        IF (TRIGOK) THEN
          OK = .FALSE.
          GOTO 999
        ENDIF
  100 CONTINUE
C
  999 RETURN
      END
