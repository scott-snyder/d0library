      FUNCTION RECO_HSUM_FILL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      call all processes for event summaries
C-
C-   ENTRY RECO_HSUM_DUMP: dump event summary banks
C-
C-   ENTRY RECO_HSUM_RUN : read run dependent parameters 
C-                         for event summary
C-                         
C-   Created  20-MAR-1992   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL RECO_HSUM_FILL,RECO_HSUM_DUMP,RECO_HSUM_RUN
      INTEGER IER
      LOGICAL FIRST,FILL_TSUM,FILL_ESUM
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      RECO_HSUM_FILL=.TRUE.
      IF(FILL_TSUM) CALL TSUM_FILL
      IF(FILL_ESUM) CALL RECO_ESUM
      GOTO 999
C
      ENTRY RECO_HSUM_DUMP()
      RECO_HSUM_DUMP=.TRUE.
      CALL TSUM_DUMP
      CALL ESUM_DUMP
      GOTO 999
C
      ENTRY RECO_HSUM_RUN
      IF(FIRST) THEN
        CALL INRCP('RECO_ESUM_RCP',IER)
        FIRST=.FALSE.
      ENDIF
      CALL EZPICK('RECO_ESUM_RCP')
      CALL EZGET('FILL_TSUM',FILL_TSUM,IER)
      CALL EZGET('FILL_ESUM',FILL_ESUM,IER)
      IF(FILL_TSUM) CALL TSUM_RUNPAR(IER)
      RECO_HSUM_RUN=IER.EQ.0.OR..NOT.FILL_TSUM
      CALL EZRSET
  999 RETURN
      END
