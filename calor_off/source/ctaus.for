      FUNCTION CTAUS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-       event interface for CTAUS package
C-   Returned value : true
C-   Created  27-SEP-1990   Serban D. Protopopescu
C-   Updated   8-APR-1991   Scott Snyder - Add EZRSET call.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL CTAUS
      LOGICAL EZERR
      LOGICAL DO_ANALYSIS
      LOGICAL FIRST
      CHARACTER*8 JET_PATH
      INTEGER IER,IDX,LENF
      REAL    TEMPLATE(20)
      SAVE FIRST,JET_PATH,DO_ANALYSIS,LENF
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF ( FIRST )THEN                  ! LOCAL INIT
        FIRST = .FALSE.
        CALL EZPICK('CTAUS_RCP')       ! SELECT TAUS RCP BANK
C
        IF (EZERR(IER)) THEN
          CALL ERRMSG('CTAUS','CTAUS',
     &      'CTAUS RCP bank not found in CTAUS.','W')
        ELSE
          CALL EZGET('DO_ANALYSIS',DO_ANALYSIS,IER)
          CALL EZGETS ('JET_PATH',IDX,JET_PATH,LENF,IER)
          CALL EZGET('TEMPLATE',TEMPLATE,IER)
          CALL EZRSET
        ENDIF
C
      ENDIF
C
      CTAUS=.TRUE.
      CALL SET_CAPH(JET_PATH(1:LENF),TEMPLATE,IER)
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('CTAUS','CTAUS','CAPH for requested path not found',
     &    'W')
        GOTO 999
      ENDIF
      CALL PTAUFL
      CALL CTAUS_ANL(DO_ANALYSIS)
C
  999 CALL RESET_CAPH
      RETURN
      END
