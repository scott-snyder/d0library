      FUNCTION TAUFIX_EVT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Main routine for TAUFIX package
C-
C-   Returned value  :
C-
C-   Created  25-MAY-1995   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      LOGICAL TAUFIX_EVT
      LOGICAL VERFLG, FLGVAL
      LOGICAL FIRST, EZERR
      INTEGER LPTAU, GZPTAU, IER
      INTEGER USE_NEW_RMS
      CHARACTER*8 JET_PATH
      INTEGER IDX, LENF
      REAL    TEMPLATE(20)
      SAVE FIRST, JET_PATH, LENF
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C      VERFLG = FLGVAL('VERTEX_CHANGE')
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('CTAUS_RCP')       ! SELECT CTAUS RCP BANK
        IF (EZERR(IER)) THEN
          CALL ERRMSG('CTAUS','TAUFIX_EVT',
     &      'CTAUS_RCP bank not found','W')
        ELSE
          CALL EZGETS('JET_PATH',IDX,JET_PATH,LENF,IER)
          CALL EZGET('TEMPLATE',TEMPLATE,IER)
          CALL EZGET_i('USE_NEW_RMS',USE_NEW_RMS,IER)
          CALL EZRSET
        ENDIF
      ENDIF
      CALL SET_CAPH(JET_PATH(1:LENF),TEMPLATE,IER)
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('TAU_FIX','TAUFIX_FIX',
     &    'CAPH for requested path not found', 'W')
        GOTO 999
      ENDIF
      IF (USE_NEW_RMS .GT. 0) THEN
C
C ****  re do PTAU
C
        LPTAU = GZPTAU()
        IF (LPTAU .GT. 0) THEN
          CALL MZDROP(IXCOM,LPTAU,'L')
        ENDIF
        CALL PTAUFL         ! redo all the PTAUs
      ELSE
C
C ****  add more information for old PTAUs
C
        CALL PTAU_FIX
      ENDIF
      CALL RESET_CAPH
C
      TAUFIX_EVT = .TRUE.
C
  999 RETURN
      END
