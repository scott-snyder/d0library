      SUBROUTINE PTAU_FIX
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : add more information for the PTAUs built earlier 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  25-MAY-1995   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INTEGER LPTAU, GZPTAU, LJETS
      INTEGER STATUS, IVER, IER
      INTEGER LENGTH, NEW_LENGTH, LDIFF
      INTEGER   HIDATA, HIETA(4), HIPHI(4), IHOT
      INTEGER*2 HIINFO(2)
      EQUIVALENCE (HIINFO(1),HIDATA)
      REAL    PROB(2), CHISQ(2), FVAR
      REAL    NEW_RMS
      REAL    HIET(4)
      REAL    THRESHOLD
      LOGICAL OK, FIRST, EZERR
      SAVE    FIRST
      DATA    FIRST/.TRUE./
      DATA    NEW_LENGTH/32/
C----------------------------------------------------------------------
C
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('CTAUS_RCP')       ! SELECT CTAUS RCP BANK
        IF (EZERR(IER)) THEN
          CALL ERRMSG('CTAUS','PTAUFL',
     &      'CTAUS RCP bank not found in PTAUFL.','W')
        ELSE
          CALL EZGET('THRESHOLD_FOR_RMS',THRESHOLD,IER)
          CALL EZRSET
        ENDIF
      ENDIF
      LPTAU = GZPTAU()
 100  IF (LPTAU .LE. 0) RETURN
      LJETS = LQ(LPTAU-2)
      IF (LJETS .LE. 0) THEN
        CALL ERRMSG('TAUFIX','PTAU_FIX',
     &      'no JETS bank, could not fix PTAU','W')
      ENDIF
      IVER = IQ(LPTAU+1)
      IF (IVER .LE. 4) THEN
        LENGTH = IQ(LPTAU-1)
        IF (LENGTH .LT. NEW_LENGTH) THEN
          LDIFF = NEW_LENGTH - LENGTH
          CALL MZPUSH(IXCOM,LPTAU,0,LDIFF,'I')
        ENDIF
        CALL CTAU_HOT(LJETS,HIET,HIETA,HIPHI)
        Q(LPTAU+12) = HIET(1)
        Q(LPTAU+13) = HIET(2)
        Q(LPTAU+22) = HIET(3)
        Q(LPTAU+23) = HIET(4)
        DO 101 IHOT = 1, 4
          HIINFO(WORD1) = HIETA(IHOT)
          HIINFO(WORD2) = HIPHI(IHOT)
          IQ(LPTAU+23+IHOT) = HIDATA
  101   CONTINUE
      ENDIF
      CALL CLEANTAU(LPTAU,STATUS,OK)
      IF (OK) IQ(LPTAU+14) = STATUS
      CALL GET_TAU_QUAN(LPTAU,PROB,CHISQ,FVAR)
      Q(LPTAU+28) = CHISQ(1)
      Q(LPTAU+29) = CHISQ(2)
      Q(LPTAU+30) = FVAR
      CALL CJET_NEWRMS(LJETS,THRESHOLD,NEW_RMS,OK)
      IF (OK) Q(LPTAU+31) = NEW_RMS
      LPTAU = LQ(LPTAU)
      GOTO 100
C
  999 RETURN
      END
