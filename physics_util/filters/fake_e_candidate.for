      FUNCTION FAKE_E_CANDIDATE()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Pick 5-jet events satisfying the following
C-     conditions: (i) each jet has Et above a threshold (ET_MIN) and 
C      (ii) the em fraction of the Et of at least one jet is above some 
C-     threshold (EMFR_MIN).  (iii) Et of the jet passing the EMFR cut
C-     ia above a threshold (ET_MIN_EMJ). All these thresholds and the 
C-     jet algorithm (JET_ALGO) are to be set in the RCP file
C-     FAKE_E_CANDIDATE_RCP.
C-
C-   Returned value  :  .TRUE. if all cuts are satisfied.
C-   Inputs:   None
C-   Outputs:  None
C-   Controls:  None
C-
C-   Created  28-FEB-1994   Dhiman Chakraborty
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$LINKS:IZVERT.LINK/LIST'
      INTEGER GZJETS
      INTEGER LJETS
      INTEGER JET_ALGO,NJET,MIN_NJET
      INTEGER CNTR(20)
      INTEGER II,IER,RUNNO,EVONUM
      EXTERNAL RUNNO,EVONUM
      INTEGER FIRST_RUN,FIRST_EVENT,LAST_RUN,LAST_EVENT
      REAL    EMFR_MIN,ET_MIN,ET_MIN_EMJ,ETA_MAX,ETA_MAX_EMJ
      REAL    TEMPLATE(5,4)
      REAL    FECSUM(20)
      LOGICAL FAKE_E_CANDIDATE,FOUND_EMJ,AN_EMJ,JETS
      LOGICAL FIRST
      LOGICAL FAKE_E_CANDIDATE_EOJ
      SAVE FIRST
      DATA FIRST /.TRUE./
      DATA TEMPLATE/
     &  1.,6.,0.7,0.,0.,      ! CONE R=0.7
     &  1.,6.,0.5,0.,0.,      ! CONE R=0.5
     &  1.,6.,0.3,0.,0.,      ! CONE R=0.3
     &  2.,7.,2.,8.,2./       ! NN 2x2
C---------------------------------------------------------------------------
      IF(FIRST)THEN
        CALL INRCP('FAKE_E_CANDIDATE_RCP',IER)
        CALL EZPICK('FAKE_E_CANDIDATE_RCP')
        CALL EZGET_i('JET_ALGO',JET_ALGO,IER)
        CALL EZGET_i('MIN_NJET',MIN_NJET,IER)
        CALL EZGET('ET_MIN',ET_MIN,IER)
        CALL EZGET('ET_MIN_EMJ',ET_MIN_EMJ,IER)
        CALL EZGET('ETA_MAX',ETA_MAX,IER)
        CALL EZGET('ETA_MAX_EMJ',ETA_MAX_EMJ,IER)
        CALL EZGET('EMFR_MIN',EMFR_MIN,IER)
        CALL EZRSET
        CALL VZERO_i(CNTR,20)
        FIRST = .FALSE.
        FIRST_RUN = RUNNO()
        FIRST_EVENT = EVONUM()
      ENDIF
C
C ****  Initialization
C
      FAKE_E_CANDIDATE = .FALSE.
C
      LAST_RUN = RUNNO()
      LAST_EVENT = EVONUM()
      CNTR(1) = CNTR(1) +1          ! # of events scanned
C-
C- Jets
C-
      IF((JET_ALGO.GE.0).AND.(JET_ALGO.LE.3))  THEN
        CALL SET_CAPH('CONE_JET',TEMPLATE(1,JET_ALGO),IER)
      ELSEIF(JET_ALGO.EQ.4) THEN
        CALL SET_CAPH('NN_JET',TEMPLATE(1,4),IER)
      ELSE
        CALL ERRMSG('BAD JET_ALGO','FAKE_E_CANDIDATE',
     &    'Error setting CAPH path, check RCP','F')
      ENDIF
C
      NJET = 0
      JETS = .FALSE.
      FOUND_EMJ = .FALSE.
      AN_EMJ = .FALSE.
      FAKE_E_CANDIDATE = .FALSE.
      LJETS = GZJETS()
      IF (LJETS.LE.0)THEN
        CALL RESET_CAPH
        GOTO 999              ! no JETS
      ENDIF
C          sort banks so they are in increasing order of Et
C          NOTE: after each reordering of banks the pointer
C                LJETS must be refetched
      CALL ZSORT(IXCOM,LJETS,6)
      LJETS = GZJETS()
      CALL ZTOPSY(IXCOM,LJETS)
      LJETS = GZJETS()
      DO WHILE (LJETS.GT.0)
        IF((Q(LJETS+6).GT.ET_MIN).AND.
     &     (ABS(Q(LJETS+9)).LT.ETA_MAX))THEN
          NJET = NJET+1
          IF((Q(LJETS+14).GT.EMFR_MIN).AND.(Q(LJETS+6).GT.ET_MIN_EMJ))
     &     AN_EMJ = .TRUE.
          IF((Q(LJETS+14).GT.EMFR_MIN).AND.(Q(LJETS+6).GT.ET_MIN_EMJ).
     &     AND.(ABS(Q(LJETS+9)).LT.ETA_MAX_EMJ))FOUND_EMJ = .TRUE.
          IF((Q(LJETS+6).GT.ET_MIN_EMJ).AND.
     &     (ABS(Q(LJETS+9)).LT.ETA_MAX_EMJ))JETS = .TRUE.
        ENDIF
        LJETS = LQ(LJETS)
      ENDDO
      IF(NJET.GE.MIN_NJET) THEN
        CNTR(2) = CNTR(2) + 1
        IF(AN_EMJ) CNTR(4) = CNTR(4) + 1
        IF(JETS) THEN
          CNTR(3) = CNTR(3) + 1
          FAKE_E_CANDIDATE = FOUND_EMJ
          IF(FAKE_E_CANDIDATE) CNTR(5) = CNTR(5) + 1
        ENDIF
      ENDIF
      CALL RESET_CAPH
  999 RETURN
C ***************************************************************************
      ENTRY FAKE_E_CANDIDATE_EOJ(FECSUM)
C----------------------------------------------------------------------------
C-   Purpose and Methods : End of job summary for FAKE_E_CANDIDATE filter.  
C-   The REAL array FECSUM(20) may contain whatever accounting information 
C-   is of interest.
C-
C-   Inputs  : none
C-   Outputs : FECSUM(20) real array of summary results (only first 14 used)
C----------------------------------------------------------------------------
      CALL VZERO(FECSUM,20)
      DO II = 1,5
        FECSUM(II) = CNTR(II)
      ENDDO
C
      FECSUM(10) = MIN_NJET
      FECSUM(11) = ET_MIN
      FECSUM(12) = EMFR_MIN
      FECSUM(13) = ET_MIN_EMJ
      FECSUM(14) = ETA_MAX
      FECSUM(15) = ETA_MAX_EMJ
C
      FECSUM(16) = FIRST_RUN
      FECSUM(17) = LAST_RUN
      FECSUM(18) = FIRST_EVENT
      FECSUM(19) = LAST_EVENT
      FAKE_E_CANDIDATE_EOJ = .TRUE.
      END

