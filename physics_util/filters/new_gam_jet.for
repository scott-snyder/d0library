      LOGICAL FUNCTION NEW_GAM_JET()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Filter to select photon + jets events for
C-                         b-prime search.
C-
C-   Controls: NEW_GAM_JET_RCP
C-
C-   Returned value  : .TRUE. = pass
C-
C-   Alternate ENTRY point: NEW_GAM_JET_EOJ - Print statistics.
C-
C-   Created  17-Feb-1994   Herbert Greenlee
C-   Updated  12-MAY-1994   Meenakshi Narain  add protection for divide
C-                              by zero while computing pelc/ppho isolaton
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      LOGICAL NEW_GAM_JET_EOJ
      INTEGER JET_ALG
      INTEGER LJETS, LPPHO, LPELC, LHMTR
      INTEGER GZJETS, GZPPHO, GZPELC
      REAL ET, ETA, PHI, CHISQ, FISOL
      INTEGER NJET, NPHOT, NELEC
      REAL JET_MIN_ET, JET_MAX_ETA, JET_MATCH_DRMIN
      REAL PHOT_MIN_ET, PHOT_MAX_ETA, PHOT_MAX_FISOL, PHOT_MAX_CHISQ
      REAL ELEC_MIN_ET, ELEC_MAX_ETA, ELEC_MAX_FISOL, ELEC_MAX_CHISQ
      INTEGER NUM_ELEC_MIN, NUM_PHOT_MIN, NUM_EM_MIN, NUM_OBJ_MIN
      LOGICAL FILTER
      LOGICAL PASS
C-
C- List of EM clusters for jet matching.
C-
      INTEGER NUM_EMCLUS, MAX_EMCLUS
      PARAMETER(MAX_EMCLUS=10)
      REAL ETA_EMCLUS(MAX_EMCLUS), PHI_EMCLUS(MAX_EMCLUS)
      REAL DR, DETA, DPHI
      LOGICAL MATCH
      REAL PROXIM
C-
C- Statistics.
C-
      INTEGER NEVENT, NPASS, NPASS_PHOT, NPASS_ELEC
      REAL PASS_RATE
C-
C- Miscellanious.
C-
      INTEGER I, J, IER
      LOGICAL FIRST
C-
      DATA FIRST/.TRUE./
      DATA NEVENT, NPASS, NPASS_PHOT, NPASS_ELEC/4*0/
C----------------------------------------------------------------------
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL INRCP('NEW_GAM_JET_RCP',IER)
        IF(IER.NE.0)THEN
          CALL ERRMSG('NEW_GAM_JET','NEW_GAM_JET_RCP',
     &      'Error reading RCP file','F')
        ENDIF
        CALL EZPICK('NEW_GAM_JET_RCP')
        CALL EZGET('DO_NEW_GAM_JET', FILTER, IER)
        IF(IER.EQ.0)CALL EZGET('ELEC_MIN_ET', ELEC_MIN_ET, IER)
        IF(IER.EQ.0)CALL EZGET('ELEC_MAX_ETA', ELEC_MAX_ETA, IER)
        IF(IER.EQ.0)CALL EZGET('ELEC_MAX_FISOL', ELEC_MAX_FISOL, IER)
        IF(IER.EQ.0)CALL EZGET('ELEC_MAX_CHISQ', ELEC_MAX_CHISQ, IER)
        IF(IER.EQ.0)CALL EZGET('PHOT_MIN_ET', PHOT_MIN_ET, IER)
        IF(IER.EQ.0)CALL EZGET('PHOT_MAX_ETA', PHOT_MAX_ETA, IER)
        IF(IER.EQ.0)CALL EZGET('PHOT_MAX_FISOL', PHOT_MAX_FISOL, IER)
        IF(IER.EQ.0)CALL EZGET('PHOT_MAX_CHISQ', PHOT_MAX_CHISQ, IER)
        IF(IER.EQ.0)CALL EZGET('JET_ALG', JET_ALG, IER)
        IF(IER.EQ.0)CALL EZGET('JET_MATCH_DRMIN', JET_MATCH_DRMIN,
     &    IER)
        IF(IER.EQ.0)CALL EZGET('JET_MIN_ET', JET_MIN_ET, IER)
        IF(IER.EQ.0)CALL EZGET('JET_MAX_ETA', JET_MAX_ETA, IER)
        IF(IER.EQ.0)CALL EZGET('NUM_PHOT_MIN', NUM_PHOT_MIN, IER)
        IF(IER.EQ.0)CALL EZGET('NUM_ELEC_MIN', NUM_ELEC_MIN, IER)
        IF(IER.EQ.0)CALL EZGET('NUM_EM_MIN', NUM_EM_MIN, IER)
        IF(IER.EQ.0)CALL EZGET('NUM_OBJ_MIN', NUM_OBJ_MIN, IER)
        IF (IER.NE.0) THEN
          CALL ERRMSG('NEW_GAM_JET','NEW_GAM_JET_RCP',
     &        'Error getting NEW_GAM_JET RCP values','F')
        ENDIF
        CALL EZRSET
      ENDIF
C
C ****  intializations
C
      NEW_GAM_JET = .FALSE.
      PASS   = .FALSE.
      NJET = 0
      NPHOT = 0
      NELEC = 0
      NUM_EMCLUS = 0
      NEVENT = NEVENT + 1
C-
C- Apply selection cuts?
C-
      IF (.NOT.FILTER) THEN
        PASS = .TRUE.
        GO TO 998
      ENDIF
C-
C- Count photons
C-
      LPPHO = GZPPHO()
      DO WHILE(LPPHO.GT.0)
        ET = Q(LPPHO+7)
        ETA = Q(LPPHO+9)
        PHI = Q(LPPHO+10)
        IF (Q(LPPHO+17).GT.0) THEN
          FISOL = (Q(LPPHO+16) - Q(LPPHO+17))/Q(LPPHO+17)
        ELSE
          FISOL = 0.
        ENDIF
        LHMTR = LQ(LPPHO-1)
        IF (LHMTR.GT.0) THEN
          CHISQ = Q(LHMTR+7)
        ELSE
          CHISQ = 0.
        ENDIF
        IF(ET.GE.PHOT_MIN_ET .AND. ABS(ETA).LE.PHOT_MAX_ETA .AND.
     &    CHISQ.LE.PHOT_MAX_CHISQ .AND. FISOL.LE.PHOT_MAX_FISOL .AND.
     &    NUM_EMCLUS.LT.MAX_EMCLUS)THEN
          NPHOT = NPHOT + 1
          NUM_EMCLUS = NUM_EMCLUS + 1
          ETA_EMCLUS(NUM_EMCLUS) = ETA
          PHI_EMCLUS(NUM_EMCLUS) = PHI
        ENDIF
        LPPHO = LQ(LPPHO)
      ENDDO
C-
C- Count electrons
C-
      LPELC = GZPELC()
      DO WHILE(LPELC.GT.0)
        ET = Q(LPELC+7)
        ETA = Q(LPELC+9)
        PHI = Q(LPELC+10)
        IF (Q(LPELC+17).GT.0) THEN
          FISOL = (Q(LPELC+16) - Q(LPELC+17))/Q(LPELC+17)
        ELSE
          FISOL = 0.
        ENDIF
        LHMTR = LQ(LPELC-1)
        IF (LHMTR.GT.0) THEN
          CHISQ = Q(LHMTR+7)
        ELSE
          CHISQ = 0.
        ENDIF
        IF(ET.GE.ELEC_MIN_ET .AND. ABS(ETA).LE.ELEC_MAX_ETA .AND.
     &    CHISQ.LE.ELEC_MAX_CHISQ .AND. FISOL.LE.ELEC_MAX_FISOL .AND.
     &    NUM_EMCLUS.LT.MAX_EMCLUS)THEN
          NELEC = NELEC + 1
          NUM_EMCLUS = NUM_EMCLUS + 1
          ETA_EMCLUS(NUM_EMCLUS) = ETA
          PHI_EMCLUS(NUM_EMCLUS) = PHI
        ENDIF
        LPELC = LQ(LPELC)
      ENDDO
C-
C- Count jets.  Test for EM cluster match.
C-
      CALL SET_CAPH_ALG(JET_ALG)
      LJETS = GZJETS()
      DO WHILE(LJETS.GT.0)
        ET = Q(LJETS+6)
        ETA = Q(LJETS+9)
        PHI = Q(LJETS+8)
        IF(ET.GE.JET_MIN_ET .AND. ABS(ETA).LE.JET_MAX_ETA)THEN
          MATCH = .FALSE.
          DO I = 1, NUM_EMCLUS
            DETA = ETA - ETA_EMCLUS(I)
            DPHI = PROXIM(PHI - PHI_EMCLUS(I), 0.)
            DR = SQRT(DETA**2 + DPHI**2)
            MATCH = MATCH .OR. DR.LT.JET_MATCH_DRMIN
          ENDDO
          IF(.NOT.MATCH)NJET = NJET + 1
        ENDIF
        LJETS = LQ(LJETS)
      ENDDO
      CALL RESET_CAPH
C-
C- Filter logic
C-
      PASS = NPHOT .GE. NUM_PHOT_MIN .AND.
     &       NELEC .GE. NUM_ELEC_MIN .AND.
     &       NPHOT+NELEC .GE. NUM_EM_MIN .AND.
     &       NPHOT+NELEC+NJET .GE. NUM_OBJ_MIN
C-
C- Accumulate statistics
C-
 998  CONTINUE
      IF(PASS .AND. NPHOT.GT.0)NPASS_PHOT = NPASS_PHOT + 1
      IF(PASS .AND. NELEC.GT.0)NPASS_ELEC = NPASS_ELEC + 1
      IF(PASS)THEN
        NEW_GAM_JET = .TRUE.
        NPASS = NPASS + 1
      ENDIF
      GO TO 999
 
      ENTRY NEW_GAM_JET_EOJ()
C-
C- Entry point to print statistics.
C-
      NEW_GAM_JET_EOJ = .TRUE.
C-
C- Calculate pass rates.
C-
      IF(NEVENT.GT.0)THEN
        PASS_RATE = FLOAT(NPASS)/FLOAT(NEVENT)
      ELSE
        PASS_RATE = 0.
      ENDIF
      PRINT 10, NEVENT, NPASS_PHOT, NPASS_ELEC, NPASS, PASS_RATE
 10   FORMAT(
     &  /' NEW_GAM_JET filter statistics'
     &  //1X,I9,'  Events processed'
     &  /1X,I9,'  Events passed w/photons'
     &  /1X,I9,'  Events passed w/electrons'
     &  /1X,I9,'  Events passed'
     &  //1X,G9.3,'  Pass rate')
  999 RETURN
      END
