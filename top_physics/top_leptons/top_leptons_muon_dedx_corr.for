      SUBROUTINE TOP_LEPTONS_MUON_DEDX_CORR(LPMUO,NOJT,ICOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Does dE/dx correction to muon momentum
C-                         for isolated muon tracks
C-
C-   Inputs  : 
C-              LPMUO - PMUO Bank Pointer
C-              NOJT  - No of good jets banks
C-   Outputs : 
C-              ICOR - -1/1 no correction/correction made
C-              Modified PMUO Bank
C-   Controls: 
C-
C-   Created  29-JUL-1993   Stephen J. Wimpenny
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      LOGICAL FIRST,CORR_JETS
C
      INTEGER LPMUO,NOJT,ICOR,IOK,IER
      INTEGER LJETS_DRMIN,LJETS_DPHI
C
      REAL MAX_EDIF_CONES,MAX_EDIF_CONES_CF,MAX_EDIF_CONES_EF
      REAL ERADG,ERAD,ECOR,EISO,P_MU_CORR,PT_MU_CORR
      REAL FACT,DR_MU_CORE,DR_MU_ISOL
      REAL DR_MIN_MUJET,DDR_MIN_MUJET,DR_MIN
      REAL E_DRMIN,ET_DRMIN,PX_DRMIN,PY_DRMIN,PZ_DRMIN,PHI_DRMIN
      REAL ETMIN_CUT,DETMIN_CUT,DPHI_MIN,ETA_DRMIN
C
      DATA DDR_MIN_MUJET,DETMIN_CUT/ 0.5, 10.0/
      DATA FIRST/.TRUE./
C
      IF(FIRST) THEN
        IER = 0
C
C *** Get all latest parameter/Options Values
C
        CALL EZPICK('TOP_LEPTONS_RCP')
        CALL EZGET('CORECONE_SIZE',DR_MU_CORE,IER)
        IF(IER.EQ.0) CALL EZGET('ISOCONE_SIZE',DR_MU_ISOL,IER)
        IF(IER.EQ.0) CALL EZGET('ISOCONE_CUT_CF',MAX_EDIF_CONES_CF,IER)
        IF(IER.EQ.0) CALL EZGET('ISOCONE_CUT_EF',MAX_EDIF_CONES_EF,IER)
        IF(IER.EQ.0) CALL EZGET_l('JETS_CORR',CORR_JETS,IER)
        IF(IER.EQ.0) CALL EZGET('MU_JET_DRMIN',DR_MIN_MUJET,IER)
        IF(IER.EQ.0) CALL EZGET('ETMIN_JET_DRMIN',ETMIN_CUT,IER)
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error TOP_LEPTONS_RCP',
     &    'TOP_LEPTONS_MUON_DEDX_CORR',' ','F')
        FIRST=.FALSE.
      ENDIF
C
      ICOR=-1
      IER=0
C
C *** Protect against stupidities
C
      IF(DR_MIN_MUJET.LT.DDR_MIN_MUJET) DR_MIN_MUJET=DDR_MIN_MUJET
      IF(ETMIN_CUT.LT.DETMIN_CUT) ETMIN_CUT=DETMIN_CUT
C
C *** Check if correction has already been applied
C
      IF(IQ(LPMUO+3).LT.3) THEN
        IF(IQ(LPMUO+7).GT.4) THEN
          MAX_EDIF_CONES=MAX_EDIF_CONES_EF
        ELSE
          MAX_EDIF_CONES=MAX_EDIF_CONES_CF
        ENDIF
        CALL TOP_LEPTONS_UTIL_MUISO_CORE(LPMUO,DR_MU_CORE,DR_MU_ISOL,
     1    MAX_EDIF_CONES,ERADG,ERAD,ECOR,EISO,IOK)
C
C *** If isolated by double cone cut then check for jets using dR(mu-jet)
C
        IF(IOK.GT.0) THEN
          IF(NOJT.GT.0) THEN
C
C *** Now check dRmin(mu-jet) algorithm
C
            CALL TOP_LEPTONS_UTIL_NEARJET(Q(LPMUO+16),Q(LPMUO+17),
     1        DR_MIN,LJETS_DRMIN,DPHI_MIN,LJETS_DPHI)
            IF(DR_MIN.LT.DR_MIN_MUJET) THEN
C
C *** Now check that jet is above minimum threshold
C
              IF(LJETS_DRMIN.LE.0) THEN
                WRITE(12,1000) LJETS_DRMIN
                GO TO 999
              ENDIF
              IF(CORR_JETS) THEN
                CALL TOP_LEPTONS_CORR_JETPARM(LJETS_DRMIN,E_DRMIN,
     1            ET_DRMIN,PX_DRMIN,PY_DRMIN,PZ_DRMIN,PHI_DRMIN,
     2            ETA_DRMIN,IER)
                IF(IER.LT.0) THEN
                  ET_DRMIN=Q(LJETS_DRMIN+6)
                ENDIF
              ELSE
                ET_DRMIN=Q(LJETS_DRMIN+6)
              ENDIF
              IF(ET_DRMIN.GT.ETMIN_CUT) THEN
                IER=-1
              ENDIF
            ENDIF
          ENDIF
          IF(IER.GE.0) THEN
C
C *** All cuts passed -> modify PMUO
C
            P_MU_CORR = Q(LPMUO+13) - ERADG + ERAD
            PT_MU_CORR = P_MU_CORR * Q(LPMUO+14) / Q(LPMUO+13)
            FACT = P_MU_CORR / Q(LPMUO+13)
            Q(LPMUO+10) = Q(LPMUO+10) * FACT
            Q(LPMUO+11) = Q(LPMUO+11) * FACT
            Q(LPMUO+12) = Q(LPMUO+12) * FACT
            Q(LPMUO+13) = P_MU_CORR
            Q(LPMUO+14) = PT_MU_CORR
            FACT=FACT**2
            Q(LPMUO+26) = Q(LPMUO+26) * FACT
            Q(LPMUO+27) = Q(LPMUO+27) * FACT
            Q(LPMUO+28) = Q(LPMUO+28) * FACT
            Q(LPMUO+29) = Q(LPMUO+29) * FACT
            Q(LPMUO+30) = Q(LPMUO+30) * FACT
            Q(LPMUO+33) = ERAD
            IQ(LPMUO+3) = 3
            ICOR = 1
          ENDIF      
        ENDIF
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
 1000 FORMAT(' ====> TOP_LEPTONS_MUON_DEDX_CORR <===== ',/,
     1 5X,' JETS Bank Problem , Pointer = ',I10,/)
      END
