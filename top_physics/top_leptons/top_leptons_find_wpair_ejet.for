      SUBROUTINE TOP_LEPTONS_FIND_WPAIR_EJET(IFGOOD,NOMU,NOEL,NOPH,
     1  NOJT,MET_VEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : event search routine for WW->e+jets decays
C-
C-   Inputs  : 
C-             NOMU         - no of 'good' PMUO candidates
C-             NOEL         - no of 'good' PELC candidates
C-             NOPH         - no of 'good' PPHO candidates
C-             NOJT         - no of 'good' JETS candidates
C-
C-   RCP file parameters :
C-
C-             MISSET_FACTOR   - multiplicative correction to missing
C-                               Et value
C-             CORR_JETS    - .TRUE./.FALSE. do/dont apply jet
C-                               corrections
C-             ETMIN_ELEC      - Etmin cut for electron
C-             NOJT_MIN        - minmum jet multiplicity (after cuts)
C-             PTMIN_JET1      - Ptmin cut for leading jet
C-             PTMIN_JET2      - Ptmin cut for second jet
C-             PTMIN_JET3      - Ptmin cut for third jet
C-             PTMIN_JET4      - Ptmin cut for fourth jet
C-             MISSET_MIN_CORR - minimum missing Et (muon corrected)
C-
C-   Outputs : 
C-              IFGOOD = .TRUE./.FALSE - event is/isn't a good candidate
C-
C-   Controls: None
C-
C-   Created  28-OCT-1992   Joey Thompson
C-   Modified 16-Mar-1993   Name changes for Good_Electron, Good_Photon
C-                          Good_Jet routines
C-   Modified 11-May-1993   HTD Wpair version.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      EXTERNAL TOP_LEPTONS_GOOD_MUON,TOP_LEPTONS_GOOD_JET
      EXTERNAL TOP_LEPTONS_GOOD_PHOTON,TOP_LEPTONS_GOOD_ELECTRON
C
      LOGICAL FIRST,IFGOOD,CORR_JETS,GOOD_MUON_TAG
      LOGICAL TOP_LEPTONS_GOOD_MUON,TOP_LEPTONS_GOOD_JET
      LOGICAL TOP_LEPTONS_GOOD_PHOTON,TOP_LEPTONS_GOOD_ELECTRON
C
      INTEGER NOEL,NOMU,NOPH,NOJT,JET_MULT_MIN,IV,IER,I,NOMUON_MIN
      INTEGER LPELC_VEC(5),LPMUO_VEC(5),LPPHO_VEC(5),LJETS_VEC(10)
      INTEGER I_EL,I_PH,I_JT,I_MU
      INTEGER ITEMP,NOJT_MIN
      INTEGER GZPELC,GZPPHO,GZPNUT,GZJETS,GZPMUO
      INTEGER LPELC,LPPHO,LPNUT2,LPNUT3,LJETS,LPMUO

C
      REAL ELEC_ETMIN,MUON1_PTMIN,MUON1_DRMAX
      REAL JET_PTMIN(4),JET_PT(10),JET_ETA(10)
      REAL JETS_EX,JETS_EY,JETS_EZ,JETS_E,JETS_ET,JETS_PHI,JETS_ETA
      REAL HT,HT_MIN,HT_JETPT_MIN,HT_JETETA_MAX,PTJTS_MIN,ETAJTS_MAX
      REAL PNUT3_ETMIN,MET_VEC(3),PNUT3_ET
      REAL PNUT2_ETMIN,PNUT2_ET
      REAL DR_NEARJET_MIN,ISOL_2N_MAX
      REAL DR_MIN,LJETS_MIN_DR,DPHI_MIN,LJETS_MIN_DPHI
      REAL MUON2_DRMAX
      REAL CONV,PI,TWOPI
C
      DATA FIRST/.TRUE./
      DATA CONV,PI,TWOPI/57.29578,3.1415927,6.2831853/
C----------------------------------------------------------------------
C
C *** Read cut parameters from RCP file
C
      IER=0
      IF(FIRST) THEN
        CALL EZPICK('TOP_LEPTONS_RCP')
C
        CALL EZGET('LJ_ELEC_PTMIN',ELEC_ETMIN,IER)
        IF (IER.EQ.0) CALL EZGET('LJ_MUON1_PTMIN',MUON1_PTMIN,IER)
        IF (IER.EQ.0) CALL EZGET('LJ_MUON2_DRMAX',MUON1_DRMAX,IER)
        IF (IER.EQ.0) CALL EZGET('LJ_PNUT2_ETMIN',PNUT2_ETMIN,IER)
        IF (IER.EQ.0) CALL EZGET('LJ_PNUT3_ETMIN',PNUT3_ETMIN,IER)
        IF (IER.EQ.0) CALL EZGET('LJ_JET_MULT_MIN',NOJT_MIN,IER)
        IF (IER.EQ.0) CALL EZGET('LJ_JET1_PTMIN',JET_PTMIN(1),IER)
        IF (IER.EQ.0) CALL EZGET('LJ_JET2_PTMIN',JET_PTMIN(2),IER)
        IF (IER.EQ.0) CALL EZGET('LJ_JET3_PTMIN',JET_PTMIN(3),IER)
        IF (IER.EQ.0) CALL EZGET('LJ_JET4_PTMIN',JET_PTMIN(4),IER)
        IF (IER.EQ.0) CALL EZGET('LJ_NOMUON_MIN',NOMUON_MIN,IER)
        IF (IER.EQ.0) CALL EZGET('LJ_HT_MIN',HT_MIN,IER)
        IF (IER.EQ.0) CALL EZGET('LJ_HT_JETPT_MIN',HT_JETPT_MIN,IER)
        IF (IER.EQ.0) CALL EZGET('LJ_HT_JETETA_MAX',HT_JETETA_MAX,IER)
        IF (IER.EQ.0) CALL EZGET('LJ_DR_NEARJET_MIN',DR_NEARJET_MIN,IER)
        IF (IER.EQ.0) CALL EZGET('LJ_ISOL_2N_MAX',ISOL_2N_MAX,IER)
        IF (IER.EQ.0) CALL EZGET('JETS_CORR',CORR_JETS,IER)
        IF (IER.EQ.0) CALL EZGET('JETS_ETAMAX',ETAJTS_MAX,IER)
        IF (IER.EQ.0) CALL EZGET('JETS_PTMIN',PTJTS_MIN,IER)
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error getting RCP parameters',
     &    'TOP_LEPTONS_FIND_EJET',' ','F')
        FIRST=.FALSE.
C
C *** Check consistency of HT selection and Jet preselection
C *** - reset limits if necessary
C
        IF(HT_JETPT_MIN.LT.PTJTS_MIN) HT_JETPT_MIN=PTJTS_MIN
        IF(HT_JETETA_MAX.GT.ETAJTS_MAX) HT_JETETA_MAX=ETAJTS_MAX
      ENDIF
C
      IFGOOD=.FALSE.
      GOOD_MUON_TAG=.FALSE.
      I_EL=0
      I_PH=0
      I_MU=0
      I_JT=0
      HT = 0.
C
C *** Get all relevent bank pointers
C *** PNUT3
C
      PNUT3_ET=0.
      PNUT2_ET=0.
      LPNUT3=GZPNUT(3)
      IF(LPNUT3.NE.0) PNUT3_ET=Q(LPNUT3+7)
      LPNUT2=GZPNUT(4)
      IF(LPNUT2.LE.0) LPNUT2=GZPNUT(2)
      IF(LPNUT2.NE.0) THEN
        PNUT2_ET=(Q(LPNUT2+3)+MET_VEC(1))**2 +
     1   (Q(LPNUT2+4)+MET_VEC(2))**2
        PNUT2_ET=SQRT(PNUT2_ET)
      ENDIF
C
      ITEMP=NOEL+NOPH
      IF(ITEMP.LT.1) GO TO 999
      IF(NOEL.LT.1) GO TO 100
C
C *** PELC
C
      LPELC=GZPELC()
      DO WHILE(LPELC.GT.0)
        IF(TOP_LEPTONS_GOOD_ELECTRON(LPELC)) THEN
          I_EL=I_EL+1
          IF(I_EL.LT.6) LPELC_VEC(I_EL)=LPELC
        ENDIF
        LPELC=LQ(LPELC)
      ENDDO
  100 IF(NOPH.LT.1) GO TO 200
C
C *** PPHO
C
      LPPHO=GZPPHO()
      DO WHILE(LPPHO.GT.0)
        IF(TOP_LEPTONS_GOOD_PHOTON(LPPHO)) THEN
          I_PH=I_PH+1
          IF(I_PH.LT.6) LPPHO_VEC(I_PH)=LPPHO
        ENDIF
        LPPHO=LQ(LPPHO)
      ENDDO
  200 IF(NOMU.LT.1) GO TO 300
C
C *** PMUO
C
      LPMUO=GZPMUO(0)
      DO WHILE(LPMUO.GT.0)
        IF(TOP_LEPTONS_GOOD_MUON(LPMUO)) THEN
          I_MU=I_MU+1
          IF(I_MU.LT.6) LPMUO_VEC(I_MU)=LPMUO
        ENDIF
        LPMUO=LQ(LPMUO)
      ENDDO
      IF(I_MU.LT.NOMUON_MIN) GO TO 999
  300 CONTINUE
C
C *** JETS   Cut on HT--sum ET in jets
C
      DO I = 1,10
        JET_PT(I) = 0.
      ENDDO
      LJETS=GZJETS()
      DO WHILE(LJETS.GT.0)
        IF(TOP_LEPTONS_GOOD_JET(LJETS)) THEN
          I_JT=I_JT+1
          IF(I_JT.LT.11)THEN
            LJETS_VEC(I_JT)=LJETS
C
C *** Use jet corrections if requested
C
            IF(CORR_JETS) THEN
              CALL TOP_LEPTONS_CORR_JETPARM(LJETS,JETS_E,JETS_ET,
     1          JETS_EX,JETS_EY,JETS_EZ,JETS_PHI,JETS_ETA,IER)
              IF(IER.GT.0) THEN
                JET_PT(I_JT)=JETS_ET
                JET_ETA(I_JT)=JETS_ETA
              ELSE
                JET_PT(I_JT)=Q(LJETS+6)
                JET_ETA(I_JT)=Q(LJETS+9)
              ENDIF
            ELSE  
              JET_PT(I_JT) = Q(LJETS+6)
              JET_ETA(I_JT)=Q(LJETS+9)
            ENDIF
          ENDIF
          IF((JET_PT(I_JT).GE.HT_JETPT_MIN).AND. 
     1      (ABS(JET_ETA(I_JT)).LE.HT_JETETA_MAX)) THEN
              HT = HT + JET_PT(I_JT)
          ENDIF
        ENDIF
        LJETS=LQ(LJETS)
      ENDDO
      IF(I_JT.LT.1) GO TO 999
C
C *** e+jets selection
C
      IF(I_EL.LT.1) GO TO 500
C
C *** Electron Et cut
C
      IF(Q(LPELC_VEC(1)+7).LT.ELEC_ETMIN) GO TO 500
C
C *** Muon tag requiremments
C
      IF(NOMUON_MIN.LT.1) GO TO 450
      IF(I_MU.GT.0) THEN
        DO I=1,I_MU
C
C *** Pt cut
C
          IF(Q(LPMUO_VEC(I)+14).LT.MUON1_PTMIN) GO TO 475
C
C *** Require muon to be in a jet
C
          CALL TOP_LEPTONS_UTIL_NEARJET(Q(LPMUO_VEC(I)+16),
     1      Q(LPMUO_VEC(I)+17),DR_MIN,LJETS_MIN_DR,DPHI_MIN,
     2      LJETS_MIN_DPHI)
          IF(DR_MIN.LT.MUON1_DRMAX) GOOD_MUON_TAG = .TRUE.
  475     CONTINUE
        ENDDO
      ENDIF
  450 CONTINUE
C
C *** Jet multiplicity and Pt cuts
C
      IF(I_JT.LT.NOJT_MIN) GO TO 500
      DO I = 1,NOJT_MIN
        IF(JET_PT(I).LT.JET_PTMIN(I)) GO TO 500
      ENDDO
C
C *** HT cut
C
      IF(HT.LT.HT_MIN) GO TO 500
C
C *** Missing ET cut
C
      IF (LPNUT3.EQ.0) GO TO 500  !Force PNUT to exist
      IF(PNUT3_ET.LT.PNUT3_ETMIN) GO TO 500
      IF(PNUT2_ET.LT.PNUT2_ETMIN) GO TO 500
C
C *** Good candidate
C
      IF(NOMUON_MIN.GE.1) THEN
        IF(GOOD_MUON_TAG) IFGOOD=.TRUE.
      ELSE
        IFGOOD=.TRUE.
      ENDIF
      GO TO 999
C
C -----------------------------------------------------------------------
C
  500 CONTINUE
C
C *** gamma+jets selection
C
      IF(I_PH.LT.1) GO TO 999
C
C *** Photon Et cut
C
      IF(Q(LPPHO_VEC(1)+7).LT.ELEC_ETMIN) GO TO 999
C
C *** Muon tag requiremments
C
      IF(NOMUON_MIN.LT.1) GO TO 550
      IF(I_MU.GT.0) THEN
        DO I=1,I_MU
C
C *** Pt cut
C
          IF(Q(LPMUO_VEC(I)+14).LT.MUON1_PTMIN) GO TO 575
C
C *** Require muon to be in a jet
C
          CALL TOP_LEPTONS_UTIL_NEARJET(Q(LPMUO_VEC(I)+16),
     1      Q(LPMUO_VEC(I)+17),DR_MIN,LJETS_MIN_DR,DPHI_MIN,
     2      LJETS_MIN_DPHI)
          IF(DR_MIN.LT.MUON1_DRMAX) GOOD_MUON_TAG = .TRUE.
  575     CONTINUE
        ENDDO
      ENDIF
  550 CONTINUE
C
C *** Jet multiplicity and Pt cuts
C
      IF(I_JT.LT.NOJT_MIN) GO TO 999
      DO I = 1,NOJT_MIN
        IF(JET_PT(I).LT.JET_PTMIN(I)) GO TO 999
      ENDDO
C
C *** HT cut
C
      IF(HT.LT.HT_MIN) GO TO 999
C
C *** Missing ET cut
C
      IF (LPNUT3.EQ.0) GO TO 999  !Force PNUT to exist
      IF(PNUT3_ET.LT.PNUT3_ETMIN) GO TO 999
      IF(PNUT2_ET.LT.PNUT2_ETMIN) GO TO 999
C
C *** Good candidate
C
      IF(NOMUON_MIN.GE.1) THEN
        IF(GOOD_MUON_TAG) IFGOOD=.TRUE.
      ELSE
        IFGOOD=.TRUE.
      ENDIF
C
  999 RETURN
      END
