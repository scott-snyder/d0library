      SUBROUTINE TOP_LEPTONS_EVENT_SUMMRY(NOEVT_LOCAL,
     1 NOMU,NOMU_UNCUT,NOEL,NOEL_UNCUT,NOPH,NOPH_UNCUT,NOJT,
     2 NOJT_UNCUT,TOP_FLAG,W_FLAG,Z_FLAG,WWG_FLAG,QCD_FLAG,WPAIR_FLAG,
     3 MET_VEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Produce an event dump on unit LUN
C-
C-   Inputs  : 
C-             LUN          - Print Lun
C-             NOEVT_LOCAL  - local event no
C-             NOMU         - no. muons after cuts
C-             NOMU_UNCUT   - no. raw PMUO banks
C-             NOEL         - no. electrons after cuts
C-             NOEL_UNCUT   - no. raw PELC banks
C-             NOJT         - no. jets after cuts
C-             NOJT_UNCUT   - no. raw JETS banks
C-             LONG_DUMP    - .TRUE. => do full dump, otherwise just give
C-                            4-vector, eta, phi and theta
C-             TOP_FLAG     - ttbar selection flag array
C-             W_FLAG       - W selection flag array
C-             Z_FLAG       - Z selection flag array
C-             WWG_FLAG     - Wgamma selection flag array
C-
C-   Outputs : Printout only
C-
C-   Controls: None
C-
C-   Created  15-JUL-1992   Stephen J. Wimpenny
C-   Modified 16-JUL-1992   more information added + LONG_DUMP flag added
C-   Modified 30-JUL-1992   masses etc added
C-   Modified 18-Aug-1992   selection flags added
C-   Modified 27-Aug-1992   trigger and muon bits unpacked 
C-   Modified 08-Sep-1992   long electron/muon/photon dumps extended
C-   Modified 15-Sep-1992   Dump flag and LUN picked dierctly from RCP file
C-   Modified 24-Sep-1992   Checks on Electron/Photon/Muon/Jet Logicals
C-   Modified 08-Sep-1992   Vertex Information added
C-   Modified 18-Oct-1992   Muon Isolation Information added
C-   Modified  4-Dec-1992   Beam Crossing No added
C-   Modified 14-Dec-1992   Bend,Non-bend muon impact parameters added
C-   Modified 28-Dec-1992   Electron/Photon dumps updated 
C-   Modified 25-Jan-1993   Misc updates TOP_FLAG dimension,MICRO_BLANK,
C-                          full plane dumps for muon
C-   Modified 27-Jan-1993   Z-finder hooks added
C-   Modified 29-Jan-1993   TRD info (1st pass) added for electron
C-                          3-body cluster mass, dR(gam-mu,gam-e) added
C-   Modified  1-Feb-1993   WWgamma flags added
C-                          MRBS Loss flag + Reco version added
C-   Modified  5-Feb-1993   Fix error in dR for egam,mugam,mue
C-   Modified 11-Feb-1993   Fix error in W 4-vector calculation
C-   Modifoed 15-Mar-1993   Routine name changes for Good_Elecron,
C-                          Good_Photon, Good_Muon, Good_Jet, Long_Muon_Dump
C-                          Long_Muon_Dump_Old, Coplan, Mass4, Masst,
C-                          Nearjet, Calor_Ratios, Caltrak_Angles,
C-                          Which_Trig_Bits, Get_Vertx_Data,
C-                          Test_Micro_Blank, Test_Mrbs, Dr_From_Deta_Dphi
C-   Modified 15-Apr-1993   Detailed electron and Photon dumps moved to
C-                          separated routines, PtW added to W dumps and
C-                          dPhi bug for W fixed.
C-   Modified 22-Apr-1993   Jet Energy corrections added
C-   Modified  6-May-1993   Main Ring Timing Information Added
C-   Modified  7-Jul-1993   em,jet and missing Et scale corrections added
C-   Modified 14-Jul-1993   event shape and multiple interaction parameters 
C-                          added
C-   Modified 12-Feb-1994   Pick RECO Version/Pass from Library Utility
C-                          avoid problems in LHSTR due to Omni Filter
C-   Modified 12-May-1994   Muon MTCA infromation added to dump
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$LINKS:IZJETS.LINK'
C
      EXTERNAL TOP_LEPTONS_GOOD_MUON,TOP_LEPTONS_GOOD_JET
      EXTERNAL TOP_LEPTONS_GOOD_PHOTON,TOP_LEPTONS_GOOD_ELECTRON
      EXTERNAL TOP_LEPTONS_UTIL_MICRO_BLANK,TOP_LEPTONS_UTIL_MRBS_LOSS
      EXTERNAL TOP_LEPTONS_UTIL_MONTECARLO,TOP_LEPTONS_UTIL_MASS4
      EXTERNAL TOP_LEPTONS_UTIL_COPLAN,TOP_LEPTONS_UTIL_MASST
      EXTERNAL TOP_LEPTONS_UTIL_CALC_DR,TOP_LEPTONS_EM_CORRECTION
      EXTERNAL TOP_LEPTONS_UTIL_LAB_ANGLES
C
      LOGICAL FIRST,LONG_DUMP,CORR_JETS,CORR_EM
      LOGICAL TOP_LEPTONS_GOOD_MUON,TOP_LEPTONS_GOOD_JET
      LOGICAL TOP_LEPTONS_GOOD_PHOTON,TOP_LEPTONS_GOOD_ELECTRON
      LOGICAL TOP_LEPTONS_UTIL_MICRO_BLANK,TOP_LEPTONS_UTIL_MRBS_LOSS
      LOGICAL TOP_LEPTONS_UTIL_MONTECARLO,OK
C
      INTEGER LUN,NOEVT,NOEVT_L1PT1,NOEVT_L1PT2,NORUN,EVT_DATE,EVT_TIME
      INTEGER NOEVT_LOCAL,NOVERT_P,NOVERT_S,MR_PERMIT,MR_BITS
      INTEGER NOMU,NOMU_UNCUT,NOEL,NOEL_UNCUT,NOPH,NOPH_UNCUT
      INTEGER NOJT,NOJT_UNCUT
      INTEGER NO_TRIG_LVL1,TRIG_LVL1_BITS(32)
      INTEGER NO_TRIG_LVL2,TRIG_LVL2_BITS(64)
      INTEGER NO_ZTRAK,NO_ZTRAK_VERTX_P(5)
      INTEGER LJETS_DR,LJETS_DPHI,JET_ALG
      INTEGER LPMUO,LPELC,LPPHO,LJETS
      INTEGER LPNUT1,LPNUT2,LPNUT3,LPNUT4
      INTEGER PMUO_VERS
      INTEGER GZPMUO,GZPELC,GZPPHO,GZPNUT,GZJETS,GZHEAD
      INTEGER MUON_SIGN,I,IV,J
      INTEGER JBIT,I_JT_MAXPERM,RECO_VERS,RECO_PASS
      INTEGER LPELC_VEC(5),LPMUO_VEC(5),LPPHO_VEC(5),LJETS_VEC(10)
      INTEGER IER,I_EL,I_EL_INF,I_MU,I_PH,I_JT,ITEMP,JTEMP
      INTEGER TOP_FLAG(6),W_FLAG(3),Z_FLAG(3),WWG_FLAG(3),QCD_FLAG(6)
      INTEGER WPAIR_FLAG(6)
C
      REAL VERTX_P(3,5),VTX_Z,PBEAM,XW1,XW2,XZ
      REAL PHI_OPP,DPHI_MAX_VEC12,NEARJET_ET,FARJET_ET
      REAL ERR1,TWO_BODY_MASS,TWO_BODY_COPLAN,DPHI_LEP12,DILEP_VEC(4)
      REAL TOP_LEPTONS_UTIL_MASS4,TWO_BODY_MASST
      REAL PI,TWOPI,CONV,PT1,PT2,PTW,PTZ1,PTZ2
      REAL TOP_LEPTONS_UTIL_COPLAN,TOP_LEPTONS_UTIL_MASST
      REAL DPHI_JET12,DETA_VEC12,DPHI_VEC12,DR_VEC12
      REAL TOP_LEPTONS_UTIL_CALC_DR,TOP_LEPTONS_EM_CORRECTION
      REAL TIME29,MET_VEC(3),MISET_TEMP(4),MISET_THETA,MISET_PHI
      REAL WMASS,W4VEC1(4),W4VEC2(4),LEP4VEC(4),NU2VEC(2)
      REAL TEMP,TEMP2,TEMP3,TEMP4,VEC1(4),VEC2(4),VMOD,MT_CLUSTER
      REAL E,ET,PX,PY,PZ,PHI,ETA,E2,ET2,PX2,PY2,PZ2,PHI2,ETA2
      REAL HT_JETPT_MIN,HT_JETETA_MAX,ETAJTS_MAX,PTJTS_MIN
      REAL SPHER,PLAN,GSPHER,GAPLAN,GY,EMAXSH,ETMAXSH,EFOUR_SHAPE,HT,H
      REAL EL_COR(5),PH_COR(5)
C
      DATA PI,TWOPI,CONV/3.141593,6.283185,57.2958/
      DATA PBEAM,WMASS/ 900., 80.6/
      DATA FIRST/.TRUE./
C
      IF(FIRST) THEN
C
C *** Read LUN and Long-Dump flag from RCP file
C
        CALL EZPICK('TOP_LEPTONS_RCP')
        CALL EZGET_l('LONG_DUMPS',LONG_DUMP,IER)
        IF (IER.EQ.0) CALL EZGET_l('EM_CORR',CORR_EM,IER)
        IF (IER.EQ.0) CALL EZGET_l('JETS_CORR',CORR_JETS,IER)
        IF (IER.EQ.0) CALL EZGET_i('JETS_ALGORITHM',JET_ALG,IER)
        IF (IER.EQ.0) CALL EZGET_i('PRINT_LUN',LUN,IER)
        IF (IER.EQ.0) CALL EZGET('LJ_HT_JETPT_MIN',HT_JETPT_MIN,IER)
        IF (IER.EQ.0) CALL EZGET('LJ_HT_JETETA_MAX',HT_JETETA_MAX,IER)
        IF (IER.EQ.0) CALL EZGET('JETS_ETAMAX',ETAJTS_MAX,IER)
        IF (IER.EQ.0) CALL EZGET('JETS_PTMIN',PTJTS_MIN,IER)
        IF (IER.NE.0) CALL ERRMSG('Error getting RCP parameters',
     1    'TOP_LEPTONS_EVENT_SUMMRY',' ','F')
        CALL EZRSET
C
C *** Check consistency of HT selection and Jet preselection
C *** - reset limits if necessary
C
        IF(HT_JETPT_MIN.LT.PTJTS_MIN) HT_JETPT_MIN=PTJTS_MIN
        IF(HT_JETETA_MAX.GT.ETAJTS_MAX) HT_JETETA_MAX=ETAJTS_MAX
C
        FIRST=.FALSE.
      ENDIF
C
      I_EL=0
      I_MU=0
      I_PH=0
      I_JT=0
      CALL VFILL(EL_COR,5,1.)
      CALL VFILL(PH_COR,5,1.)
      EVT_DATE=-9999
      EVT_TIME=-9999
      NOEVT=-9999
      NORUN=-9999
      NO_ZTRAK=0
      RECO_VERS=11
      RECO_PASS=16
C
C *** Run,Event,Beam Crossing No info 
C
      LHEAD=GZHEAD()
      IF(LHEAD.NE.0) THEN
        EVT_DATE=IQ(LHEAD+4)
        EVT_TIME=IQ(LHEAD+5)
        NORUN=IQ(LHEAD+6)
        NOEVT_L1PT1=IQ(LHEAD+7)
        NOEVT_L1PT2=IQ(LHEAD+8)
        NOEVT=IQ(LHEAD+9)
      ENDIF
C
C *** Reco Production Version No etc.
C
      CALL RECO_VERSION(RECO_VERS,RECO_PASS)
C
C *** Trigger bits (L1,L2)
C
      CALL TOP_LEPTONS_UTIL_DECODE_TRIG(NO_TRIG_LVL1,TRIG_LVL1_BITS,
     1  NO_TRIG_LVL2,TRIG_LVL2_BITS,IER)
C
C *** Event and Processing Information
C
      WRITE(LUN,1000) NOEVT_LOCAL,NOEVT,NORUN,NOEVT_L1PT1,
     1      NOEVT_L1PT2,EVT_DATE,EVT_TIME,RECO_VERS,RECO_PASS
      IF(TOP_LEPTONS_UTIL_MONTECARLO()) THEN
        WRITE(LUN,1015)
      ELSE
        WRITE(LUN,1016)
      ENDIF
C
C *** Trigger bits and filters
C
      WRITE(LUN,1017) (TRIG_LVL1_BITS(IV),IV=1,NO_TRIG_LVL1)
      WRITE(LUN,1001) (TRIG_LVL2_BITS(IV),IV=1,NO_TRIG_LVL2)
C
C *** Main Ring Timing
C
      IER=-1
      CALL TOP_LEPTONS_UTIL_MAIN_RING(TIME29,MR_BITS,IER)
      IF(IER.EQ.0) THEN
        MR_PERMIT=JBIT(MR_BITS,5)
        IF(MR_PERMIT.EQ.1) THEN
          WRITE(LUN,1011) TIME29
        ELSE
          WRITE(LUN,1012) TIME29
        ENDIF
      ENDIF
C
C *** Micro blank
C
      IF(TOP_LEPTONS_UTIL_MICRO_BLANK()) THEN
        WRITE(LUN,1006)
      ELSE
        WRITE(LUN,1007)
      ENDIF
C
C *** MRBS Loss
C
      IF(TOP_LEPTONS_UTIL_MRBS_LOSS()) THEN
        WRITE(LUN,1008)
      ELSE
        WRITE(LUN,1009)
      ENDIF
C
C *** Multiple Interaction Flag
C
C      MI_FLAG=MULTIPLE_INTERACTION_TOOL()
C      IF(MI_FLAG.EQ.0) THEN
C        WRITE(LUN,1014)
C      ELSE
C        WRITE(LUN,1013) MI_FLAG
C      ENDIF
C
      WRITE(LUN,1005) NOMU,NOEL,NOPH,NOJT
      WRITE(LUN,1010) NOMU_UNCUT,NOEL_UNCUT,NOPH_UNCUT,NOJT_UNCUT
      IF(TOP_FLAG(1).GT.0) WRITE(LUN,1020) (TOP_FLAG(IV),IV=2,6)
      IF(W_FLAG(1).GT.0) WRITE(LUN,1030) (W_FLAG(IV),IV=2,3)
      IF(Z_FLAG(1).GT.0) WRITE(LUN,1040) (Z_FLAG(IV),IV=2,3)
      IF(WWG_FLAG(1).GT.0) WRITE(LUN,1050) (WWG_FLAG(IV),IV=2,3)
      IF(QCD_FLAG(1).GT.0) WRITE(LUN,1060) (QCD_FLAG(IV),IV=2,6)
      IF(WPAIR_FLAG(1).GT.0) WRITE(LUN,1070) (WPAIR_FLAG(IV),IV=2,6)
C
C *** Vertex Information
C
      CALL TOP_LEPTONS_UTIL_DECODE_VERTEX(NOVERT_P,NOVERT_S,NO_ZTRAK,
     1 NO_ZTRAK_VERTX_P,VERTX_P,IER)
C
      WRITE(LUN,1002) NOVERT_P,NOVERT_S,NO_ZTRAK
      IF(NOVERT_P.GT.0) THEN
        DO I=1,NOVERT_P
          WRITE(LUN,1003) I,(VERTX_P(IV,I),IV=1,3),NO_ZTRAK_VERTX_P(I)
        ENDDO
      ENDIF
C
C *** Muons
C
        LPMUO=GZPMUO(0)
        IF(LPMUO.GT.0) THEN
          DO WHILE (LPMUO.GT.0)
            IF(TOP_LEPTONS_GOOD_MUON(LPMUO)) THEN
              WRITE(LUN,1100)
              I_MU=I_MU+1
              IF(I_MU.GT.1) WRITE(LUN,1500)
              IF(I_MU.LT.6) LPMUO_VEC(I_MU)=LPMUO
              PMUO_VERS=IQ(LPMUO+1)
              ERR1=0.
              MUON_SIGN=1
              IF(IQ(LPMUO+2).GT.0) THEN
                MUON_SIGN=-1
              ENDIF
              VTX_Z=VERTX_P(3,1)
              CALL DET_ETA(VTX_Z,Q(LPMUO+15),TEMP)
              IF(Q(LPMUO+22).GT.0.) ERR1=SQRT(Q(LPMUO+22))
              WRITE(LUN,1101) MUON_SIGN,(Q(LPMUO+9+IV),IV=1,5),
     1           ERR1,Q(LPMUO+15),Q(LPMUO+17),Q(LPMUO+16),TEMP
              ITEMP=IQ(LPMUO+4)
              JTEMP=0
              IF(ITEMP.GT.9) THEN
                JTEMP=1
                ITEMP=ITEMP-10
              ENDIF
              IF(JTEMP.GT.0) THEN
                WRITE(LUN,1104) ITEMP
              ELSE
                WRITE(LUN,1103) ITEMP
              ENDIF
              WRITE(LUN,1105) IQ(LPMUO+3)
C
C *** Muon-Jet Isolation Information
C
              WRITE(LUN,1109) (Q(LPMUO+27+I),I=1,5)
              IF(NOJT.GE.1) THEN
                CALL TOP_LEPTONS_UTIL_NEARJET(Q(LPMUO+16),Q(LPMUO+17),
     1            DR_VEC12,LJETS_DR,DPHI_VEC12,LJETS_DPHI)
                DPHI_VEC12=DPHI_VEC12*CONV
                NEARJET_ET=Q(LJETS_DR+6)
                PHI_OPP=Q(LPMUO+17)+PI
                IF(PHI_OPP.GT.TWOPI) PHI_OPP=PHI_OPP-TWOPI
                TEMP=-10.
                CALL TOP_LEPTONS_UTIL_NEARJET(TEMP,PHI_OPP,TEMP,ITEMP,
     1            DPHI_MAX_VEC12,LJETS_DPHI)
                DPHI_MAX_VEC12=(PI-DPHI_MAX_VEC12)*CONV
                FARJET_ET=Q(LJETS_DPHI+6)
                WRITE(LUN,2101) DR_VEC12,DPHI_VEC12,NEARJET_ET,
     1            DPHI_MAX_VEC12,FARJET_ET
              ENDIF
C
              IF(LONG_DUMP) THEN
                IF(PMUO_VERS.GE.3) THEN
                  CALL TOP_LEPTONS_MUON_MTCA_DUMP(LPMUO,LUN)
                  CALL TOP_LEPTONS_LONG_MUON_DUMP(LPMUO,LUN)
                ELSE
                  CALL TOP_LEPTONS_LONG_MUON_DUMP_OLD(LPMUO,LUN)
                ENDIF
              ENDIF
            ENDIF
            LPMUO=LQ(LPMUO)
          ENDDO
          IF(I_MU.GT.5) I_MU=5
        ENDIF
C
C *** Electrons
C
        LPELC=GZPELC()
        IF(LPELC.GT.0) THEN
          I_EL_INF=0
          DO WHILE (LPELC.GT.0)
            IF(TOP_LEPTONS_GOOD_ELECTRON(LPELC)) THEN
              WRITE(LUN,1110)
              I_EL=I_EL+1
              IF(I_EL.GT.1) WRITE(LUN,1500)
              IF(I_EL.LT.6) LPELC_VEC(I_EL)=LPELC
              TEMP=Q(LPELC+19)/10.
              CALL UCOPY(Q(LPELC+3),VEC1(1),4)
              TEMP3=Q(LPELC+7)
              TEMP4=Q(LPELC+13)
C
C *** Calculate scale corrections - if requested
C
              IF(CORR_EM) THEN
                TEMP2=TOP_LEPTONS_EM_CORRECTION(LPELC)
                IF(I_EL.LT.6) EL_COR(I_EL)=TEMP2
                CALL VSCALE(VEC1(1),TEMP2,VEC1(1),4)
                TEMP3=TEMP2*Q(LPELC+7)
                TEMP4=TEMP4*TEMP2
              ENDIF
              WRITE(LUN,1111) (Q(LPELC+2+IV),IV=1,4),(VEC1(IV),IV=1,4),
     1          Q(LPELC+7),TEMP3,TEMP4,Q(LPELC+8),Q(LPELC+10),
     2          Q(LPELC+9),TEMP
C
C *** Electron-Jet Isolation Information
C
              IF(NOJT.GE.1) THEN
                CALL TOP_LEPTONS_UTIL_NEARJET(Q(LPELC+9),Q(LPELC+10),
     1            DR_VEC12,LJETS_DR,DPHI_VEC12,LJETS_DPHI)
                DPHI_VEC12=DPHI_VEC12*CONV
                NEARJET_ET=Q(LJETS_DR+6)
                PHI_OPP=Q(LPELC+10)+PI
                IF(PHI_OPP.GT.TWOPI) PHI_OPP=PHI_OPP-TWOPI
                TEMP=-10.
                CALL TOP_LEPTONS_UTIL_NEARJET(TEMP,PHI_OPP,TEMP,ITEMP,
     1            DPHI_MAX_VEC12,LJETS_DPHI)
                DPHI_MAX_VEC12=(PI-DPHI_MAX_VEC12)*CONV
                FARJET_ET=Q(LJETS_DPHI+6)
                WRITE(LUN,2101) DR_VEC12,DPHI_VEC12,NEARJET_ET,
     1            DPHI_MAX_VEC12,FARJET_ET
              ENDIF
              IF(LONG_DUMP) THEN
C
C *** Long electron dump
C
                CALL TOP_LEPTONS_LONG_ELECTRON_DUMP(LUN,LPELC)
              ENDIF  
            ENDIF
            LPELC=LQ(LPELC)
          ENDDO
          IF(I_EL.GT.5) I_EL=5
        ENDIF
C
C *** Photons
C
        LPPHO=GZPPHO()
        IF(LPPHO.GT.0) THEN
          DO WHILE (LPPHO.GT.0)
            IF(TOP_LEPTONS_GOOD_PHOTON(LPPHO)) THEN
              WRITE(LUN,1120)
              I_PH=I_PH+1
              IF(I_PH.GT.1) WRITE(LUN,1500)
              IF(I_PH.LT.6) LPPHO_VEC(I_PH)=LPPHO
              TEMP=Q(LPPHO+19)/10.
              CALL UCOPY(Q(LPPHO+3),VEC1(1),4)
              TEMP3=Q(LPPHO+7)
              TEMP4=Q(LPPHO+13)
C
C *** Calculate scale corrections - if requested
C
              IF(CORR_EM) THEN
                TEMP2=TOP_LEPTONS_EM_CORRECTION(LPPHO)
                IF(I_PH.LT.6) PH_COR(I_PH)=TEMP2
                CALL VSCALE(VEC1(1),TEMP2,VEC1(1),4)
                TEMP3=TEMP2*Q(LPPHO+7)
                TEMP4=TEMP4*TEMP2
              ENDIF
              WRITE(LUN,1121) (Q(LPPHO+2+IV),IV=1,4),(VEC1(IV),IV=1,4),
     1          Q(LPPHO+7),TEMP3,TEMP4,Q(LPPHO+8),Q(LPPHO+10),
     2          Q(LPPHO+9),TEMP
C
C *** Photon-Jet Isolation Information
C
              IF(NOJT.GE.1) THEN
                CALL TOP_LEPTONS_UTIL_NEARJET(Q(LPPHO+9),Q(LPPHO+10),
     1            DR_VEC12,LJETS_DR,DPHI_VEC12,LJETS_DPHI)
                DPHI_VEC12=DPHI_VEC12*CONV
                NEARJET_ET=Q(LJETS_DR+6)
                PHI_OPP=Q(LPPHO+10)+PI
                IF(PHI_OPP.GT.TWOPI) PHI_OPP=PHI_OPP-TWOPI
                TEMP=-10.
                CALL TOP_LEPTONS_UTIL_NEARJET(TEMP,PHI_OPP,TEMP,ITEMP,
     1            DPHI_MAX_VEC12,LJETS_DPHI)
                DPHI_MAX_VEC12=(PI-DPHI_MAX_VEC12)*CONV
                FARJET_ET=Q(LJETS_DPHI+6)
                WRITE(LUN,2101) DR_VEC12,DPHI_VEC12,NEARJET_ET,
     1            DPHI_MAX_VEC12,FARJET_ET
              ENDIF
              IF(LONG_DUMP) THEN
C
C *** Long Photon Dump
C
                CALL TOP_LEPTONS_LONG_PHOTON_DUMP(LUN,LPPHO)
              ENDIF
            ENDIF
            LPPHO=LQ(LPPHO)
          ENDDO
          IF(I_PH.GT.5) I_PH=5
        ENDIF
C
C *** Jets
C
        HT = 0.
        H = 0.
        LJETS=GZJETS()
        IF(LJETS.GT.0) THEN
          IF(JET_ALG.EQ.1) THEN
            WRITE(LUN,1130)
          ELSEIF(JET_ALG.EQ.2) THEN
            WRITE(LUN,1136)
          ELSEIF(JET_ALG.EQ.3) THEN
            WRITE(LUN,1137)
          ELSEIF(JET_ALG.EQ.4) THEN
            WRITE(LUN,1138)
          ELSE
            WRITE(LUN,1130)
          ENDIF
C
          DO WHILE (LJETS.GT.0)
            IF(TOP_LEPTONS_GOOD_JET(LJETS)) THEN
              I_JT=I_JT+1
              IF(I_JT.GT.1) WRITE(LUN,1500)
              IF(I_JT.LT.11) LJETS_VEC(I_JT)=LJETS
              CALL DET_ETA(VTX_Z,Q(LJETS+7),TEMP)
              WRITE(LUN,1131) (Q(LJETS+1+IV),IV=1,8),TEMP
              IF(Q(LPMUO+22).GT.0.) ERR1=SQRT(Q(LPMUO+22))
              IF(CORR_JETS) THEN
                CALL TOP_LEPTONS_CORR_JETPARM(LJETS,E,ET,PX,PY,PZ,
     1            PHI,ETA,IER)
                IF(IER.GE.0) WRITE(LUN,1132) PX,PY,PZ,E,ET,
     1            Q(LJETS+7),PHI,ETA,TEMP
                IF((ET.GE.HT_JETPT_MIN).AND.
     1             (ABS(ETA).LE.HT_JETETA_MAX))THEN
                  HT = HT + ET
                  H  = H + E
                ENDIF
              ELSE
                IF((Q(LJETS+6).GE.HT_JETPT_MIN).AND.
     1             (ABS(Q(LJETS+9)).LE.HT_JETETA_MAX))THEN
                  HT =  HT + Q(LJETS+6)
                  H  =  H + Q(LJETS+5)
                ENDIF
              ENDIF
              IF(LONG_DUMP) THEN
C
C *** Long Jets Dump
C
                CALL TOP_LEPTONS_LONG_JET_DUMP(LUN,LJETS)
              ENDIF
            ENDIF
            LJETS=LQ(LJETS)
          ENDDO
          IF(I_JT.GT.10) I_JT=10
        ENDIF
C
C *** Missing Et
C
      LPNUT1=GZPNUT(1)
      IF(LPNUT1.GT.0) THEN
        CALL VADD(Q(LPNUT1+3),MET_VEC(1),MISET_TEMP(1),3)
        MISET_TEMP(4)=SQRT( MISET_TEMP(1)**2+MISET_TEMP(2)**2+
     1    MISET_TEMP(3)**2 )
        CALL TOP_LEPTONS_UTIL_LAB_ANGLES(MISET_TEMP,MISET_THETA,
     1    MISET_PHI)
        TEMP=SQRT(MISET_TEMP(1)**2+MISET_TEMP(2)**2)
        WRITE(LUN,1142) (Q(LPNUT1+2+IV),IV=1,4), 
     1    (MISET_TEMP(IV),IV=1,4),Q(LPNUT1+7),TEMP,
     2    Q(LPNUT1+13),Q(LPNUT1+8),MISET_THETA,Q(LPNUT1+10),
     3    MISET_PHI,Q(LPNUT1+9)
        IF(LONG_DUMP) WRITE(LUN,1141) Q(LPNUT1+14)
      ENDIF
C
      LPNUT2=GZPNUT(2)
      IF(LPNUT2.GT.0) THEN
        CALL VADD(Q(LPNUT2+3),MET_VEC(1),MISET_TEMP(1),3)
        MISET_TEMP(4)=SQRT( MISET_TEMP(1)**2+MISET_TEMP(2)**2+
     1    MISET_TEMP(3)**2 )
        CALL TOP_LEPTONS_UTIL_LAB_ANGLES(MISET_TEMP,MISET_THETA,
     1    MISET_PHI)
        TEMP=SQRT(MISET_TEMP(1)**2+MISET_TEMP(2)**2)
        WRITE(LUN,1140) (Q(LPNUT2+2+IV),IV=1,4),
     1    (MISET_TEMP(IV),IV=1,4),Q(LPNUT2+7),TEMP,
     2    Q(LPNUT2+13),Q(LPNUT2+8),MISET_THETA,Q(LPNUT2+10),
     3    MISET_PHI,Q(LPNUT2+9)
        IF(LONG_DUMP) WRITE(LUN,1141) Q(LPNUT2+14)
      ENDIF
C
      LPNUT4=GZPNUT(4)
      IF(LPNUT4.GT.0) THEN
        CALL VADD(Q(LPNUT4+3),MET_VEC(1),MISET_TEMP(1),3)
        MISET_TEMP(4)=SQRT( MISET_TEMP(1)**2+MISET_TEMP(2)**2+
     1    MISET_TEMP(3)**2 )
        CALL TOP_LEPTONS_UTIL_LAB_ANGLES(MISET_TEMP,MISET_THETA,
     1    MISET_PHI)
        TEMP=SQRT(MISET_TEMP(1)**2+MISET_TEMP(2)**2)
        WRITE(LUN,1143) (Q(LPNUT4+2+IV),IV=1,4),
     1    (MISET_TEMP(IV),IV=1,4),Q(LPNUT4+7),TEMP,
     2    Q(LPNUT4+13),Q(LPNUT4+8),MISET_THETA,Q(LPNUT4+10),
     3    MISET_PHI,Q(LPNUT4+9)
        IF(LONG_DUMP) WRITE(LUN,1141) Q(LPNUT4+14)
      ENDIF
C
      LPNUT3=GZPNUT(3)
      IF(LPNUT3.GT.0) THEN
        WRITE(LUN,1145) (Q(LPNUT3+2+IV),IV=1,4),Q(LPNUT3+7),
     1    Q(LPNUT3+13),Q(LPNUT3+8),Q(LPNUT3+10),Q(LPNUT3+9)
        IF(LONG_DUMP) WRITE(LUN,1141) Q(LPNUT3+14)
      ENDIF
C
C *** corellations
C *** a.) Start with muons
C
        WRITE(LUN,1200)
        IF(I_MU.GE.2) THEN
          DPHI_LEP12=ABS( Q(LPMUO_VEC(1)+17)-Q(LPMUO_VEC(2)+17) )
          IF(DPHI_LEP12.GT.PI) DPHI_LEP12=TWOPI-DPHI_LEP12
          DPHI_LEP12=DPHI_LEP12*CONV
          CALL UCOPY (Q(LPMUO_VEC(1)+10),DILEP_VEC(1),4)
          CALL VADD(Q(LPMUO_VEC(2)+10),DILEP_VEC(1),DILEP_VEC(1),4)
          TWO_BODY_MASS=TOP_LEPTONS_UTIL_MASS4(DILEP_VEC)
          XZ=DILEP_VEC(3)/PBEAM
          PTZ1=SQRT(DILEP_VEC(1)**2+DILEP_VEC(2)**2)
          PTZ2=SQRT(Q(LPNUT2+3)**2+Q(LPNUT2+4)**2)
          TWO_BODY_COPLAN=TOP_LEPTONS_UTIL_COPLAN(Q(LPMUO_VEC(1)+10),
     1      Q(LPMUO_VEC(2)+10))
          WRITE(LUN,1201) DPHI_LEP12,TWO_BODY_COPLAN,
     1      TWO_BODY_MASS,XZ,PTZ1,PTZ2
        ENDIF
C
C *** b.) Electrons
C
        IF(I_EL.GE.2) THEN
          DPHI_LEP12=ABS( Q(LPELC_VEC(1)+10)-Q(LPELC_VEC(2)+10) )
          IF(DPHI_LEP12.GT.PI) DPHI_LEP12=TWOPI-DPHI_LEP12
          DPHI_LEP12=DPHI_LEP12*CONV
          CALL VSCALE(Q(LPELC_VEC(1)+3),EL_COR(1),VEC1(1),4)
          CALL VSCALE(Q(LPELC_VEC(2)+3),EL_COR(2),VEC2(1),4)
          CALL UCOPY(VEC1(1),DILEP_VEC(1),4)
          CALL VADD(VEC2(1),DILEP_VEC(1),DILEP_VEC(1),4)
          XZ=DILEP_VEC(3)/PBEAM
          PTZ1=SQRT(DILEP_VEC(1)**2+DILEP_VEC(2)**2)
          TWO_BODY_MASS=TOP_LEPTONS_UTIL_MASS4(DILEP_VEC)
          TWO_BODY_COPLAN=TOP_LEPTONS_UTIL_COPLAN(VEC1(1),
     1      VEC2(1))
          WRITE(LUN,1202) DPHI_LEP12,TWO_BODY_COPLAN,
     1      TWO_BODY_MASS,XZ,PTZ1
          CALL VADD(DILEP_VEC(1),Q(LPNUT2+3),VEC2(1),2)
          MT_CLUSTER=(SQRT(TWO_BODY_MASS**2+VMOD(DILEP_VEC,2)**2)
     1      + VMOD(Q(LPNUT2+3),2) )**2 - VMOD(VEC2,2)**2
          IF(MT_CLUSTER.GT.0.) THEN
            MT_CLUSTER=SQRT(MT_CLUSTER)
          ELSE
            MT_CLUSTER=0.0
          ENDIF
          WRITE(LUN,1210) MT_CLUSTER
        ENDIF
C
C *** c.) Photons
C
        IF(I_PH.GE.2) THEN
          DPHI_LEP12=ABS( Q(LPPHO_VEC(1)+10)-Q(LPPHO_VEC(2)+10) )
          IF(DPHI_LEP12.GT.PI) DPHI_LEP12=TWOPI-DPHI_LEP12
          DPHI_LEP12=DPHI_LEP12*CONV
          CALL VSCALE(Q(LPPHO_VEC(1)+3),PH_COR(1),VEC1(1),4)
          CALL VSCALE(Q(LPPHO_VEC(2)+3),PH_COR(2),VEC2(1),4)
          CALL UCOPY(VEC1(1),DILEP_VEC(1),4)
          CALL VADD(VEC2(1),DILEP_VEC(1),DILEP_VEC(1),4)
          XZ=DILEP_VEC(3)/PBEAM
          PTZ1=SQRT(DILEP_VEC(1)**2+DILEP_VEC(2)**2)
          TWO_BODY_MASS=TOP_LEPTONS_UTIL_MASS4(DILEP_VEC)
          TWO_BODY_COPLAN=TOP_LEPTONS_UTIL_COPLAN(VEC1(1),
     1      VEC2(1))
          WRITE(LUN,1203) DPHI_LEP12,TWO_BODY_COPLAN,
     1      TWO_BODY_MASS,XZ,PTZ1
          CALL VADD(DILEP_VEC(1),Q(LPNUT2+3),VEC2(1),2)
          MT_CLUSTER=(SQRT(TWO_BODY_MASS**2+VMOD(DILEP_VEC,2)**2)
     1      + VMOD(Q(LPNUT2+3),2) )**2 - VMOD(VEC2,2)**2
          IF(MT_CLUSTER.GT.0.) THEN
            MT_CLUSTER=SQRT(MT_CLUSTER)
          ELSE
            MT_CLUSTER=0.0
          ENDIF
          WRITE(LUN,1211) MT_CLUSTER
        ENDIF
C
C *** d.) muon-electron events
C ***     --- take muon1 + electron1
C
        IF(I_MU.GT.0.AND.I_EL.GT.0) THEN
          DPHI_LEP12=ABS( Q(LPMUO_VEC(1)+17)-Q(LPELC_VEC(1)+10) )
          IF(DPHI_LEP12.GT.PI) DPHI_LEP12=TWOPI-DPHI_LEP12
          DPHI_LEP12=DPHI_LEP12*CONV
          CALL UCOPY (Q(LPMUO_VEC(1)+10),DILEP_VEC(1),4)
          CALL VSCALE(Q(LPELC_VEC(1)+3),EL_COR(1),VEC1(1),4)
          CALL VADD(VEC1(1),DILEP_VEC(1),DILEP_VEC(1),4)
          TWO_BODY_MASS=TOP_LEPTONS_UTIL_MASS4(DILEP_VEC)
          TWO_BODY_COPLAN=TOP_LEPTONS_UTIL_COPLAN(Q(LPMUO_VEC(1)+10),
     1      VEC1(1))
          WRITE(LUN,1204) DPHI_LEP12,TWO_BODY_COPLAN,
     1      TWO_BODY_MASS        
          DETA_VEC12=ABS( Q(LPMUO_VEC(1)+16)-Q(LPELC_VEC(1)+9) )
          DPHI_LEP12=DPHI_LEP12/CONV
          DR_VEC12=TOP_LEPTONS_UTIL_CALC_DR(DETA_VEC12,DPHI_LEP12)  
          CALL VADD(DILEP_VEC(1),Q(LPNUT3+3),VEC2(1),2)
          MT_CLUSTER=( SQRT(TWO_BODY_MASS**2+VMOD(DILEP_VEC,2)**2)
     1      + VMOD(Q(LPNUT3+3),2) )**2 - VMOD(VEC2,2)**2 
          IF(MT_CLUSTER.GT.0.) THEN
            MT_CLUSTER=SQRT(MT_CLUSTER)
          ELSE
            MT_CLUSTER=0.0
          ENDIF
          WRITE(LUN,1207) DR_VEC12,MT_CLUSTER
        ENDIF
C
C *** e.) muon-gamma events
C ***     --- take muon1 + gamma1
C
        IF(I_MU.GT.0.AND.I_PH.GT.0) THEN
          DPHI_LEP12=ABS( Q(LPMUO_VEC(1)+17)-Q(LPPHO_VEC(1)+10) )
          IF(DPHI_LEP12.GT.PI) DPHI_LEP12=TWOPI-DPHI_LEP12
          DPHI_LEP12=DPHI_LEP12*CONV
          CALL UCOPY (Q(LPMUO_VEC(1)+10),DILEP_VEC(1),4)
          CALL VSCALE(Q(LPPHO_VEC(1)+3),PH_COR(1),VEC1(1),4)
          CALL VADD(VEC1(1),DILEP_VEC(1),DILEP_VEC(1),4)
          TWO_BODY_MASS=TOP_LEPTONS_UTIL_MASS4(DILEP_VEC)
          TWO_BODY_COPLAN=TOP_LEPTONS_UTIL_COPLAN(Q(LPMUO_VEC(1)+10),
     1      VEC1(1))
          WRITE(LUN,1205) DPHI_LEP12,TWO_BODY_COPLAN,
     1      TWO_BODY_MASS          
          DETA_VEC12=ABS( Q(LPMUO_VEC(1)+16)-Q(LPPHO_VEC(1)+9) )
          DPHI_LEP12=DPHI_LEP12/CONV
          DR_VEC12=TOP_LEPTONS_UTIL_CALC_DR(DETA_VEC12,DPHI_LEP12)  
          CALL VADD(DILEP_VEC(1),Q(LPNUT3+3),VEC2(1),2)
          MT_CLUSTER=( SQRT(TWO_BODY_MASS**2+VMOD(DILEP_VEC,2)**2)
     1      + VMOD(Q(LPNUT3+3),2) )**2 - VMOD(VEC2,2)**2 
          IF(MT_CLUSTER.GT.0.) THEN
            MT_CLUSTER=SQRT(MT_CLUSTER)
          ELSE
            MT_CLUSTER=0.0
          ENDIF
          WRITE(LUN,1208) DR_VEC12,MT_CLUSTER
        ENDIF
C
C *** f.) Electron-gamma events
C ***     --- take elec1 + gamma1
C
        IF(I_EL.GT.0.and.I_PH.GT.0) THEN
          DPHI_LEP12=ABS( Q(LPELC_VEC(1)+10)-Q(LPPHO_VEC(1)+10) )
          IF(DPHI_LEP12.GT.PI) DPHI_LEP12=TWOPI-DPHI_LEP12
          DPHI_LEP12=DPHI_LEP12*CONV
          CALL VSCALE(Q(LPELC_VEC(1)+3),EL_COR(1),VEC1(1),4)
          CALL VSCALE(Q(LPPHO_VEC(1)+3),PH_COR(1),VEC2(1),4)
          CALL UCOPY(VEC1(1),DILEP_VEC(1),4)
          CALL VADD(VEC2(1),DILEP_VEC(1),DILEP_VEC(1),4)
          XZ=DILEP_VEC(3)/PBEAM
          PTZ1=SQRT(DILEP_VEC(1)**2+DILEP_VEC(2)**2)
          TWO_BODY_MASS=TOP_LEPTONS_UTIL_MASS4(DILEP_VEC)
          TWO_BODY_COPLAN=TOP_LEPTONS_UTIL_COPLAN(VEC1(1),
     1      VEC2(1))
          WRITE(LUN,1206) DPHI_LEP12,TWO_BODY_COPLAN,
     1      TWO_BODY_MASS,XZ,PTZ1
          DETA_VEC12=ABS( Q(LPELC_VEC(1)+9)-Q(LPPHO_VEC(1)+9) )
          DPHI_LEP12=DPHI_LEP12/CONV
          DR_VEC12=TOP_LEPTONS_UTIL_CALC_DR(DETA_VEC12,DPHI_LEP12)  
          IF(I_MU.GT.0) THEN
            CALL VADD(DILEP_VEC(1),Q(LPNUT3+3),VEC2(1),2)
            MT_CLUSTER=(SQRT(TWO_BODY_MASS**2+VMOD(DILEP_VEC,2)**2)
     1        + VMOD(Q(LPNUT3+3),2) )**2 - VMOD(VEC2,2)**2 
            IF(MT_CLUSTER.GT.0.) THEN
              MT_CLUSTER=SQRT(MT_CLUSTER)
            ELSE
              MT_CLUSTER=0.0
            ENDIF
          ELSE
            IF(LPNUT4.GT.0) THEN
              CALL VADD(DILEP_VEC(1),Q(LPNUT4+3),VEC2(1),2)
              CALL VADD(VEC2(1),MET_VEC(1),VEC2(1),2)
              MT_CLUSTER=(SQRT(TWO_BODY_MASS**2+VMOD(DILEP_VEC,2)**2)
     1          + VMOD(Q(LPNUT4+3),2) )**2 - VMOD(VEC2,2)**2 
              IF(MT_CLUSTER.GT.0.) THEN
                MT_CLUSTER=SQRT(MT_CLUSTER)
              ELSE
                MT_CLUSTER=0.0
              ENDIF
            ELSE
              CALL VADD(DILEP_VEC(1),Q(LPNUT2+3),VEC2(1),2)
              CALL VADD(VEC2(1),MET_VEC(1),VEC2(1),2)
              MT_CLUSTER=(SQRT(TWO_BODY_MASS**2+VMOD(DILEP_VEC,2)**2)
     1          + VMOD(Q(LPNUT2+3),2) )**2 - VMOD(VEC2,2)**2 
              IF(MT_CLUSTER.GT.0.) THEN
                MT_CLUSTER=SQRT(MT_CLUSTER)
              ELSE
                MT_CLUSTER=0.0
              ENDIF
            ENDIF
          ENDIF
          WRITE(LUN,1209) DR_VEC12,MT_CLUSTER
        ENDIF
C
C *** g.) 2-jet correlations
C
      I_JT_MAXPERM = I_JT
      IF (I_JT_MAXPERM .GT. 6) I_JT_MAXPERM = 6
      DO I=1,I_JT_MAXPERM-1
        DO J=I+1,I_JT_MAXPERM
          IF(CORR_JETS) THEN
              CALL TOP_LEPTONS_CORR_JETPARM(LJETS_VEC(I),E,ET,
     1          PX,PY,PZ,PHI,ETA,IER)
              CALL TOP_LEPTONS_CORR_JETPARM(LJETS_VEC(J),E2,ET2,
     1          PX2,PY2,PZ2,PHI2,ETA2,IER)
            DPHI_JET12=ABS(PHI-PHI2)
            DO IV=1,4
              DILEP_VEC(1)=PX+PX2
              DILEP_VEC(2)=PY+PY2
              DILEP_VEC(3)=PZ+PZ2
              DILEP_VEC(4)=E+E2
            ENDDO
          ELSE
            DPHI_JET12=ABS( Q(LJETS_VEC(I)+7)-Q(LJETS_VEC(J)+7) )
            CALL UCOPY (Q(LJETS_VEC(I)+2),DILEP_VEC(1),4)
            CALL VADD(Q(LJETS_VEC(J)+2),DILEP_VEC(1),DILEP_VEC(1),4)
          ENDIF
          IF(DPHI_JET12.GT.PI) DPHI_JET12=TWOPI-DPHI_JET12
          DPHI_JET12=DPHI_JET12*CONV
          TWO_BODY_MASS=TOP_LEPTONS_UTIL_MASS4(DILEP_VEC)
          WRITE(LUN,1135)I,J,DPHI_JET12,I,J,TWO_BODY_MASS
        ENDDO
      ENDDO
C
C *** h.) lepton-neutrino 2-body correlations
C ***
C ***    i.) electron-missing energy system
C
      IF(I_EL.GT.0.AND.LPNUT2.GT.0) THEN
        PT1=EL_COR(1)*Q(LPELC_VEC(1)+7)
        IF(LPNUT3.GT.0) THEN
          DPHI_LEP12=ABS(Q(LPELC_VEC(1)+10)-Q(LPNUT3+10))
          IF(DPHI_LEP12.GT.PI) DPHI_LEP12=TWOPI-DPHI_LEP12
          PT2=Q(LPNUT3+7)
        ELSE
          IF(LPNUT4.GT.0) THEN
            DPHI_LEP12=ABS(Q(LPELC_VEC(1)+10)-Q(LPNUT4+10))
            IF(DPHI_LEP12.GT.PI) DPHI_LEP12=TWOPI-DPHI_LEP12
            PT2= (Q(LPNUT4+3)+MET_VEC(1))**2 +
     1        (Q(LPNUT4+4)+MET_VEC(2))**2
            PT2=SQRT(PT2)
          ELSE
            DPHI_LEP12=ABS(Q(LPELC_VEC(1)+10)-Q(LPNUT2+10))
            IF(DPHI_LEP12.GT.PI) DPHI_LEP12=TWOPI-DPHI_LEP12
            PT2= (Q(LPNUT2+3)+MET_VEC(1))**2 +
     1        (Q(LPNUT2+4)+MET_VEC(2))**2
            PT2=SQRT(PT2)
          ENDIF
        ENDIF
        TWO_BODY_MASST=TOP_LEPTONS_UTIL_MASST(PT1,PT2,DPHI_LEP12)
        DPHI_LEP12=DPHI_LEP12*CONV
        WRITE(LUN,1151) DPHI_LEP12,TWO_BODY_MASST
        ITEMP=TOP_FLAG(5)+TOP_FLAG(2)+W_FLAG(2)+WWG_FLAG(2)
     1    +QCD_FLAG(2)+QCD_FLAG(5)+WPAIR_FLAG(2)+WPAIR_FLAG(5)
        IF(ITEMP.GT.0) THEN
          DO I=1,4
            LEP4VEC(I) = EL_COR(1)*Q(LPELC_VEC(1)+2+I)
            W4VEC1(I) = 0.
            W4VEC2(I) = 0.
          ENDDO
          IF(LPNUT4.GT.0) THEN
            NU2VEC(1) = Q(LPNUT4+3)+MET_VEC(1)
            NU2VEC(2) = Q(LPNUT4+4)+MET_VEC(2)
          ELSE
            NU2VEC(1) = Q(LPNUT2+3)+MET_VEC(1)
            NU2VEC(2) = Q(LPNUT2+4)+MET_VEC(2)
          ENDIF
          CALL FIND_WLNU2(WMASS,LEP4VEC,NU2VEC,W4VEC1,W4VEC2,OK)
          IF(OK) THEN
            PTW=SQRT(W4VEC1(1)**2+W4VEC1(2)**2)
            XW1=W4VEC1(3)/PBEAM
            XW2=W4VEC2(3)/PBEAM
            WRITE(LUN,1153) (W4VEC1(I),I=1,4),PTW,XW1
            WRITE(LUN,1154) (W4VEC2(I),I=1,4),PTW,XW2
          ELSE
            PTW=SQRT(W4VEC1(1)**2+W4VEC1(2)**2)
            WRITE(LUN,1155) PTW
          ENDIF
        ENDIF
      ENDIF
C
C ***    ii.) muon - missing energy system
C
      IF(I_MU.GT.0.AND.LPNUT3.GT.0) THEN
        DPHI_LEP12=ABS(Q(LPMUO_VEC(1)+17)-Q(LPNUT3+10))
        IF(DPHI_LEP12.GT.PI) DPHI_LEP12=TWOPI-DPHI_LEP12
        PT1=Q(LPMUO_VEC(1)+14)
        PT2=Q(LPNUT3+7)
        TWO_BODY_MASST=TOP_LEPTONS_UTIL_MASST(PT1,PT2,DPHI_LEP12)
        DPHI_LEP12=DPHI_LEP12*CONV
        WRITE(LUN,1152) DPHI_LEP12,TWO_BODY_MASST
        ITEMP=TOP_FLAG(2)+TOP_FLAG(6)+W_FLAG(3)+WWG_FLAG(3)
     1    +QCD_FLAG(2)+QCD_FLAG(6)+WPAIR_FLAG(2)+WPAIR_FLAG(6)
        IF(ITEMP.GT.0) THEN
          DO I=1,4
            LEP4VEC(I) = Q(LPMUO_VEC(1)+9+I)
            W4VEC1(I) = 0.
            W4VEC2(I) = 0.
          ENDDO
          NU2VEC(1) = Q(LPNUT3+3)
          NU2VEC(2) = Q(LPNUT3+4)
          CALL FIND_WLNU2(WMASS,LEP4VEC,NU2VEC,W4VEC1,W4VEC2,OK)
          IF(OK) THEN
            PTW=SQRT(W4VEC1(1)**2+W4VEC1(2)**2)
            XW1=W4VEC1(3)/PBEAM
            XW2=W4VEC2(3)/PBEAM
            WRITE(LUN,1156) (W4VEC1(I),I=1,4),PTW,XW1
            WRITE(LUN,1157) (W4VEC2(I),I=1,4),PTW,XW2
          ELSE
            PTW=SQRT(W4VEC1(1)**2+W4VEC1(2)**2)
            WRITE(LUN,1158) PTW
          ENDIF
        ENDIF
      ENDIF
C
C *** j.) Event shape quantities
C *** Jets
C
      WRITE(LUN,1215)HT,H
      IF (I_JT.GE.3.AND.IER.GT.0) THEN
        CALL TOP_LEPTONS_EVENT_SHAPE(I_JT,LJETS_VEC,SPHER,PLAN,GSPHER,
     &    GAPLAN,GY,EMAXSH,ETMAXSH,EFOUR_SHAPE,IER)
        IF(I_JT.EQ.3) THEN
          WRITE(LUN,1220)SPHER,PLAN,GSPHER,GAPLAN,GY
        ELSEIF(I_JT.GE.4) THEN
          WRITE(LUN,1225)SPHER,PLAN,GSPHER,GAPLAN,GY,EMAXSH,ETMAXSH,
     &      EFOUR_SHAPE
        ENDIF
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
C
 1000 FORMAT(////,70(1H-),//,
     1 10X,' Event dump for local event no. = ',I8,//,
     2 5X,' Event No   = ',I8,'  Run No   = ',I8,/,
     3 5X,' Level 1 No = ',2I10,/,
     4 5X,' Date / Time taken = ',2I12,/,
     5 5X,' Reco Version = ',I5,' Pass = ',I5)
 1001 FORMAT(5X,' Level 2 bits - ',16(I2,2X),/21X,16(I2,2X),
     6 /21X,16(I1,2X),/21X,16(I1,2X),/)
 1002 FORMAT(/,10X,' Vertex Dump  :',//,
     1 5X,' No Vertices - Primary / Secondary = ',I2,2X,I2,
     2 /5X,' Total Ztrak Multiplicity = ',I3)
 1003 FORMAT(10X,' Vertex No. ',I2,2X,' x,y,z = ',3F6.1,
     1 2X,' No. tracks = ',I3)
 1005 FORMAT(/,10X,' Event selection found :',//,
     1 5X,I5,' Muons, ',I5,' Electrons, ',I5,' Photons, ',
     2 I5,' Jets',/)
 1006 FORMAT(5X,' Micro Blank Flag : set ')
 1007 FORMAT(5X,' Micro Blank Flag : not set ')
 1008 FORMAT(5X,' MRBS Loss Flag : set ')
 1009 FORMAT(5X,' MRBS Loss Flag : not set ')
 1011 FORMAT(5X,' Main Ring Timing : Time29 = ',F6.3,' MR_PERMIT set')
 1012 FORMAT(5X,' Main Ring Timing : Time29 = ',F6.3,
     1 ' MR_PERMIT not set')
 1013 FORMAT(5X,' Multiple Interaction Flag = ',I3,
     1' 1->4 for single->double interaction ')
 1014 FORMAT(5X,' Multiple Interaction Tool = 0 -> no vertex or',
     1 ' tool failure ')
 1015 FORMAT(5X,' Data Type : MonteCarlo ',/)
 1016 FORMAT(5X,' Data Type : Collider Beam Data ',/)
 1017 FORMAT(10X,' Trigger Information :',//,
     1  5X,' Level 1 bits - ',16(I2,2X),/21X,16(I2,2X)) 
 1010 FORMAT(10X,' Raw ZEBRA Bank Counts (before selection) :',//,
     1 5X,I5,' PMUO, ',I5,' PELC, ',I5,' PPHO, ',I5,' JETS')
 1020 FORMAT(/,5X,' Event found as ttbar Candidate   : ',5(I2,2X),
     1 /25X,' (e-mu,e-e,mu-mu,e-jet,mu-jet)')
 1030 FORMAT(/,5X,' Event found as W Candidate       : ',2(I2,2X),
     1 ' e-nu, mu-nu ')
 1040 FORMAT(/,5X,' Event found as Z0 Candidate      : ',2(I2,2X),
     1 ' e-e, mu-mu ')
 1050 FORMAT(/,5X,' Event found as Wgamma Candidate  : ',2(I2,2X),
     1 ' e-gamma-nu, mu-gamma-nu ')
 1060 FORMAT(/,5X,' Event found as QCD  Candidate    : ',5(I2,2X),
     1 /,25X,' (e-mu,e-e,mu-mu,e-jet,mu-jet)')
 1070 FORMAT(/,5X,' Event found as WPair  Candidate  : ',5(I2,2X),
     1 /,25X,' (e-mu,e-e,mu-mu,e-jet,mu-jet)')
C
 1100 FORMAT(/,10X,' Muon Dump (PMUO) :',/)
 1101 FORMAT(5X,' Charge = ',I3,4X,' 4-Vector (Px,Py,Pz,P) = ',4F8.1,/,
     1 5X,' Pt = ',F8.1,' +/- ',F8.1,
     2 /5X,' Theta = ',F6.2,' Phi = ',F6.2,' Eta = ',F6.2,
     3 ' Ieta = ',F6.2)
 1102 FORMAT(5X,' Predicted dEdx in Calorimeter / Muon Iron = ',2F8.1,
     1 /5X,' Calorimeter Deposition (Cone 0.2,0.4,0.6) = ',3F8.1)
 1103 FORMAT(5X,' Momentum flag = ',I3,' (Muon only, global, other)')
 1104 FORMAT(5X,' Momentum flag = ',I3,' (Muon only, global, other)',
     1 ' including vertex point ')
 1105 FORMAT(5X,' dEdx correction flag = ',I3,
     1  ' (Lookup table, Geant, Measured) ')
 1109 FORMAT(5X,' Isolation (Hit Cells, Hit Cells+neighbors) = ',
     1 2F8.2,/5X,'           (dR = 0.2,0.4,0.6 Cones )        = ',
     2 3F8.2)
 2101 FORMAT(5X,' Neaside Jet (dRmin,dPhimin,Et) = ',3F8.2,
     1 /5X,' Farside Jet (dPhimax,Et)       = ',8X,2F8.2)
C
 1110 FORMAT(/,10X,' Electron Dump (PELC) :',/)
 1111 FORMAT(5X,' Raw       4-Vector (Ex,Ey,Ez,E) = ',4F8.1,/,
     1 5X,' Corrected 4-Vector (Ex,Ey,Ez,E) = ',4F8.1,/,
     2 5X,' Et (Raw/Corrected) = ',F6.1,' / ',F6.1,' +/- ',F4.1,
     3 /,5X,' Theta = ',F6.2,' Phi = ',F6.2,' Eta = ',F6.2,
     4 ' Ieta = ',F6.2)
C
 1120 FORMAT(/,10X,' Photon Dump (PPHO) :',/)
 1121 FORMAT(5X,' Raw       4-Vector (Ex,Ey,Ez,E) = ',4F8.1,/,
     1 5X,' Corrected 4-Vector (Ex,Ey,Ez,E) = ',4F8.1,/,
     2 5X,' Et (Raw/Corrected) = ',F6.1,' / ',F6.1,' +/- ',F4.1,
     3 /,5X,' Theta = ',F6.2,' Phi = ',F6.2,' Eta = ',F6.2,
     4 ' Ieta = ',F6.2)
 1122 FORMAT(5X,' Energy in Isolation Cone (dR=0.4:Etot,em) = ',2F8.1,/,
     1 5X,' Energy in core (dR=0.2:Etot,em)           = ',2F8.1,/,
     2 5X,' em Energy outside central tower    = ',F8.1)
C
 1130 FORMAT(/,10X,' Jets Dump (JETS : 0.7 Cone Algorithm) :',/)
 1136 FORMAT(/,10X,' Jets Dump (JETS : 0.5 Cone Algorithm) :',/)
 1137 FORMAT(/,10X,' Jets Dump (JETS : 0.3 Cone Algorithm) :',/)
 1138 FORMAT(/,10X,' Jets Dump (JETS : NN Algorithm) :',/)
 1131 FORMAT(5X,' Uncorrected 4-Vector (Ex,Ey,Ez,E) = ',4F8.1,/,
     1 5X,' Et = ',F6.1,/,5X,' Theta = ',F6.2,' Phi = ',F6.2,
     2 ' Eta = ',F6.2,' Ieta = ',F6.2)
 1132 FORMAT(5X,' Corrected 4-Vector (Ex,Ey,Ez,E) = ',4F8.1,/,
     1 5X,' Et = ',F6.1,/,5X,' Theta = ',F6.2,' Phi = ',F6.2,
     2 ' Eta = ',F6.2,' Ieta = ',F6.2)
 1135 FORMAT(5X,' dPhi(jet',I1,'-jet',I1,') = ',F8.1,
     1 ' Mass(Jet',I1,'-jet',I1,') = ',F8.1)
C
 1140 FORMAT(/,10X,' Missing Et Dump (Calorimeters+MG+ICD) :',//,
     1 5X,' 4-Vector (Raw)       (Ex,Ey,Ez,E) = ',4F8.1,/,
     2 5X,' 4-Vector (Corrected) (Ex,Ey,Ez,E) = ',4F8.1,/,
     3 5X,' Et (Raw) / (Corrected) = ',F8.1,' / ',F8.1,' +/- ',F8.1,
     4 /5X,' Theta (Raw/Corrected) = ',F6.2,' / ',F6.2,
     5 ' Phi (Raw/Corrected) = ',F6.2,' / ',F6.2,
     6 /5X,' Eta = ',F6.2)
 1141 FORMAT(5X,' Scalar Et = ',F8.1)
 1142 FORMAT(/,10X,' Missing Et Dump (Calorimeters Only) :',//,
     1 5X,' 4-Vector (Raw)       (Ex,Ey,Ez,E) = ',4F8.1,/,
     2 5X,' 4-Vector (Corrected) (Ex,Ey,Ez,E) = ',4F8.1,/,
     3 5X,' Et (Raw) / (Corrected) = ',F8.1,' / ',F8.1,' +/- ',F8.1,
     4 /5X,' Theta (Raw/Corrected) = ',F6.2,' / ',F6.2,
     5 ' Phi (Raw/Corrected) = ',F6.2,' / ',F6.2,
     6 /5X,' Eta = ',F6.2)
 1143 FORMAT(/,10X,' Missing Et Dump (Calorimeters+MG+ICD+MR) :',//,
     1 5X,' 4-Vector (Raw)       (Ex,Ey,Ez,E) = ',4F8.1,/,
     2 5X,' 4-Vector (Corrected) (Ex,Ey,Ez,E) = ',4F8.1,/,
     3 5X,' Et (Raw) / (Corrected) = ',F8.1,' / ',F8.1,' +/- ',F8.1,
     4 /5X,' Theta (Raw/Corrected) = ',F6.2,' / ',F6.2,
     5 ' Phi (Raw/Corrected) = ',F6.2,' / ',F6.2,
     6 /5X,' Eta = ',F6.2)
 1145 FORMAT(/,10X,' Missing Et Dump (Muon Corrected) :',//,
     1 5X,' 4-Vector (Ex,Ey,Ez,E) = ',4F8.1,/,
     1 5X,' Et = ',F8.1,' +/- ',F8.1,' Theta = ',F6.2,' Phi = ',F6.2,
     2 ' Eta = ',F6.2)
C
 1151 FORMAT(5X,' dPhi(elec-neut) = ',F8.1,' Transverse Mass = ',F8.1)
 1152 FORMAT(5X,' dPhi(muon-neut) = ',F8.1,' Transverse Mass = ',F8.1)
 1153 FORMAT(5X,' W 4-Vector (1st em sol) (Ex,Ey,Ez,E) = ',4F7.1,
     1 /,38X,' PtW = ',F5.1,' XW = ',F5.2)
 1154 FORMAT(5X,' W 4-Vector (2nd em sol) (Ex,Ey,Ez,E) = ',4F7.1,
     1 /,38X,' PtW = ',F5.1,' XW = ',F5.2)
 1155 FORMAT(5X,' No real solution for em-nu W 4-vector',/,
     1 5X,' Pt(e-nu) = ',F7.1)
 1156 FORMAT(5X,' W 4-Vector (1st mu sol) (Ex,Ey,Ez,E) = ',4F7.1,/,
     1 38X,' PtW = ',F5.1,' XW = ',F5.2)
 1157 FORMAT(5X,' W 4-Vector (2nd mu sol) (Ex,Ey,Ez,E) = ',4F7.1,/,
     1 38X,' PtW = ',F5.1,' XW = ',F5.2)
 1158 FORMAT(5X,' No real solution for mu-nu W 4-vector',/,
     1 5X,' Pt(mu-nu) = ',F5.1)
C
 1200 FORMAT(/10X,' Multiparticle Information :',/)
 1201 FORMAT(5X,' dPhi(mu12)      = ',F8.1,' deg, Coplanarity(mu12) = ',
     1 F8.2,/,5X,' Mass(mu12)      = ',F8.2,' XZ(mu12) = ',F5.2/,
     2 5X,' Pt(mu12) = ',F7.1,' /',F7.1,' from muons / calorimeters ')
 1202 FORMAT(5X,' dPhi(elec12)    = ',F8.1,' deg, Coplanarity',
     1 '(elec12) = ',F8.2,/,5X,' Mass(elec12)    = ',F8.2,
     2 ' XZ(elec12) = ',F5.2,' Pt(elec12) = ',F7.1)
 1203 FORMAT(5X,' dPhi(gam12)     = ',F8.1,' deg, Coplanarity',
     1 '(gam12) = ',F8.2,/,5X,' Mass(gam12)     = ',F8.2,
     2 ' XZ(gam12) = ',F5.2,' Pt(gam12) = ',F7.1)
 1204 FORMAT(5X,' dPhi(mu1-elec1) = ',F8.1,
     1  ' deg, Coplanarity(mu1-elec1) = ',F8.2,/,5X,
     2  ' Mass(mu1-elec1) = ',F8.2)
 1205 FORMAT(5X,' dPhi(mu1-gam1)  = ',F8.1,
     1  ' deg, Coplanarity(mu1-gam1) = ',F8.2,/,5X,
     2  ' Mass(mu1-gam1)  = ',F8.2)
 1206 FORMAT(5X,' dPhi(elec1-gam1) = ',F7.1,
     1  ' deg, Coplanarity(elec1-gam1) = ',F8.2,/,5X,
     2  ' Mass(elec1-gam1) = ',F7.2,
     3  ' XZ(elec1-gam1) = ',F5.2,' Pt(elec1-gam1) = ',F7.1)
 1207 FORMAT(5X,' dR(mu1-elec1)   = ',F8.2,
     1 ', Transverse Mass(mu1-elec1-neut) = ',F8.2)
 1208 FORMAT(5X,' dR(mu1-gam1)    = ',F8.2,
     1 ', Transverse Mass(mu1-gam1-neut) = ',F8.2)
 1209 FORMAT(5X,' dR(elec1-gam1)  = ',F8.2,
     1 ', Transverse Mass(elec1-gam1-neut) = ',F8.2)
 1210 FORMAT(5X,'Transverse Mass(elec1-elec2-neut) = ',F8.2)
 1211 FORMAT(5X,'Transverse Mass(elec1-gam1-neut) = ',F8.2)
C
 1215 FORMAT(/10X,' Total jet energy parameters : ',//,
     1 5X,' HT                         = ',F8.3,/,
     2 5X,' H                          = ',F8.3)
 1220 FORMAT(/10X,' Event jet shape parameters : ',//,
     1 5X,' Sphericity                 = ',F8.4,/,
     2 5X,' Planarity                  = ',F8.4,/,
     3 5X,' GSphericity                = ',F8.4,/,
     4 5X,' GAplanarity                = ',F8.4,/,
     5 5X,' GY                         = ',F8.4)
 1225 FORMAT(/10X,' Event jet shape parameters : ',//,
     1 5X,' Sphericity                 = ',F8.4,/,
     2 5X,' Planarity                  = ',F8.4,/,
     3 5X,' GSphericity                = ',F8.4,/,
     4 5X,' GAplanarity                = ',F8.4,/,
     5 5X,' GY                         = ',F8.4,/,
     6 5X,' EMax Shape                 = ',F8.4,/,
     7 5X,' ETMax Shape                = ',F8.4,/,
     8 5X,' Sum ET 4-JETS/Sum E 4-Jets = ',F8.4)
C
 1500 FORMAT(' ')
      END
