      SUBROUTINE TOP_LEPTONS_HIST_FILL(TOP_FLAG,W_FLAG,Z_FLAG,WWG_FLAG,
     1  QCD_FLAG,WPAIR_FLAG,MET_VEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TOP_LEPTONS Histogramming Routine for
C-                              selected event candidates
C-
C-   Inputs  : 
C-              TOP_FLAG - ttbar selection flag array 
C-              W_FLAG   - W selection flag array
C-              Z_FLAG   - Z selection flag array
C-              WWG_FLAG - WWgamma selection flag array
C-
C-   Outputs : None
C-   Controls: None
C-
C-   Created  31-JUL-1992   Stephen J. Wimpenny
C-   Modified  3-Sep-1992   Diagnostics added. 
C-   Modified 14-Sep-1992   diagnostics switched from RCP file
C-   Modified 12-Oct-1992   Trigger,Vertex and muon isolation added
C-   Modified 16-Nov-1992   HT added for leptons+jets anal
C-   Modified 28-Dec-1992   Electron/Photon Histogramming updated
C-   Modified 30-Dec-1992   Mt for electron events modified to icl muons
C-                          if good muons present
C-   Modified 31-Dec-1992   Plots to study muon dE/dx + bremm added
C-   Modified 28-Jan-1993   Plots for muon+jet analysis added
C-                          Z selection hooks added
C-   Modified  1-Feb-1993   WWgamma hook added
C-   Modified  4-Mar-1993   new histograms added
C-   Modified 15-Mar-1993   Name changes for Good_Electron, Good_Photon,
C-                          Good_Jet and Good_Muon logicals, Coplan,
C-                          Mass4, Masst, Nearjet, Decode_Ifw2,
C-                          Calor_Ratios, Caltrak_Angles, Mupair_Signs,
C-                          Which_Trig_Bits, Get_Vertx_Data,
C-                          Dr_From_Deta_Dphi
C-   Modified 23-Mar-1993   New histograms added for mumu analysis
C-   Modified 21-Apr-1993   Updates to muon plots for RECO 11
C-   Modified 29-Apr-1993   Jet energy corrections added
C-   Updated  17-MAY-1993   Brajesh C Choudhary   Wgamma related histograms 
C-   Modified  9-Jul-1993   Energy Scale corrections included
C-                          Topological variables added
C-   Modified 17-Jul-1993   Logic tidied up
C-   Modified  4-Sep-1993   More topoplogical plots added
C-   Modified 16-Sep-1993   RE Hall Put in dphi(max):mu,Et(miss3) hist's
C-   Modified  4-Dec-1993   Requirement of at least 1 muon removed from
C-                          'MUJET' histograms (background studies)
C-   Modified  1-Apr-1994   Muon Mip plots revised
C-   Modified 17--May-1994  Muon Monitoring Updated
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      EXTERNAL TOP_LEPTONS_GOOD_MUON,TOP_LEPTONS_GOOD_JET
      EXTERNAL TOP_LEPTONS_GOOD_PHOTON,TOP_LEPTONS_GOOD_ELECTRON
      EXTERNAL TOP_LEPTONS_UTIL_MASS4,TOP_LEPTONS_UTIL_MASST
      EXTERNAL TOP_LEPTONS_UTIL_COPLAN,TOP_LEPTONS_UTIL_CALC_DR
      EXTERNAL TOP_LEPTONS_EM_CORRECTION
C
      LOGICAL ELECTRON_HIST,MUON_HIST,DIMUON_HIST,PHOTON_HIST,MUJET_HIST
      LOGICAL FIRST,CORR_JETS,CORR_EM
      LOGICAL TOP_LEPTONS_GOOD_MUON,TOP_LEPTONS_GOOD_JET
      LOGICAL TOP_LEPTONS_GOOD_PHOTON,TOP_LEPTONS_GOOD_ELECTRON
C
      INTEGER PMUO_VERS,MR_BITS,MR_PERMIT,MULTIPLE_INTERACTION_TOOL
      INTEGER I,IV,J,IER,ITEMP,JTEMP,JBIT,SIGN1,SIGN2,PAIR
      INTEGER IOFF_EL,IOFF_PH,IOFF_MU,IOFF_JT,IOFF_NU,IOFF_PHYS,IOFF_MJ
      INTEGER NO_TRIG_LVL1,TRIG_LVL1_BITS(32)
      INTEGER NO_TRIG_LVL2,TRIG_LVL2_BITS(64)
      INTEGER NO_IFW2_BITS,IFW2_BITS(32)
      INTEGER NO_IFW3_BITS,IFW3_BITS(32)
      INTEGER NOVERT_P,NOVERT_S,NO_ZTRAK,NO_ZTRAK_VERTX_P(5)
      INTEGER I_MU,LPMUO,GZPMUO,LPMUO_VEC(5),LMUOT
      INTEGER I_EL,LPELC,GZPELC,LPELC_VEC(5)
      INTEGER I_PH,LPPHO,GZPPHO,LPPHO_VEC(5)
      INTEGER I_JT,LJETS,GZJETS,LJETS_VEC(10)
      INTEGER LJETS_DR,LJETS2_DR,LJETS_DPHI
      INTEGER LPNUT1,LPNUT2,LPNUT3,GZPNUT,GZHEAD
      INTEGER LZTRK,LDTRK,LFDCT,LVTXT,LHMTC
      INTEGER TOP_FLAG(6),W_FLAG(3),Z_FLAG(3),WWG_FLAG(3),QCD_FLAG(6)
      INTEGER WPAIR_FLAG(6)
      INTEGER NEARJETS(2),ICALL,WAM_HIT(6),SAM_HIT(3)
C
      REAL PELC_EMCOR(5),PPHO_EMCOR(5),PT_CORR,PT_CORR2
      REAL VECT_CORR(4),VECT2_CORR(4)
      REAL TEMP,TEMP2,CONV,PI,TWOPI,DTHETA,DPHI,DETA
      REAL VECT(4),VECT2(4),VMOD,VADD,MT3_CLUS
      REAL SPHER,PLAN,GSPHER,GAPLAN,GY,EMAXSH,ETMAXSH,EFOUR_SHAPE
      REAL MASS12,TOP_LEPTONS_UTIL_MASS4
      REAL MASS12_T,TOP_LEPTONS_UTIL_MASST,TOP_LEPTONS_EM_CORRECTION
      REAL ETMISS,ETMISS_CORR,ETMISS_CORR_PHI,PT1,PT2
      REAL ETAJT_MAX,PTJT_MIN,HT,HT_JETPT_MIN,HT_JETETA_MAX
      REAL ZTRAK_THETA,ZTRAK_PHI,ZTRAK_ETA,ZTRAK_DCOS,VERTX_P(3,5)
      REAL EISOL,EMCORE,EMFRAC,DCOS_FROM_ETA_PHI,DPHI_VEC12,DETA_VEC12
      REAL DR_VEC12,DR2_VEC12,TOP_LEPTONS_UTIL_CALC_DR
      REAL PHI_OPP
      REAL JET_EX,JET_EY,JET_EZ,JET_E,JET_ET,JET_ETA,JET_PHI
      REAL TOP_LEPTONS_UTIL_COPLAN,TWO_BODY_COPLAN
      REAL PT12_LEP,PT12_CALO,RZ_IMPACT,XY_IMPACT,TIME29
      REAL MJ_MU_PT(2),MJ_MU_ETA(2),MJ_MU_PHI(2)
      REAL MJ_JET_ET(5),MJ_JET_ETA(5),MJ_JET_PHI(5)
      REAL DPHI_MU_JET(2),DR_MU_JET(2)
      REAL E2,E4,E6,ET2,ET4,TEMPT,ZVERT
      REAL MU_PHO_NEU_CLUSMASS,DR_PHOT_MUON
      REAL MET_VEC(3),DPHI_1,DPHI_2
      REAL PHI12,DPHI12_ECAL,DPHI12_ECORR
      REAL PHI_REC,PT_REC,DPHI_REC_ECAL,DPHI_REC_ECORR
C
      DATA IOFF_EL,IOFF_PH,IOFF_MU,IOFF_JT/ 1000,2000,3000,4000/
      DATA IOFF_NU,IOFF_PHYS,IOFF_MJ/ 5000,6000,7000/
      DATA CONV,PI,TWOPI/ 57.29578,3.1415927,6.2831853/
      DATA FIRST/.TRUE./
C
C *** Read Control Parameters from RCP file
C
      IF(FIRST) THEN
        IER=0
C
C *** Get all latest parameter/Options Values
C
        CALL EZPICK('TOP_LEPTONS_RCP')
        CALL EZGET('PLOT_ELECTRON_HIST',ELECTRON_HIST,IER)
        IF(IER.EQ.0) CALL EZGET('PLOT_MUON_HIST',MUON_HIST,IER)
        IF(IER.EQ.0) CALL EZGET('PLOT_DIMUON_HIST',DIMUON_HIST,IER)
        IF(IER.EQ.0) CALL EZGET('PLOT_MUJET_HIST',MUJET_HIST,IER)
        IF(IER.EQ.0) CALL EZGET('PLOT_PHOTON_HIST',PHOTON_HIST,IER)
        IF(IER.EQ.0) CALL EZGET('JETS_ETAMAX',ETAJT_MAX,IER)
        IF(IER.EQ.0) CALL EZGET('JETS_PTMIN',PTJT_MIN,IER)
        IF(IER.EQ.0) CALL EZGET('EM_CORR',CORR_EM,IER)
        IF(IER.EQ.0) CALL EZGET('JETS_CORR',CORR_JETS,IER)
        IF(IER.EQ.0) CALL EZGET('LJ_HT_JETPT_MIN',HT_JETPT_MIN,IER)
        IF(IER.EQ.0) CALL EZGET('LJ_HT_JETETA_MAX',HT_JETETA_MAX,IER)
        IF (IER.NE.0) CALL ERRMSG('Error getting RCP parameters',
     &    'TOP_LEPTONS_HIST_FILL',' ','F')
        CALL EZRSET
C
C *** Check consistency of HT selection and Jet preselection
C *** - reset limits if necessary
C
        IF(HT_JETPT_MIN.LT.PTJT_MIN) HT_JETPT_MIN=PTJT_MIN
        IF(HT_JETETA_MAX.GT.ETAJT_MAX) HT_JETETA_MAX=ETAJT_MAX
        FIRST=.FALSE.
      ENDIF
C
C *** Go to HBOOK directory
C
      CALL HCDIR('//PAWC/TOP_LEPTONS',' ')
C
C *** Get information for monitoring :
C ***        get+store pointers
C ***   a.) electrons
C
      I_EL=0
      CALL VFILL(PELC_EMCOR,5,1.)
      LPELC=GZPELC()
      DO WHILE (LPELC.GT.0)
        IF(TOP_LEPTONS_GOOD_ELECTRON(LPELC)) THEN
          I_EL=I_EL+1
          IF (CORR_EM) THEN
            PELC_EMCOR(I_EL)=TOP_LEPTONS_EM_CORRECTION(LPELC)
          ENDIF
          IF(I_EL.LT.6) LPELC_VEC(I_EL)=LPELC
        ENDIF
        LPELC=LQ(LPELC)
      ENDDO
      IF(I_EL.GT.5) I_EL=5
C
C ***   b.) muons
C
      I_MU=0
      LPMUO=GZPMUO(0)
      DO WHILE (LPMUO.GT.0)
        IF(TOP_LEPTONS_GOOD_MUON(LPMUO)) THEN
          I_MU=I_MU+1
          IF(I_MU.LT.6) LPMUO_VEC(I_MU)=LPMUO
        ENDIF
        LPMUO=LQ(LPMUO)
      ENDDO
      IF(I_MU.GT.5) I_MU=5
C
C ***    c.) photons
C
      I_PH=0
      CALL VFILL(PPHO_EMCOR,5,1.)
      LPPHO=GZPPHO()
      DO WHILE (LPPHO.GT.0)
        IF(TOP_LEPTONS_GOOD_PHOTON(LPPHO)) THEN
          I_PH=I_PH+1
          IF (CORR_EM) THEN
            PPHO_EMCOR(I_PH)=TOP_LEPTONS_EM_CORRECTION(LPPHO)
          ENDIF
          IF(I_PH.LT.6) LPPHO_VEC(I_PH)=LPPHO
        ENDIF
        LPPHO=LQ(LPPHO)
      ENDDO
      IF(I_PH.GT.5) I_PH=5
C
C ***   d.) jets
C
      I_JT=0
      HT=0.
      LJETS=GZJETS()
      DO WHILE (LJETS.GT.0)
        IF(TOP_LEPTONS_GOOD_JET(LJETS)) THEN
          I_JT=I_JT+1
          IF(I_JT.LT.11) LJETS_VEC(I_JT)=LJETS
          IF(CORR_JETS) THEN
            CALL TOP_LEPTONS_CORR_JETPARM(LJETS,JET_E,JET_ET,JET_EX,
     1        JET_EY,JET_EZ,JET_PHI,JET_ETA,IER)
            IF(IER.LT.0) THEN
              JET_ET=Q(LJETS+6)
              JET_ETA=Q(LJETS+9)
            ENDIF
          ELSE
            JET_ET=Q(LJETS+6)
            JET_ETA=Q(LJETS+9)
          ENDIF
          IF(JET_ET.GE.HT_JETPT_MIN.AND.
     1      ABS(JET_ETA).LE.HT_JETETA_MAX) THEN
              HT=HT+JET_ET
          ENDIF
        ENDIF
        LJETS=LQ(LJETS)
      ENDDO
      IF(I_JT.GT.10) I_JT=10
C
C ***    e.) Missing Et
C
      LPNUT1=GZPNUT(1)
      LPNUT2=GZPNUT(4)
      IF(LPNUT2.LE.0) LPNUT2=GZPNUT(2)
      LPNUT3=GZPNUT(3)
C
C *** Fill monitoring histograms
C ***    a.) general event characteristics
C
      LHEAD=GZHEAD()
C
C *** Level 1 and 2 Trigger Bits
C
      CALL TOP_LEPTONS_UTIL_DECODE_TRIG(NO_TRIG_LVL1,TRIG_LVL1_BITS,
     1  NO_TRIG_LVL2,TRIG_LVL2_BITS,IER)
C
      DO IV=1,NO_TRIG_LVL1
        TEMP=FLOAT(TRIG_LVL1_BITS(IV))
        CALL HFILL(1,TEMP,0.,1.)
      ENDDO
C
      DO IV=1,NO_TRIG_LVL2
        TEMP=FLOAT(TRIG_LVL2_BITS(IV))
        CALL HFILL(2,TEMP,0.,1.)
      ENDDO
C
C *** Main Ring Timing
C
      IER=-1
      CALL TOP_LEPTONS_UTIL_MAIN_RING(TIME29,MR_BITS,IER)
      MR_PERMIT=0
      IF(IER.EQ.0) MR_PERMIT=JBIT(MR_BITS,5)
      IF(IER.EQ.0) CALL HFILL(3,TIME29,0.,1.)
C      
C *** Vertex and global track information Information
C
      CALL TOP_LEPTONS_UTIL_DECODE_VERTEX(NOVERT_P,NOVERT_S,NO_ZTRAK,
     1 NO_ZTRAK_VERTX_P,VERTX_P,IER)
      IF(IER.LT.0) THEN
        TEMP=-10.
        CALL HFILL(20,TEMP,0.,1.)
        CALL HFILL(21,TEMP,0.,1.)
      ENDIF
      IF(IER.GT.0) THEN
        ZVERT=VERTX_P(3,1)
        TEMP=FLOAT(NOVERT_P)
        CALL HFILL(20,TEMP,0.,1.)
        TEMP=FLOAT(NOVERT_S)
        CALL HFILL(21,TEMP,0.,1.)
        DO I=1,NOVERT_P
          CALL HFILL(22,VERTX_P(1,I),0.,1.)
          CALL HFILL(23,VERTX_P(2,I),0.,1.)
          CALL HFILL(24,VERTX_P(3,I),0.,1.)
          TEMP=FLOAT(NO_ZTRAK_VERTX_P(I))
          IF(I.EQ.1) CALL HFILL(31,TEMP,0.,1.)
          IF(I.GT.1) CALL HFILL(32,TEMP,0.,1.)
        ENDDO
        IF(NOVERT_P.GT.1) THEN
          TEMP=VERTX_P(3,1)-VERTX_P(3,2)
          CALL HFILL(25,TEMP,0.,1.)
        ENDIF
      ENDIF
      TEMP=FLOAT(NO_ZTRAK)
      CALL HFILL(30,TEMP,0.,1.)
C
      TEMP=FLOAT(I_EL)
      CALL HFILL(10,TEMP,0.,1.)
      TEMP=FLOAT(I_MU)
      CALL HFILL(11,TEMP,0.,1.)
      TEMP=FLOAT(I_PH)
      CALL HFILL(12,TEMP,0.,1.)
      TEMP=FLOAT(I_JT)
      CALL HFILL(13,TEMP,0.,1.)
C
C *** Electrons
C
      IF(I_EL.LT.1) GO TO 10
      DO I=1,I_EL
        PT_CORR=PELC_EMCOR(I)*Q(LPELC_VEC(I)+7)
        CALL HFILL(100,PT_CORR,0.,1.)
        CALL HFILL(101,Q(LPELC_VEC(I)+9),0.,1.)
        TEMP=Q(LPELC_VEC(I)+10)*CONV
        CALL HFILL(102,TEMP,0.,1.)
        TEMP=Q(LPELC_VEC(I)+19)/10.
        CALL HFILL(106,TEMP,0.,1.)
      ENDDO
   10 IF(I_MU.LT.1) GO TO 15
C
C *** Muons
C
      DO I=1,I_MU
        CALL DET_ETA(ZVERT,Q(LPMUO_VEC(I)+15),TEMP)
        CALL HFILL(116,TEMP,0.,1.)
        CALL HFILL(110,Q(LPMUO_VEC(I)+14),0.,1.)
        CALL HFILL(111,Q(LPMUO_VEC(I)+16),0.,1.)
        TEMP=Q(LPMUO_VEC(I)+17)*CONV
        CALL HFILL(112,TEMP,0.,1.)
      ENDDO
   15 IF(I_MU.LT.2) GO TO 20
C
C *** store dimuon pair phi vector 
C
        CALL UCOPY(Q(LPMUO_VEC(1)+10),VECT,4)
        CALL VADD(VECT(1),Q(LPMUO_VEC(2)+10),VECT2(1),4)
        PHI12=ATAN2(VECT2(2),VECT2(1))
   20 IF(I_PH.LT.1) GO TO 30
C
C *** Photons
C
      DO I=1,I_PH
        PT_CORR=PPHO_EMCOR(I)*Q(LPPHO_VEC(I)+7)
        CALL HFILL(120,PT_CORR,0.,1.)
        CALL HFILL(121,Q(LPPHO_VEC(I)+9),0.,1.)
        TEMP=Q(LPPHO_VEC(I)+10)*CONV
        CALL HFILL(122,TEMP,0.,1.)
        TEMP=Q(LPPHO_VEC(I)+19)/10.
        CALL HFILL(126,TEMP,0.,1.)
      ENDDO
   30 IF(I_JT.LT.1) GO TO 40
C
C *** Jets
C
      CALL VZERO(VECT,4)
      DO I=1,I_JT
        IF(CORR_JETS) THEN
          CALL TOP_LEPTONS_CORR_JETPARM(LJETS_VEC(I),JET_E,JET_ET,
     1      JET_EX,JET_EY,JET_EZ,JET_PHI,JET_ETA,IER)
          IF(IER.LT.0) THEN
              JET_ETA=Q(LJETS_VEC(I)+9)
              JET_PHI=Q(LJETS_VEC(I)+8)
              JET_ET=Q(LJETS_VEC(I)+6)
          ENDIF
        ELSE
          JET_ETA=Q(LJETS_VEC(I)+9)
          JET_PHI=Q(LJETS_VEC(I)+8)
          JET_ET=Q(LJETS_VEC(I)+6)
        ENDIF
        CALL DET_ETA(ZVERT,Q(LJETS_VEC(I)+7),TEMP)
        CALL HFILL(136,TEMP,0.,1.)
        CALL HFILL(130,JET_ET,0.,1.)
        CALL HFILL(131,JET_ETA,0.,1.)
        TEMP=JET_PHI*CONV
        CALL HFILL(132,TEMP,0.,1.)
C
C *** Find the Pt of the sum of all jets (ie. recoil system Et)
C
        CALL VADD(Q(LJETS_VEC(I)+2),VECT(1),VECT(1),4)
      ENDDO
C
C *** store phi angle + magnitude of recoil(jet) system 
C
      PHI_REC=ATAN2(VECT(2),VECT(1))
      IF(PHI_REC.LT.0) PHI_REC=PHI_REC+TWOPI
      PT_REC=SQRT(VECT(1)**2+VECT(2)**2)
   40 CONTINUE
C
C *** Missing Energy
C
      IF(LPNUT1.NE.0) THEN
        ETMISS=(Q(LPNUT1+3)+MET_VEC(1))**2 +
     1   (Q(LPNUT1+4)+MET_VEC(2))**2
        ETMISS=SQRT(ETMISS)
        CALL HFILL(140,ETMISS,0.,1.)
      ENDIF
      ETMISS=0.
      IF(LPNUT2.NE.0) THEN
        ETMISS=(Q(LPNUT2+3)+MET_VEC(1))**2 +
     1   (Q(LPNUT2+4)+MET_VEC(2))**2
        ETMISS=SQRT(ETMISS)
        CALL HFILL(145,ETMISS,0.,1.)
        TEMP=Q(LPNUT2+10)*CONV
        CALL HFILL(147,TEMP,0.,1.)
      ENDIF
      ETMISS_CORR=0.
      ETMISS_CORR_PHI=0.
      IF(I_MU.GT.0.AND.LPNUT3.NE.0) THEN
        ETMISS_CORR=Q(LPNUT3+7)
        CALL HFILL(150,ETMISS_CORR,0.,1.)
        ETMISS_CORR_PHI=Q(LPNUT3+10)
        TEMP=ETMISS_CORR_PHI*CONV
        CALL HFILL(152,TEMP,0.,1.)
      ENDIF
      CALL HFILL(180,HT,0.,1.)
C
C *** Recoil (jet) system plots
C
      CALL HFILL(210,PHI_REC*CONV,0.,1.)
C
C *** dphi between dimuon Pt vector and Etmiss (cal&corr)
C
      DPHI_REC_ECAL=ABS(PHI_REC-Q(LPNUT2+10))
      IF(DPHI_REC_ECAL.GT.PI) DPHI_REC_ECAL=TWOPI-DPHI_REC_ECAL
      CALL HFILL(211,DPHI_REC_ECAL*CONV,0.,1.)
      DPHI_REC_ECORR=ABS(PHI12-ETMISS_CORR_PHI)
      IF(DPHI_REC_ECORR.GT.PI) DPHI_REC_ECORR=TWOPI-DPHI_REC_ECORR
      CALL HFILL(212,DPHI_REC_ECORR*CONV,0.,1.)
C
      CALL HFILL(213,PT_REC,0.,1.)
      CALL HFILL(214,PT_REC,ETMISS,1.)
      CALL HFILL(215,PT_REC,ETMISS_CORR,1.)
C
C ***    b.) physics event characteristics
C
      IF(TOP_FLAG(1).GT.0.OR.QCD_FLAG(1).GT.0) THEN
        IF(TOP_FLAG(2).GT.0.OR.QCD_FLAG(2).GT.0) THEN
C
C *** top emu event
C
          IF(I_EL.GT.0.AND.I_MU.GT.0) THEN
            CALL VSCALE(Q(LPELC_VEC(1)+3),PELC_EMCOR(1),VECT_CORR(1),4)
            CALL VADD(Q(LPMUO_VEC(1)+10),VECT_CORR(1),VECT(1),4)
            MASS12=TOP_LEPTONS_UTIL_MASS4(VECT)
            CALL HFILL(IOFF_PHYS+10,MASS12,0.,1.)
            DPHI=ABS(Q(LPELC_VEC(1)+10)-Q(LPMUO_VEC(1)+17))
            IF(DPHI.GT.PI) DPHI=TWOPI-DPHI
            DETA=(Q(LPELC_VEC(1)+9)-Q(LPMUO_VEC(1)+16))
            DR_VEC12=TOP_LEPTONS_UTIL_CALC_DR(DETA,DPHI)
            DPHI=DPHI*CONV
            PT_CORR=PELC_EMCOR(1)*Q(LPELC_VEC(1)+7)
            IF(PT_CORR.GT.Q(LPMUO_VEC(1)+14)) THEN
              CALL HFILL(IOFF_PHYS+120,Q(LPMUO_VEC(1)+14),DPHI,1.)
            ELSE
              CALL HFILL(IOFF_PHYS+120,PT_CORR,DPHI,1.)
            ENDIF
            CALL HFILL(IOFF_PHYS+20,DPHI,0.,1.)
            CALL HFILL(IOFF_PHYS+60,DR_VEC12,0.,1.)
            CALL HFILL(IOFF_PHYS+30,ETMISS_CORR,DPHI,1.)
            CALL HFILL(IOFF_PHYS+130,MASS12,ETMISS_CORR,1.)
            CALL HFILL(IOFF_PHYS+90,MASS12,DPHI,1.)
            CALL HFILL(IOFF_PHYS+40,PT_CORR,
     1         Q(LPMUO_VEC(1)+14),1.)
            CALL HFILL(IOFF_PHYS+1040,PT_CORR,
     1         -1/Q(LPMUO_VEC(1)+14),1.)
            TWO_BODY_COPLAN=TOP_LEPTONS_UTIL_COPLAN(Q(LPMUO_VEC(1)+10),
     1        VECT_CORR(1))
            CALL HFILL(IOFF_PHYS+50,TWO_BODY_COPLAN,0.,1.)
            CALL HFILL(IOFF_PHYS+80,TWO_BODY_COPLAN,DPHI,1.)
            IF(I_JT.GE.1) THEN
              CALL TOP_LEPTONS_UTIL_NEARJET(Q(LPMUO_VEC(1)+16),
     1          Q(LPMUO_VEC(1)+17),DR_VEC12,LJETS_DR,DPHI_VEC12,
     2          LJETS_DPHI)
              CALL TOP_LEPTONS_UTIL_NEARJET(Q(LPELC_VEC(1)+9),
     1          Q(LPELC_VEC(1)+10),DR2_VEC12,LJETS2_DR,DPHI_VEC12,
     2          LJETS_DPHI)
              TEMP=FLOAT(I_JT)
              CALL HFILL(IOFF_PHYS+110,DR_VEC12,TEMP,1.)
              CALL HFILL(IOFF_PHYS+111,DR2_VEC12,TEMP,1.)
            ENDIF
C
C *** 3-body Mt
C
            CALL VZERO(VECT,4)
            CALL UCOPY(Q(LPMUO_VEC(1)+10),VECT,4)
            CALL VADD(VECT_CORR(1),VECT(1),VECT(1),4)
            MASS12=TOP_LEPTONS_UTIL_MASS4(VECT)
C
            CALL VZERO(VECT2,4)
            IF(LPNUT3.GT.0) THEN
              CALL VADD(VECT,Q(LPNUT3+3),VECT2,2)
              MT3_CLUS=( SQRT(MASS12**2+VMOD(VECT,2)**2)
     1          +VMOD(Q(LPNUT3+3),2) )**2 - VMOD(VECT2,2)**2
            ELSE
              CALL VADD(VECT,Q(LPNUT2+3),VECT2,2)
              CALL VADD(VECT2,MET_VEC(1),VECT2,2)
              MT3_CLUS=( SQRT(MASS12**2+VMOD(VECT,2)**2)
     1          +VMOD(Q(LPNUT2+3),2) )**2 - VMOD(VECT2,2)**2
            ENDIF
            IF(MT3_CLUS.GT.0.) THEN
              MT3_CLUS=SQRT(MT3_CLUS)
            ELSE
              MT3_CLUS=0.
            ENDIF
            CALL HFILL(IOFF_PHYS+140,MT3_CLUS,0.,1.)
          ENDIF
C
C *** Also pick up photon-muon events
C
          IF(I_PH.GT.0.AND.I_MU.GT.0) THEN
            CALL VSCALE(Q(LPPHO_VEC(1)+3),PPHO_EMCOR(1),VECT_CORR(1),4)
            CALL VADD(Q(LPMUO_VEC(1)+10),VECT_CORR(1),VECT(1),4)
            MASS12=TOP_LEPTONS_UTIL_MASS4(VECT)
            CALL HFILL(IOFF_PHYS+14,MASS12,0.,1.)
            DPHI=ABS(Q(LPPHO_VEC(1)+10)-Q(LPMUO_VEC(1)+17))
            IF(DPHI.GT.PI) DPHI=TWOPI-DPHI
            DETA=(Q(LPPHO_VEC(1)+9)-Q(LPMUO_VEC(1)+16))
            DR_VEC12=TOP_LEPTONS_UTIL_CALC_DR(DETA,DPHI)
            DPHI=DPHI*CONV
            PT_CORR=PPHO_EMCOR(1)*Q(LPPHO_VEC(1)+7)
            IF(PT_CORR.GT.Q(LPMUO_VEC(1)+14)) THEN
              CALL HFILL(IOFF_PHYS+124,Q(LPMUO_VEC(1)+14),DPHI,1.)
            ELSE
              CALL HFILL(IOFF_PHYS+124,PT_CORR,DPHI,1.)
            ENDIF
            CALL HFILL(IOFF_PHYS+24,DPHI,0.,1.)
            CALL HFILL(IOFF_PHYS+64,DR_VEC12,0.,1.)
            CALL HFILL(IOFF_PHYS+212,MASS12,DR_VEC12,1.)
            DR_PHOT_MUON = DR_VEC12
            CALL HFILL(IOFF_PHYS+34,ETMISS_CORR,DPHI,1.)
            CALL HFILL(IOFF_PHYS+134,MASS12,ETMISS_CORR,1.)
            CALL HFILL(IOFF_PHYS+94,MASS12,DPHI,1.)
            CALL HFILL(IOFF_PHYS+44,PT_CORR,
     1         Q(LPMUO_VEC(1)+14),1.)
            TWO_BODY_COPLAN=TOP_LEPTONS_UTIL_COPLAN(Q(LPMUO_VEC(1)+10),
     1         VECT_CORR(1))
            CALL HFILL(IOFF_PHYS+54,TWO_BODY_COPLAN,0.,1.)
            CALL HFILL(IOFF_PHYS+84,TWO_BODY_COPLAN,DPHI,1.)
            IF(I_JT.GE.1) THEN
              CALL TOP_LEPTONS_UTIL_NEARJET(Q(LPMUO_VEC(1)+16),
     1          Q(LPMUO_VEC(1)+17),DR_VEC12,LJETS_DR,DPHI_VEC12,
     2          LJETS_DPHI)
              CALL TOP_LEPTONS_UTIL_NEARJET(Q(LPPHO_VEC(1)+9),
     1          Q(LPPHO_VEC(1)+10),DR2_VEC12,LJETS2_DR,DPHI_VEC12,
     2          LJETS_DPHI)
              TEMP=FLOAT(I_JT)
              CALL HFILL(IOFF_PHYS+112,DR2_VEC12,TEMP,1.)
            ENDIF
C
C *** 3-body Mt
C
            CALL VZERO(VECT,4)
            CALL UCOPY(Q(LPMUO_VEC(1)+10),VECT,4)
            CALL VADD(VECT_CORR(1),VECT(1),VECT(1),4)
            MASS12=TOP_LEPTONS_UTIL_MASS4(VECT)
C
            CALL VZERO(VECT2,4)
            IF(LPNUT3.GT.0) THEN
              CALL VADD(VECT,Q(LPNUT3+3),VECT2,2)
              MT3_CLUS=( SQRT(MASS12**2+VMOD(VECT,2)**2)
     1           +VMOD(Q(LPNUT3+3),2) )**2 - VMOD(VECT2,2)**2
            ELSE
              CALL VADD(VECT,Q(LPNUT2+3),VECT2,2)
              CALL VADD(VECT2,MET_VEC(1),VECT2,2)
              MT3_CLUS=( SQRT(MASS12**2+VMOD(VECT,2)**2)
     1          +VMOD(Q(LPNUT2+3),2) )**2 - VMOD(VECT2,2)**2
            ENDIF
            IF(MT3_CLUS.GT.0.) THEN
              MT3_CLUS=SQRT(MT3_CLUS)
            ELSE
              MT3_CLUS=0.
            ENDIF
            MU_PHO_NEU_CLUSMASS = MT3_CLUS
            CALL HFILL(IOFF_PHYS+141,MT3_CLUS,0.,1.)
            CALL HFILL(IOFF_PHYS+210,MT3_CLUS,DR_PHOT_MUON,1.)
          ENDIF
        ENDIF
        IF(TOP_FLAG(3).GT.0.OR.QCD_FLAG(3).GT.0) THEN
C
C *** top ee event
C
          IF(I_EL.GT.1) THEN
            CALL VSCALE(Q(LPELC_VEC(1)+3),PELC_EMCOR(1),VECT_CORR(1),4)
            CALL VSCALE(Q(LPELC_VEC(2)+3),PELC_EMCOR(2),VECT2_CORR(1),4)
            CALL VZERO(VECT,4)
            CALL VADD(VECT_CORR(1),VECT(1),VECT(1),4)
            CALL VADD(VECT2_CORR(1),VECT(1),VECT(1),4)
            MASS12=TOP_LEPTONS_UTIL_MASS4(VECT)
            CALL HFILL(IOFF_PHYS+11,MASS12,0.,1.)
            DPHI=ABS(Q(LPELC_VEC(1)+10)-Q(LPELC_VEC(2)+10))
            IF(DPHI.GT.PI) DPHI=TWOPI-DPHI
            DPHI=DPHI*CONV
            PT_CORR2=PELC_EMCOR(2)*Q(LPELC_VEC(2)+7)
            CALL HFILL(IOFF_PHYS+121,PT_CORR2,DPHI,1.)
            CALL HFILL(IOFF_PHYS+21,DPHI,0.,1.)
            CALL HFILL(IOFF_PHYS+31,ETMISS,DPHI,1.)
            PT_CORR=PELC_EMCOR(1)*Q(LPELC_VEC(1)+7)
            CALL HFILL(IOFF_PHYS+41,PT_CORR,
     1         PT_CORR2,1.)
            IF(I_JT.GE.1) THEN
              CALL TOP_LEPTONS_UTIL_NEARJET(Q(LPELC_VEC(1)+9),
     1          Q(LPELC_VEC(1)+10),DR_VEC12,LJETS_DR,DPHI_VEC12,
     2          LJETS_DPHI)
              CALL TOP_LEPTONS_UTIL_NEARJET(Q(LPELC_VEC(2)+9),
     1          Q(LPELC_VEC(2)+10),DR2_VEC12,LJETS2_DR,DPHI_VEC12,
     2          LJETS_DPHI)
              CALL HFILL(6071,DR_VEC12,DR2_VEC12,1.)
            ENDIF
C
C *** 3-body Mt
C
            CALL VZERO(VECT,4)
            CALL UCOPY(VECT_CORR(1),VECT,4)
            CALL VADD(VECT2_CORR(1),VECT(1),VECT(1),4)
            MASS12=TOP_LEPTONS_UTIL_MASS4(VECT)
C
            CALL VZERO(VECT2,4)
            IF(LPNUT3.GT.0) THEN
              CALL VADD(VECT,Q(LPNUT3+3),VECT2,2)
              MT3_CLUS=( SQRT(MASS12**2+VMOD(VECT,2)**2)
     1          +VMOD(Q(LPNUT3+3),2) )**2 - VMOD(VECT2,2)**2
            ELSE
              CALL VADD(VECT,Q(LPNUT2+3),VECT2,2)
              CALL VADD(VECT2,MET_VEC(1),VECT2,2)
              MT3_CLUS=( SQRT(MASS12**2+VMOD(VECT,2)**2)
     1          +VMOD(Q(LPNUT2+3),2) )**2 - VMOD(VECT2,2)**2
            ENDIF
            IF(MT3_CLUS.GT.0.) THEN
              MT3_CLUS=SQRT(MT3_CLUS)
            ELSE
              MT3_CLUS=0.
            ENDIF
            CALL HFILL(IOFF_PHYS+142,MT3_CLUS,0.,1.)
          ENDIF
C
C *** also keep electron-photon pairs
C
          IF(I_PH.GT.0.AND.I_EL.EQ.1) THEN
            CALL VSCALE(Q(LPELC_VEC(1)+3),PELC_EMCOR(1),VECT_CORR(1),4)
            CALL VSCALE(Q(LPPHO_VEC(1)+3),PPHO_EMCOR(1),VECT2_CORR(1),4)
            CALL VZERO(VECT,4)
            CALL VADD(VECT_CORR(1),VECT(1),VECT(1),4)
            CALL VADD(VECT2_CORR(1),VECT(1),VECT(1),4)
            MASS12=TOP_LEPTONS_UTIL_MASS4(VECT)
            CALL HFILL(IOFF_PHYS+15,MASS12,0.,1.)
            DPHI=ABS(Q(LPELC_VEC(1)+10)-Q(LPPHO_VEC(1)+10))
            IF(DPHI.GT.PI) DPHI=TWOPI-DPHI
            DPHI=DPHI*CONV
            PT_CORR=PELC_EMCOR(1)*Q(LPELC_VEC(1)+7)
            PT_CORR2=PPHO_EMCOR(1)*Q(LPPHO_VEC(1)+7)
            IF(PT_CORR2.GT.PT_CORR) THEN
              CALL HFILL(IOFF_PHYS+125,PT_CORR,DPHI,1.)
              CALL HFILL(IOFF_PHYS+45,PT_CORR,PT_CORR2,1.)
            ELSE
              CALL HFILL(IOFF_PHYS+125,PT_CORR2,DPHI,1.)
              CALL HFILL(IOFF_PHYS+45,PT_CORR2,PT_CORR,1.)
            ENDIF
            CALL HFILL(IOFF_PHYS+25,DPHI,0.,1.)
            CALL HFILL(IOFF_PHYS+35,ETMISS,DPHI,1.)
            IF(I_JT.GE.1) THEN
              CALL TOP_LEPTONS_UTIL_NEARJET(Q(LPELC_VEC(1)+9),
     1          Q(LPELC_VEC(1)+10),DR_VEC12,LJETS_DR,DPHI_VEC12,
     2          LJETS_DPHI)
              CALL TOP_LEPTONS_UTIL_NEARJET(Q(LPPHO_VEC(1)+9),
     1          Q(LPPHO_VEC(1)+10),DR2_VEC12,LJETS2_DR,DPHI_VEC12,
     2          LJETS_DPHI)
              CALL HFILL(6075,DR_VEC12,DR2_VEC12,1.)
            ENDIF
C
C *** 3-body Mt
C
            CALL VZERO(VECT,4)
            CALL UCOPY(VECT_CORR(1),VECT,4)
            CALL VADD(VECT2_CORR(1),VECT(1),VECT(1),4)
            MASS12=TOP_LEPTONS_UTIL_MASS4(VECT)
C
            CALL VZERO(VECT2,4)
            IF(LPNUT3.GT.0) THEN
              CALL VADD(VECT,Q(LPNUT3+3),VECT2,2)
              MT3_CLUS=( SQRT(MASS12**2+VMOD(VECT,2)**2)
     1           +VMOD(Q(LPNUT3+3),2) )**2 - VMOD(VECT2,2)**2
            ELSE
              CALL VADD(VECT,Q(LPNUT2+3),VECT2,2)
              CALL VADD(VECT2,MET_VEC(1),VECT2,2)
              MT3_CLUS=( SQRT(MASS12**2+VMOD(VECT,2)**2)
     1          +VMOD(Q(LPNUT2+3),2) )**2 - VMOD(VECT2,2)**2
            ENDIF
            IF(MT3_CLUS.GT.0.) THEN
              MT3_CLUS=SQRT(MT3_CLUS)
            ELSE
              MT3_CLUS=0.
            ENDIF
            CALL HFILL(IOFF_PHYS+143,MT3_CLUS,0.,1.)
          ENDIF
        ENDIF
        IF(TOP_FLAG(4).GT.0.OR.QCD_FLAG(4).GT.0) THEN
C
C *** top mumu event
C
          CALL TOP_LEPTONS_UTIL_MUPAIR_SIGNS(LPMUO_VEC(1),
     1      LPMUO_VEC(2),SIGN1,SIGN2,PAIR,IER)
          CALL VZERO(VECT,4)
          CALL VADD(Q(LPMUO_VEC(1)+10),VECT(1),VECT(1),4)
          CALL VADD(Q(LPMUO_VEC(2)+10),VECT(1),VECT(1),4)
          MASS12=TOP_LEPTONS_UTIL_MASS4(VECT)
          CALL HFILL(IOFF_PHYS+12,MASS12,0.,1.)
          CALL HFILL(IOFF_PHYS+132,MASS12,ETMISS_CORR,1.)
          DPHI=ABS(Q(LPMUO_VEC(1)+17)-Q(LPMUO_VEC(2)+17))
          IF(DPHI.GT.PI) DPHI=TWOPI-DPHI
          DPHI=DPHI*CONV
          CALL HFILL(IOFF_PHYS+122,Q(LPMUO_VEC(2)+14),DPHI,1.)
          CALL HFILL(IOFF_PHYS+92,MASS12,DPHI,1.)
          CALL HFILL(IOFF_PHYS+22,DPHI,0.,1.)
          CALL HFILL(IOFF_PHYS+32,ETMISS_CORR,DPHI,1.)
          CALL HFILL(IOFF_PHYS+42,Q(LPMUO_VEC(1)+14),
     1       Q(LPMUO_VEC(2)+14),1.)
          IF(I_JT.GE.1) THEN
            CALL TOP_LEPTONS_UTIL_NEARJET(Q(LPMUO_VEC(1)+16),
     1        Q(LPMUO_VEC(1)+17),DR_VEC12,LJETS_DR,DPHI_VEC12,
     2        LJETS_DPHI)
            CALL TOP_LEPTONS_UTIL_NEARJET(Q(LPMUO_VEC(2)+16),
     1        Q(LPMUO_VEC(2)+17),DR2_VEC12,LJETS2_DR,DPHI_VEC12,
     2        LJETS_DPHI)
            CALL HFILL(6072,DR_VEC12,DR2_VEC12,1.)
            IF(PAIR.EQ.0) CALL HFILL(6073,DR_VEC12,DR2_VEC12,1.)
          ENDIF
        ENDIF
        IF(TOP_FLAG(6).GT.0.OR.QCD_FLAG(6).GT.0) THEN
C
C *** top mujet event
C
 7110     IF(.NOT.MUJET_HIST) GO TO 7999
          DO J = 1,2  !Zero Mu-Jet correlations
            DPHI_MU_JET(J)=9999.
            DR_MU_JET(J)  =9999.
            NEARJETS(J)=0
          ENDDO
          DO I=1,I_MU  !IF forces exit after two muons
            MJ_MU_PT(I) = Q(LPMUO_VEC(I)+14)
            MJ_MU_ETA(I)= Q(LPMUO_VEC(I)+16)
            MJ_MU_PHI(I)= Q(LPMUO_VEC(I)+17)
            CALL HFILL(IOFF_MJ+110+ (I-1)*10,MJ_MU_PT(I),0.,1.)
            CALL HFILL(IOFF_MJ+111+ (I-1)*10,MJ_MU_ETA(I),0.,1.)
            CALL HFILL(IOFF_MJ+112+ (I-1)*10,MJ_MU_PHI(I)*CONV,0.,1.)
            CALL TOP_LEPTONS_UTIL_NEARJET(Q(LPMUO_VEC(I)+16),
     1        Q(LPMUO_VEC(I)+17),DR_MU_JET(I),LJETS_DR,DPHI_MU_JET(I),
     2        LJETS_DPHI)
            IF (I.GE.2) GO TO 7130
          ENDDO
 7130     IF(I_JT.LT.1) GO TO 7190
          DO I=1,I_JT 
            IF(CORR_JETS) THEN
              CALL TOP_LEPTONS_CORR_JETPARM(LJETS_VEC(I),JET_E,JET_ET,
     1          JET_EX,JET_EY,JET_EZ,JET_PHI,JET_ETA,IER)
              IF(IER.LT.0) THEN
                MJ_JET_ET(I)=Q(LJETS_VEC(I)+6)
                MJ_JET_ETA(I)=Q(LJETS_VEC(I)+9)
                MJ_JET_PHI(I)=Q(LJETS_VEC(I)+8)
              ELSE
                MJ_JET_ET(I)=JET_ET
                MJ_JET_ETA(I)=JET_ETA
                MJ_JET_PHI(I)=JET_PHI
              ENDIF
            ELSE
              MJ_JET_ET(I) =Q(LJETS_VEC(I)+6)
              MJ_JET_ETA(I)=Q(LJETS_VEC(I)+9)
              MJ_JET_PHI(I)=Q(LJETS_VEC(I)+8)
            ENDIF
            CALL HFILL(IOFF_MJ+130+ (I-1)*10,MJ_JET_ET(I),0.,1.)
            CALL HFILL(IOFF_MJ+131+ (I-1)*10,MJ_JET_ETA(I),0.,1.)
            CALL HFILL(IOFF_MJ+132+ (I-1)*10,MJ_JET_PHI(I)*CONV,0.,1.)
            IF (I.GE.5) GO TO 7190
          ENDDO
 7190     CONTINUE
C
          IF(I_MU.GE.1.AND.LPNUT3.NE.0) THEN
            PT1=Q(LPMUO_VEC(1)+14)
            PT2=Q(LPNUT3+7)
            DPHI=ABS(Q(LPMUO_VEC(1)+17)-Q(LPNUT3+10))
            IF(DPHI.GT.PI) DPHI=TWOPI-DPHI
            MASS12_T=TOP_LEPTONS_UTIL_MASST(PT1,PT2,DPHI)
            CALL HFILL(IOFF_MJ+210,MASS12_T,0.,1.)
            CALL HFILL(IOFF_MJ+220,PT1,PT2,1.)
            CALL HFILL(IOFF_MJ+221,DPHI*CONV,0.,1.)
            CALL HFILL(IOFF_MJ+230,PT1,HT,1.)
            CALL HFILL(IOFF_MJ+231,HT,PT2,1.)
          ENDIF
          IF(DR_MU_JET(1).LT.9999.) THEN
            CALL HFILL(IOFF_MJ+240,DPHI_MU_JET(1)*CONV,0.,1.)
            CALL HFILL(IOFF_MJ+241,DR_MU_JET(1),0.,1.)
          ENDIF
          IF(DR_MU_JET(2).LT.9999.) THEN
            CALL HFILL(IOFF_MJ+260,DPHI_MU_JET(2)*CONV,0.,1.)
            CALL HFILL(IOFF_MJ+261,DR_MU_JET(2),0.,1.)
            CALL HFILL(IOFF_MJ+262,MJ_JET_ET(NEARJETS(2)),
     1        MJ_MU_PT(2),1.)
          ENDIF
          IF(I_MU.GE.2) THEN
            DPHI = ABS(MJ_MU_PHI(1) - MJ_MU_PHI(2))
            IF(DPHI.GT.PI) DPHI=TWOPI-DPHI
            CALL HFILL(IOFF_MJ+250,DPHI*CONV,0.,1.)
            CALL HFILL(IOFF_MJ+251,MJ_MU_PT(2),MJ_MU_PT(1),1.)
            CALL VZERO(VECT,4)
            CALL VADD(Q(LPMUO_VEC(1)+10),VECT(1),VECT(1),4)
            CALL VADD(Q(LPMUO_VEC(2)+10),VECT(1),VECT(1),4)
            MASS12=TOP_LEPTONS_UTIL_MASS4(VECT)
            CALL HFILL(IOFF_MJ+252,MASS12,0.,1.)
          ENDIF
          IF(I_JT.GE.2) THEN
            CALL VZERO(VECT,4)
            CALL VADD(Q(LJETS_VEC(1)+2),VECT(1),VECT(1),4)
            CALL VADD(Q(LJETS_VEC(2)+2),VECT(1),VECT(1),4)
            MASS12=TOP_LEPTONS_UTIL_MASS4(VECT)
            CALL HFILL(IOFF_MJ+270,MASS12,0.,1.)
          ENDIF
C
 7210     IF(I_JT.GE.3) THEN
            CALL TOP_LEPTONS_EVENT_SHAPE(I_JT,LJETS_VEC,SPHER,PLAN,
     1        GSPHER,GAPLAN,GY,EMAXSH,ETMAXSH,EFOUR_SHAPE,IER)
C
            CALL HFILL(IOFF_MJ+310,SPHER,0.,1.)
            CALL HFILL(IOFF_MJ+311,PLAN,0.,1.)
            CALL HFILL(IOFF_MJ+312,PLAN,SPHER,1.)
C
            CALL HFILL(IOFF_MJ+320,GSPHER,0.,1.)
            CALL HFILL(IOFF_MJ+321,GAPLAN,0.,1.)
            CALL HFILL(IOFF_MJ+322,GAPLAN,GSPHER,1.)
            CALL HFILL(IOFF_MJ+323,GSPHER,GY,1.)
C
            CALL HFILL(IOFF_MJ+330,EMAXSH,0.,1.)
            CALL HFILL(IOFF_MJ+331,ETMAXSH,0.,1.)
            CALL HFILL(IOFF_MJ+332,ETMAXSH,EMAXSH,1.)
C
            CALL HFILL(IOFF_MJ+340,EFOUR_SHAPE,0.,1.)
C
          ENDIF
 7999   ENDIF
      ENDIF
C
C *** Look at 'W->enu' Mt 
C
      IF(I_EL.GE.1) THEN
        PT1=PELC_EMCOR(1)*Q(LPELC_VEC(1)+7)
        IF(LPNUT3.GT.0) THEN
          PT2=Q(LPNUT3+7)
          DPHI=ABS(Q(LPELC_VEC(1)+10)-Q(LPNUT3+10))
        ELSE
          PT2=((Q(LPNUT2+3)+MET_VEC(1))**2)+
     1      ((Q(LPNUT2+4)+MET_VEC(2))**2)
          PT2=SQRT(PT2)
          DPHI=ABS(Q(LPELC_VEC(1)+10)-Q(LPNUT2+10))
        ENDIF
        IF(DPHI.GT.PI) DPHI=TWOPI-DPHI
        MASS12_T=TOP_LEPTONS_UTIL_MASST(PT1,PT2,DPHI)
        CALL HFILL(IOFF_PHYS+510,MASS12_T,0.,1.)
      ENDIF
C
C *** Do same for 'photon' events
C
      IF(I_PH.GE.1) THEN
        PT1=PPHO_EMCOR(1)*Q(LPPHO_VEC(1)+7)
        IF(LPNUT3.GT.0) THEN
          PT2=Q(LPNUT3+7)
          DPHI=ABS(Q(LPPHO_VEC(1)+10)-Q(LPNUT3+10))
        ELSE
          PT2=(( Q(LPNUT2+3)+MET_VEC(1) )**2)+
     1      ((Q(LPNUT2+4)+MET_VEC(2) )**2)
          PT2=SQRT(PT2)
          DPHI=ABS(Q(LPPHO_VEC(1)+10)-Q(LPNUT2+10))
        ENDIF
        IF(DPHI.GT.PI) DPHI=TWOPI-DPHI
        MASS12_T=TOP_LEPTONS_UTIL_MASST(PT1,PT2,DPHI)
        CALL HFILL(IOFF_PHYS+511,MASS12_T,0.,1.)
      ENDIF
C
C *** Look at 'W->mu-nu' Mt
C
      IF(I_MU.GE.1) THEN
        PT1=Q(LPMUO_VEC(1)+14)
        PT2=Q(LPNUT3+7)
        DPHI=ABS(Q(LPMUO_VEC(1)+17)-Q(LPNUT3+10))
        IF(DPHI.GT.PI) DPHI=TWOPI-DPHI
        MASS12_T=TOP_LEPTONS_UTIL_MASST(PT1,PT2,DPHI)
        CALL HFILL(IOFF_PHYS+512,MASS12_T,0.,1.)
        CALL HFILL(IOFF_PHYS+211,MU_PHO_NEU_CLUSMASS,MASS12_T,1.)
      ENDIF
C
C *** Technical Quality Study Plots
C ***     a.) electrons
C
      IF(.NOT.ELECTRON_HIST) GO TO 100
      IF(I_EL.LT.1) GO TO 100
C
C *** For track match info go to ZTRK,DTRK,FDCT and VTXT Banks
C
      DO I=1,I_EL
        LDTRK=0
        LFDCT=0
        LVTXT=0
        CALL HFILL(IOFF_EL+10,Q(LPELC_VEC(I)+21),0.,1.)
        LZTRK=LQ(LPELC_VEC(I)-3)
        IF(LZTRK.GT.0) THEN
          LDTRK=LQ(LZTRK-7)
          LFDCT=LQ(LZTRK-8)
          LVTXT=LQ(LZTRK-6)
          CALL TOP_LEPTONS_UTIL_CALTRAK_ANGLES(LZTRK,
     1      ZTRAK_THETA,ZTRAK_PHI,ZTRAK_ETA)
          DETA_VEC12=Q(LPELC_VEC(I)+9)-ZTRAK_ETA
          DPHI=ABS(Q(LPELC_VEC(I)+10)-ZTRAK_PHI)
          IF(DPHI.GT.PI) DPHI=TWOPI-DPHI
          DR_VEC12=TOP_LEPTONS_UTIL_CALC_DR(DETA_VEC12,DPHI)
          IF(DPHI.GT.PI) DPHI=TWOPI-DPHI
          DTHETA=ABS(Q(LPELC_VEC(I)+8)-ZTRAK_THETA)
          IF(DTHETA.GT.PI) DTHETA=TWOPI-DTHETA
          ZTRAK_DCOS=DCOS_FROM_ETA_PHI(Q(LPELC_VEC(I)+9),
     1      Q(LPELC_VEC(I)+10),ZTRAK_ETA,ZTRAK_PHI)
        ENDIF
        IF(LDTRK.GT.0) CALL HFILL(IOFF_EL+30,Q(LDTRK+20),0.,1.)
        IF(LFDCT.GT.0) CALL HFILL(IOFF_EL+31,Q(LFDCT+20),0.,1.)
        IF(LVTXT.GT.0) CALL HFILL(IOFF_EL+32,Q(LVTXT+20),0.,1.)
C
C *** Calorimeter energy ratios
C
        CALL TOP_LEPTONS_UTIL_CALOR_RATIOS(LPELC_VEC(I),
     1    EMFRAC,EMCORE,EISOL,IER)
        CALL HFILL(IOFF_EL+20,EMFRAC,0.,1.)
        CALL HFILL(IOFF_EL+21,EISOL,0.,1.)
C
C *** HMatrix Chisquares - Longitudinal only for now
C
        LHMTC=LQ(LPELC_VEC(I)-1)
        IF(LHMTC.GT.0) THEN
          IF(Q(LPELC_VEC(I)+9).LT.2.5) THEN
            CALL HFILL(IOFF_EL+40,Q(LHMTC+7),0.,1.)
          ELSE
            CALL HFILL(IOFF_EL+41,Q(LHMTC+7),0.,1.)
          ENDIF
        ENDIF
C
C *** Electron-jet isolation
C
        IF(I_JT.GE.1) THEN
          CALL TOP_LEPTONS_UTIL_NEARJET(Q(LPELC_VEC(I)+9),
     1      Q(LPELC_VEC(I)+10),DR_VEC12,LJETS_DR,DPHI_VEC12,LJETS_DPHI)
          CALL HFILL(IOFF_EL+65,DR_VEC12,0.,1.)
          DPHI_VEC12=DPHI_VEC12*CONV
          IF(DPHI_VEC12.LT.90.) THEN
            CALL HFILL(IOFF_EL+70,Q(LJETS_DPHI+6),0.,1.)
          ENDIF
          PHI_OPP=Q(LPELC_VEC(I)+10)+PI
           IF(PHI_OPP.GT.TWOPI) PHI_OPP=PHI_OPP-TWOPI
          TEMP=-10.
          CALL TOP_LEPTONS_UTIL_NEARJET(TEMP,PHI_OPP,TEMP,ITEMP,
     1      DPHI_VEC12,LJETS_DPHI)
          DPHI_VEC12=(PI-DPHI_VEC12)*CONV
          IF(DPHI_VEC12.GT.90.) THEN
            CALL HFILL(IOFF_EL+67,DPHI_VEC12,0.,1.)
            IF(Q(LHMTC+7).GT.100.) THEN
              CALL HFILL(IOFF_EL+68,DPHI_VEC12,0.,1.)
            ELSE
              CALL HFILL(IOFF_EL+69,DPHI_VEC12,0.,1.)
            ENDIF
            CALL HFILL(IOFF_EL+71,Q(LJETS_DPHI+6),0.,1.)
          ENDIF
        ENDIF
C
      ENDDO
C ----------------------------------------------
C
C ***     b.) muons
C
  100 CONTINUE
      CALL VZERO(VECT,4)
C
      IF(.NOT.MUON_HIST) GO TO 200
      IF(I_MU.LT.1) GO TO 200
      DO I=1,I_MU
C
C *** Muon Quality Flags (IFW2,IFW4,IFW3,IFW1)
C
C *** IFW4
C
        TEMP=FLOAT(IQ(LPMUO_VEC(I)+9))
        CALL HFILL(IOFF_MU+10,TEMP,0.,1.)  
C
C *** IFW2
C
        CALL TOP_LEPTONS_UTIL_DECODE_IFW2(LPMUO_VEC(I),
     1    NO_IFW2_BITS,IFW2_BITS,IER)
        IF(NO_IFW2_BITS.EQ.0) THEN
          TEMP=-0.5
          CALL HFILL(IOFF_MU+11,TEMP,0.,1.)
        ELSE
          DO IV=1,NO_IFW2_BITS
            TEMP=FLOAT(IFW2_BITS(IV))
            IF(IFW2_BITS(IV).LT.25) THEN
              CALL HFILL(IOFF_MU+11,TEMP,0.,1.)
            ENDIF
          ENDDO
        ENDIF
C
C *** IFW3
C
        CALL TOP_LEPTONS_UTIL_DECODE_IFW3(LPMUO_VEC(I),
     1    NO_IFW3_BITS,IFW3_BITS,IER)
        IF(NO_IFW3_BITS.EQ.0) THEN
          TEMP=-0.5
          CALL HFILL(IOFF_MU+12,TEMP,0.,1.)
        ELSE
          DO IV=1,NO_IFW3_BITS
            TEMP=FLOAT(IFW3_BITS(IV))
            IF(IFW3_BITS(IV).LT.25) THEN
              CALL HFILL(IOFF_MU+12,TEMP,0.,1.)
            ENDIF
          ENDDO
        ENDIF
C
        CALL HFILL(IOFF_MU+19,Q(LPMUO_VEC(I)+23),0.,1.)
C ---------------------
        TEMP=FLOAT(IQ(LPMUO_VEC(I)+7))
        TEMP2=Q(LPMUO_VEC(I)+17)*CONV
C
C *** Time 29 - by Quadrant no.
C
        IF(MR_PERMIT.EQ.1) THEN
          IF(TEMP.EQ.1.) CALL HFILL(4,TIME29,0.,1.)
          IF(TEMP.EQ.2.) CALL HFILL(5,TIME29,0.,1.)
          IF(TEMP.EQ.3.) CALL HFILL(6,TIME29,0.,1.)
          IF(TEMP.EQ.4.) CALL HFILL(7,TIME29,0.,1.)
          IF(TEMP.GT.4.) CALL HFILL(8,TIME29,0.,1.)
          CALL HFILL(55,TEMP2,0.,1.)
          IF(TIME29.LE.1.) THEN
            CALL HFILL(58,TEMP2,0.,1.)
          ELSE
            CALL HFILL(59,TEMP2,0.,1.)
          ENDIF
        ELSE
          CALL HFILL(9,TIME29,0.,1.)
          CALL HFILL(50,TEMP2,0.,1.)
        ENDIF
C----------------------------------------------------------------
        CALL HFILL(IOFF_MU+15,TEMP,0.,1.)
        CALL HFILL(IOFF_MU+20,Q(LPMUO_VEC(I)+41),0.,1.)
        CALL HFILL(IOFF_MU+25,Q(LPMUO_VEC(I)+24),0.,1.)
        TEMP=FLOAT(IQ(LPMUO_VEC(I)+6))
        CALL HFILL(IOFF_MU+30,TEMP,0.,1.)
        CALL HFILL(IOFF_MU+32,Q(LPMUO_VEC(I)+37),0.,1.)
        TEMP=Q(LPMUO_VEC(I)+38)/CONV
        CALL HFILL(IOFF_MU+33,TEMP,0.,1.)
        TEMP=Q(LPMUO_VEC(I)+39)/CONV
        CALL HFILL(IOFF_MU+34,TEMP,0.,1.)
        LZTRK=LQ(LPMUO_VEC(I)-5)
        IF(LZTRK.GT.0) THEN
          CALL TOP_LEPTONS_UTIL_CALTRAK_ANGLES(LZTRK,
     1      ZTRAK_THETA,ZTRAK_PHI,ZTRAK_ETA)
          DETA_VEC12=Q(LPMUO_VEC(I)+16)-ZTRAK_ETA
          DPHI_VEC12=Q(LPMUO_VEC(I)+17)-ZTRAK_PHI
          IF(DPHI_VEC12.GT.PI) DPHI_VEC12=TWOPI-DPHI_VEC12
          DR_VEC12=TOP_LEPTONS_UTIL_CALC_DR(DETA_VEC12,DPHI_VEC12)
          CALL HFILL(IOFF_MU+35,DR_VEC12,0.,1.)
        ENDIF
C
C *** Cal mip monitoring
C
        CALL HFILL(IOFF_MU+40,Q(LPMUO_VEC(I)+33),0.,1.)
        CALL HFILL(IOFF_MU+41,Q(LPMUO_VEC(I)+83),0.,1.)
        CALL HFILL(IOFF_MU+42,Q(LPMUO_VEC(I)+84),0.,1.)
        CALL HFILL(IOFF_MU+43,Q(LPMUO_VEC(I)+34),0.,1.)
        CALL HFILL(IOFF_MU+47,Q(LPMUO_VEC(I)+43),0.,1.)
        PMUO_VERS=IQ(LPMUO_VEC(I)+1)
C
C *** MTCA Information
C
        IF(PMUO_VERS.GE.4) THEN
          IF(Q(LPMUO_VEC(I)+97).GT.0.1) THEN
            CALL HFILL(IOFF_MU+44,Q(LPMUO_VEC(I)+97),0.,1.)
          ENDIF
        ENDIF
        IF(PMUO_VERS.GE.3) THEN
C
C *** PMUO Bank 3 onwards specific plots
C
          CALL HFILL(IOFF_MU+46,Q(LPMUO_VEC(I)+88),0.,1.)
          CALL HFILL(IOFF_MU+23,Q(LPMUO_VEC(I)+56),0.,1.)
          CALL HFILL(IOFF_MU+24,Q(LPMUO_VEC(I)+57),0.,1.)
C
C *** For hit multiplicities, use hits use in fit only
C
          ICALL=2
          CALL TOP_LEPTONS_UTIL_DECODE_PLANES(LPMUO_VEC(I),
     1      ICALL,WAM_HIT,SAM_HIT)
          ITEMP=WAM_HIT(1)+WAM_HIT(2)+WAM_HIT(3)+WAM_HIT(4)
     1      +WAM_HIT(5)+WAM_HIT(6)
          JTEMP=SAM_HIT(1)+SAM_HIT(2)+SAM_HIT(3)
          TEMP=FLOAT(ITEMP)
          CALL HFILL(IOFF_MU+50,TEMP,0.,1.)
          TEMP=FLOAT(JTEMP)
          CALL HFILL(IOFF_MU+51,TEMP,0.,1.)
          CALL HFILL(IOFF_MU+53,TEMP,Q(LPMUO_VEC(I)+16),1.)
        ENDIF
C
        E2=Q(LPMUO_VEC(I)+34)
        E4=Q(LPMUO_VEC(I)+35)
        E6=Q(LPMUO_VEC(I)+36)
        ET2=E2*SIN(Q(LPMUO_VEC(I)+15))
        ET4=E4*SIN(Q(LPMUO_VEC(I)+15))
        TEMP=E4-E2
        TEMPT=ET4-ET2
        CALL HFILL(IOFF_MU+80,TEMP,0.,1.)
        IF(IQ(LPMUO_VEC(I)+7).LT.5) THEN
          IF(ABS( Q(LPMUO_VEC(I)+16) ).LT.0.5) THEN
            CALL HFILL(IOFF_MU+83,TEMP,0.,1.)
            CALL HFILL(IOFF_MU+86,TEMPT,0.,1.)
          ELSE
            CALL HFILL(IOFF_MU+84,TEMP,0.,1.)
            CALL HFILL(IOFF_MU+87,TEMPT,0.,1.)            
          ENDIF
        ELSE
          CALL HFILL(IOFF_MU+85,TEMP,0.,1.)
          CALL HFILL(IOFF_MU+88,TEMPT,0.,1.)
        ENDIF  
        TEMP=E6-E2
        CALL HFILL(IOFF_MU+81,TEMP,0.,1.)
C
C *** Get Corresponding MUOT Bank
C
        LMUOT=LQ(LPMUO_VEC(I)-2)
        IF(LMUOT.GT.0) THEN
          IF(PMUO_VERS.LT.3) THEN
C
C *** PMUO Bank Version 1,2 specific plots
C
            RZ_IMPACT=0.
            XY_IMPACT=0.
            TEMP=SQRT(Q(LMUOT+17)**2+Q(LMUOT+18)**2)
            IF(TEMP.GT.0.) THEN
              XY_IMPACT=(Q(LMUOT+11)*Q(LMUOT+18)-
     1          Q(LMUOT+12)*Q(LMUOT+17))/TEMP
            ENDIF
            CALL HFILL(IOFF_MU+23,RZ_IMPACT,0.,1.)
            CALL HFILL(IOFF_MU+24,XY_IMPACT,0.,1.)
            TEMP=FLOAT(IQ(LMUOT+1))
            CALL HFILL(IOFF_MU+50,TEMP,0.,1.)
            TEMP=FLOAT(IQ(LMUOT+2))
            CALL HFILL(IOFF_MU+51,TEMP,0.,1.)
            TEMP=TEMP+FLOAT(IQ(LMUOT+1))
            CALL HFILL(IOFF_MU+52,TEMP,0.,1.)
            CALL HFILL(IOFF_MU+53,TEMP,Q(LPMUO_VEC(I)+16),1.)
            TEMP2=IQ(LPMUO_VEC(I)+7)
            CALL HFILL(IOFF_MU+54,TEMP,TEMP2,1.)
          ENDIF
          CALL HFILL(IOFF_MU+16,Q(LMUOT+22),0.,1.)
          CALL HFILL(IOFF_MU+17,Q(LMUOT+20),0.,1.)
          CALL HFILL(IOFF_MU+18,Q(LMUOT+21),0.,1.)
          TEMP=FLOAT(IQ(LMUOT+4))
          CALL HFILL(IOFF_MU+13,TEMP,0.,1.)
        ENDIF
C
C *** jet opposite information
C
        IF(I_JT.GE.1) THEN
          CALL TOP_LEPTONS_UTIL_NEARJET(Q(LPMUO_VEC(I)+16),
     1      Q(LPMUO_VEC(I)+17),DR_VEC12,LJETS_DR,DPHI_VEC12,LJETS_DPHI)
          IF(CORR_JETS) THEN
            CALL TOP_LEPTONS_CORR_JETPARM(LJETS_DR,JET_E,JET_ET,
     1        JET_EX,JET_EY,JET_EZ,JET_PHI,JET_ETA,IER)
            IF(IER.LT.0) THEN
                JET_ET=Q(LJETS_DR+6)
            ENDIF
          ELSE
            JET_ET=Q(LJETS_DR+6)
          ENDIF
          CALL HFILL(IOFF_MU+65,DR_VEC12,0.,1.)
C
          IF(CORR_JETS) THEN
            CALL TOP_LEPTONS_CORR_JETPARM(LJETS_DPHI,JET_E,JET_ET,
     1        JET_EX,JET_EY,JET_EZ,JET_PHI,JET_ETA,IER)
            IF(IER.LT.0) THEN
                JET_ET=Q(LJETS_DPHI+6)
            ENDIF
          ELSE
            JET_ET=Q(LJETS_DPHI+6)
          ENDIF
          DPHI_VEC12=DPHI_VEC12*CONV
          IF(DPHI_VEC12.LT.90.) THEN
            CALL HFILL(IOFF_MU+70,Q(LJETS_DPHI+6),0.,1.)
          ENDIF
          PHI_OPP=Q(LPMUO_VEC(I)+17)+PI
          IF(PHI_OPP.GT.TWOPI) PHI_OPP=PHI_OPP-TWOPI
          TEMP=-10.
          CALL TOP_LEPTONS_UTIL_NEARJET(TEMP,PHI_OPP,TEMP,ITEMP,
     1      DPHI_VEC12,LJETS_DPHI)
          IF(CORR_JETS) THEN
            CALL TOP_LEPTONS_CORR_JETPARM(LJETS_DPHI,JET_E,JET_ET,
     1        JET_EX,JET_EY,JET_EZ,JET_PHI,JET_ETA,IER)
            IF(IER.LT.0) THEN
                JET_ET=Q(LJETS_DPHI+6)
            ENDIF
          ELSE
            JET_ET=Q(LJETS_DPHI+6)
          ENDIF
          DPHI_VEC12=(PI-DPHI_VEC12)*CONV
          IF(DPHI_VEC12.GT.90.) THEN
            CALL HFILL(IOFF_MU+67,DPHI_VEC12,0.,1.)
            CALL HFILL(IOFF_MU+71,JET_ET,0.,1.)
          ENDIF
        ENDIF
      ENDDO
  200 CONTINUE
C
C *** Dimuon Technical Study Plots
C
      IF(.NOT.DIMUON_HIST) GO TO 250
      IF(I_MU.LT.2) GO TO 250
C
      CALL TOP_LEPTONS_UTIL_MUPAIR_SIGNS(LPMUO_VEC(1),LPMUO_VEC(2),
     1  SIGN1,SIGN2,PAIR,IER)
C
      CALL VZERO(VECT,4)
      CALL VADD(Q(LPMUO_VEC(1)+10),VECT(1),VECT(1),4)
      CALL VADD(Q(LPMUO_VEC(2)+10),VECT(1),VECT(1),4)
      MASS12=TOP_LEPTONS_UTIL_MASS4(VECT)
      PHI12=ATAN2(VECT(2),VECT(1))
      IF(PHI12.LT.0) PHI12=PHI12+TWOPI
      PT12_LEP=SQRT(VECT(1)**2+VECT(2)**2)
      PT12_CALO=ETMISS
C
C *** plot phi of dimuon pair
C
      CALL HFILL(IOFF_MU+210,PHI12*CONV,0.,1.)
C
C *** plot dphi between dimuon Pt vector and Etmiss (cal&corr)
C
      DPHI12_ECAL=ABS(PHI12-Q(LPNUT2+10))
      IF(DPHI12_ECAL.GT.PI) DPHI12_ECAL=TWOPI-DPHI12_ECAL
      CALL HFILL(IOFF_MU+211,DPHI12_ECAL*CONV,0.,1.)
C
      DPHI12_ECORR=ABS(PHI12-ETMISS_CORR_PHI)
      IF(DPHI12_ECORR.GT.PI) DPHI12_ECORR=TWOPI-DPHI12_ECORR
      CALL HFILL(IOFF_MU+212,DPHI12_ECORR*CONV,0.,1.)
C 
      DPHI=ABS(Q(LPMUO_VEC(1)+17)-Q(LPMUO_VEC(2)+17))
      IF(DPHI.GT.PI) DPHI=TWOPI-DPHI
      DPHI=DPHI*CONV
C      
      CALL HFILL(IOFF_MU+100,Q(LPMUO_VEC(1)+14),Q(LPMUO_VEC(2)+14),
     1  1.)
      CALL HFILL(IOFF_MU+110,DPHI,0.,1.)
      CALL HFILL(IOFF_MU+120,ETMISS_CORR,DPHI,1.)
      CALL HFILL(IOFF_MU+130,MASS12,0.,1.)
      IF(DPHI.LT.45.) CALL HFILL(IOFF_MU+140,MASS12,0.,1.)
      IF(DPHI.GE.45..AND.DPHI.LT.135.) CALL HFILL(IOFF_MU+150,MASS12,0.,
     1  1.)
      IF(DPHI.GE.135.) CALL HFILL(IOFF_MU+160,MASS12,0.,1.)
      CALL HFILL(IOFF_MU+170,PT12_LEP,0.,1.)
      CALL HFILL(IOFF_MU+180,PT12_CALO,0.,1.)
C
C *** dphi(max):mu,Et(miss3)
C
      DPHI_1=ABS(Q(LPMUO_VEC(1)+17)-ETMISS_CORR_PHI)
      IF(DPHI_1.GT.PI) DPHI_1=TWOPI-DPHI_1
      DPHI_2=ABS(Q(LPMUO_VEC(2)+17)-ETMISS_CORR_PHI)
      IF(DPHI_2.GT.PI) DPHI_2=TWOPI-DPHI_2
      IF(DPHI_1.GT.DPHI_2) THEN
        CALL HFILL(IOFF_MU+26,DPHI_1*CONV,0.,1.)
        CALL HFILL(IOFF_MU+27,DPHI_1*CONV,ETMISS_CORR,1.)
       ELSE
        CALL HFILL(IOFF_MU+26,DPHI_2*CONV,0.,1.)
        CALL HFILL(IOFF_MU+27,DPHI_2*CONV,ETMISS_CORR,1.)
      ENDIF
      IF(PAIR.EQ.0) THEN
C
C *** +- pairs
C
        CALL HFILL(IOFF_MU+101,Q(LPMUO_VEC(1)+14),Q(LPMUO_VEC(2)+14),
     1    1.)
        CALL HFILL(IOFF_MU+111,DPHI,0.,1.)
        CALL HFILL(IOFF_MU+121,ETMISS_CORR,DPHI,1.)
        CALL HFILL(IOFF_MU+131,MASS12,0.,1.)
        CALL HFILL(IOFF_MU+171,PT12_LEP,0.,1.)
        CALL HFILL(IOFF_MU+181,PT12_CALO,0.,1.)
      ELSE
C
C *** ++,-- pairs
C
        CALL HFILL(IOFF_MU+102,Q(LPMUO_VEC(1)+14),Q(LPMUO_VEC(2)+14),
     1    1.)
        CALL HFILL(IOFF_MU+112,DPHI,0.,1.)
        CALL HFILL(IOFF_MU+122,ETMISS_CORR,DPHI,1.)
        CALL HFILL(IOFF_MU+132,MASS12,0.,1.)
        CALL HFILL(IOFF_MU+172,PT12_LEP,0.,1.)
        CALL HFILL(IOFF_MU+182,PT12_CALO,0.,1.)
      ENDIF
C
  250 CONTINUE
C
C ***  ----------------------------------------------------------------------
C ***  Photon study distributions
C
      IF(.NOT.PHOTON_HIST) GO TO 300
      IF(I_PH.LT.1) GO TO 300
C
C *** Photon diagnostic plots
C
  300 CONTINUE
      IF(I_PH.LT.1) GO TO 400
C
C *** For track match info go to ZTRK,DTRK,FDCT and VTXT Banks
C
      DO I=1,I_PH
C
C *** Calorimeter energy ratios
C
        CALL TOP_LEPTONS_UTIL_CALOR_RATIOS(LPPHO_VEC(I),
     1    EMFRAC,EMCORE,EISOL,IER)
        CALL HFILL(IOFF_PH+20,EMFRAC,0.,1.)
        CALL HFILL(IOFF_PH+21,EISOL,0.,1.)
C
C *** HMatrix Chisquares - Longitudinal only for now
C
        LHMTC=LQ(LPPHO_VEC(I)-1)
        IF(LHMTC.GT.0) THEN
          IF(Q(LPPHO_VEC(I)+9).LT.2.5) THEN
            CALL HFILL(IOFF_PH+40,Q(LHMTC+7),0.,1.)
          ELSE
            CALL HFILL(IOFF_PH+41,Q(LHMTC+7),0.,1.)
          ENDIF
        ENDIF
C
C *** Photon-jet isolation
C
        IF(I_JT.GE.1) THEN
          CALL TOP_LEPTONS_UTIL_NEARJET(Q(LPPHO_VEC(I)+9),
     1      Q(LPPHO_VEC(I)+10),DR_VEC12,LJETS_DR,DPHI_VEC12,LJETS_DPHI)
          CALL HFILL(IOFF_PH+65,DR_VEC12,0.,1.)
          DPHI_VEC12=DPHI_VEC12*CONV
          IF(DPHI_VEC12.LT.90.) THEN
            CALL HFILL(IOFF_PH+70,Q(LJETS_DPHI+6),0.,1.)
          ENDIF
          PHI_OPP=Q(LPPHO_VEC(I)+10)+PI
          IF(PHI_OPP.GT.TWOPI) PHI_OPP=PHI_OPP-TWOPI
          TEMP=-10.
          CALL TOP_LEPTONS_UTIL_NEARJET(TEMP,PHI_OPP,TEMP,ITEMP,
     1      DPHI_VEC12,LJETS_DPHI)
          DPHI_VEC12=(PI-DPHI_VEC12)*CONV
          IF(DPHI_VEC12.GT.90.) THEN
            CALL HFILL(IOFF_PH+67,DPHI_VEC12,0.,1.)
            IF(Q(LHMTC+7).GT.100.) THEN
              CALL HFILL(IOFF_PH+68,DPHI_VEC12,0.,1.)
            ELSE
              CALL HFILL(IOFF_PH+69,DPHI_VEC12,0.,1.)
            ENDIF
            CALL HFILL(IOFF_PH+71,Q(LJETS_DPHI+6),0.,1.)
          ENDIF
        ENDIF
C
      ENDDO
  400 CONTINUE
C----------------------------------------------------------------------
  999 RETURN
      END
