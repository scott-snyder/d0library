      SUBROUTINE TOP_LEPTONS_FIND_MUMU(IFGOOD,NOMU,NOEL,NOPH,
     1  NOJT,NOMU_UNCUT,MET_VEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : event search routine to look for top->mu mu
C-                                event candidates
C-
C-   Inputs  :
C-             NOMU         - no of 'good' PMUO candidates
C-             NOMU_UNCUT   - no of 'raw' PMUO candidates
C-             NOEL         - no of 'good' PELC candidates
C-             NOPH         - no of 'good' PPHO candidates
C-             NOJT         - no of 'good' JETS candidates
C-
C-   RCP file paramters :
C-
C-             CORR_JETS    - .TRUE./.FALSE. do/dont apply jet
C-                               corrections
C-             DPHI_MAX_LEP12  - max allowed dphi(lepton1-lepton2) in deg
C-             NOJT_MIN        - minimum jet multiplicty (after cuts)
C-             PTMIN_JET1      - Ptmin cut for leading jet
C-             PTMIN_JET2      - Ptmin cut for second jet
C-             MISSET_MIN_CORR - minimum missing Et (muon corrected)
C-             ETMIN_JET_DRMIN - minimum jet Et for isolation(dR min)
C-             EF_DETA_CUT     - back to back eta cut on EF muons when no CF mu
C-
C-
C-
C-   Outputs :
C-              IFGOOD = .TRUE./.FALSE. - event is/isnt a candidate
C-
C-   Controls:
C-
C-   Created  19-JUL-1992   Stephen J. Wimpenny
C-   Modified 21-JUL-1992   Di-muon selection code implemented
C-                              (Pt cuts only)
C-   Modified 15-Sep-1992   Modified to use Good_Muon Logical
C-   Modified 17-Sep-1992   Read cuts directly from RCP file
C-   Modified 19-Oct-1992   Looks for additional 'poor' quality muons
C-                          if one good muon is found
C-   Modified 28-Jan-1993   Jet Et and multiplicity cuts added
C-   Modified 10-Feb-1993   Updated for Version 3 PMUO compatibility
C-   Modified 17-Feb-1993   dPhi(max) mu-mu cut added
C-   Modified 16-Mar-1993   Change in names of Good_Muon, Good_Jet logicals
C-   Modified 28-Mar-1993   Muon EF Level2 cut handling changed for PMUO
C-                          bank versions before V 3.0
C-   Modified 29-Mar-1993   dR(mu1-jet) and dR(mu2-jet) min cuts added
C-   Modified  1-Apr-1993   Silver Muon logic modified
C-   Modified 22-Apr-1993   Jet Energy corrections added
C-   Modified 16-Jun-1993   Missing Et re-calculated if silver muon(s) are
C-                          used
C-   Modified 16-Jul-1993   Uses new muon isolation code + missing Et
C-                          corrections due to Jet and em corr.
C-   Modified 22-Jul-1993   muon dE/dx correction added for silver muons
C-   Modified 18-Aug-1993   RE Hall Put in dphi:dimu,Et(miss3) cuts
C-   Modified 02-NOV-1994   RE Hall remove silver muon code
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      EXTERNAL TOP_LEPTONS_GOOD_MUON,TOP_LEPTONS_GOOD_JET
C
      LOGICAL FIRST,IFGOOD,CORR_JETS,CORR_MU
      LOGICAL TOP_LEPTONS_GOOD_MUON,TOP_LEPTONS_GOOD_JET
      LOGICAL DO_MASS12_CUT,DO_DPHI12_CUT
      LOGICAL DO_DPHI12_MIN_CUT
      LOGICAL DO_EF_COSMIC_CUT,DO_DPHILEM_CUT
      LOGICAL DO_DPHI2LTEM_CUT,DO_DPHI3LTEM_CUT
      LOGICAL DO_ZFIT_CHISQ_CUT
      LOGICAL DO_DETA_DPHI_CUT
      LOGICAL DO_DPHI_PNUT3_CUT
C
      INTEGER IV,IER,I_SET,I_RESET,VERSION,MODE
      INTEGER NOMU,NOEL,NOPH,NOJT,NOJT_MIN,NOMU_UNCUT
      INTEGER LPMUO,LPNUT2,LPNUT3,LJETS,GZPMUO,GZPNUT,GZJETS
      INTEGER LPMUO_VEC(5),I_MU
      INTEGER LJETS_VEC(10),I_JT
      INTEGER I_MU_ISOL,LPMUO_VEC_ISOL(5)
      INTEGER LPROC,GZPROC,LFIT2
      INTEGER ITEMP,WAM_HIT(6),SAM_HIT(6)
C
      REAL PTMIN_JET1,PTMIN_JET2
      REAL DR_MU_CORE,DR_MU_ISOL
      REAL MISSET_MIN_CORR,MISSET_CORR
      REAL MISSET_MIN_CALO,MISSET_CORR2
      REAL DPHI_LEP12,DPHI_MAX_LEP12,DPHI_MIN_LEP12
      REAL PI,TWOPI,CONV,ET1,ET2,JET1_ET,JET2_ET
      REAL JETS_EX,JETS_EY,JETS_EZ,JETS_E,JETS_PHI,JETS_ETA
      REAL MET_VEC(3)
      REAL EF_DETA_CUT,EF_DETA
      REAL DPHI_MIN_LEMISS,MISSET_CORR_PHI
      REAL DPHI_MAX_2LTEMISS,DPHI_MAX_3LTEMISS
      REAL PHI12,DPHI12_ECAL,VECT(4)
      REAL MET2_X,MET2_Y,MISSET_PHI
      REAL MIN_ZFIT_CHISQ,ZFIT_CHISQ
      REAL TOP_LEPTONS_UTIL_MASS4
      REAL MASS_L12_MIN,MASS_L12_MAX,MASS12
      REAL MAX_EDIF_CONES_CF,MAX_EDIF_CONES_EF
      REAL DPHI_1
      REAL DETA12,DETA12_CUT
      REAL ASSOC_PNUT3_ETMIN,ASSOC_DPHI_L12 
C
      DATA PI,TWOPI,CONV/3.141593,6.283185,57.2958/
      DATA FIRST/.TRUE./
      DATA MODE/ 1/
C
C *** Read cut parameters from RCP file
C
      IER=0
      IF(FIRST) THEN
        CALL EZPICK('TOP_LEPTONS_RCP')
C
        CALL EZGET_l('DO_DPHI12_CUT',DO_DPHI12_CUT,IER)
        IF (IER.EQ.0) CALL EZGET_l('DO_DPHI12_MIN_CUT'
     &     ,DO_DPHI12_MIN_CUT,IER)
        IF (IER.EQ.0) CALL EZGET_l('MU_CORR',CORR_MU,IER)
        IF (IER.EQ.0) CALL EZGET('CORECONE_SIZE',DR_MU_CORE,IER)
        IF (IER.EQ.0) CALL EZGET('ISOCONE_SIZE',DR_MU_ISOL,IER)
        IF (IER.EQ.0) CALL EZGET('ISOCONE_CUT_CF',MAX_EDIF_CONES_CF,IER)
        IF (IER.EQ.0) CALL EZGET('ISOCONE_CUT_EF',MAX_EDIF_CONES_EF,IER)
        IF (IER.EQ.0) CALL EZGET('MAX_DPHI_L12',DPHI_MAX_LEP12,IER)
        IF (IER.EQ.0) CALL EZGET('MIN_DPHI_L12',DPHI_MIN_LEP12,IER)
        IF (IER.EQ.0) CALL EZGET_l('DO_MASS12_CUT',DO_MASS12_CUT,IER)
        IF (IER.EQ.0) CALL EZGET('MASS_L12_MIN',MASS_L12_MIN,IER)
        IF (IER.EQ.0) CALL EZGET('MASS_L12_MAX',MASS_L12_MAX,IER)
        IF (IER.EQ.0) CALL EZGET('PNUT2_ETMIN',MISSET_MIN_CALO,IER)
        IF (IER.EQ.0) CALL EZGET('PNUT3_ETMIN',MISSET_MIN_CORR,IER)
        IF (IER.EQ.0) CALL EZGET_l('JETS_CORR',CORR_JETS,IER)
        IF (IER.EQ.0) CALL EZGET_i('JET_MULT_MIN',NOJT_MIN,IER)
        IF (IER.EQ.0) CALL EZGET('JET1_PTMIN',PTMIN_JET1,IER)
        IF (IER.EQ.0) CALL EZGET('JET2_PTMIN',PTMIN_JET2,IER)
        IF (IER.EQ.0) CALL EZGET_l('DO_EF_COSMIC_CUT'
     &     ,DO_EF_COSMIC_CUT,IER)
        IF (IER.EQ.0) CALL EZGET('EF_DETA_CUT',EF_DETA_CUT,IER)
        IF (IER.EQ.0) CALL EZGET_l('DO_DPHI_PNUT3_CUT',DO_DPHI_PNUT3_CUT
     &       ,IER)
        IF (IER.EQ.0) CALL EZGET('ASSOC_PNUT3_ETMIN',ASSOC_PNUT3_ETMIN,
     &     IER)
        IF (IER.EQ.0) CALL EZGET('ASSOC_DPHI_L12',ASSOC_DPHI_L12,
     &     IER)
        IF (IER.EQ.0) CALL EZGET_l('DO_DPHI2LTEM_CUT',DO_DPHI2LTEM_CUT,
     &     IER)
        IF (IER.EQ.0) CALL EZGET('MAX_DPHI_2LTEMISS',DPHI_MAX_2LTEMISS,
     &     IER)
        IF (IER.EQ.0) CALL EZGET_l('DO_DPHI3LTEM_CUT',DO_DPHI3LTEM_CUT,
     &     IER)
        IF (IER.EQ.0) CALL EZGET('MAX_DPHI_3LTEMISS',DPHI_MAX_3LTEMISS,
     &     IER)
        IF (IER.EQ.0) CALL EZGET_l('DO_DPHILEM_CUT',DO_DPHILEM_CUT,IER)
        IF (IER.EQ.0) CALL EZGET('MIN_DPHI_LEMISS',DPHI_MIN_LEMISS,IER)
        IF (IER.EQ.0) CALL EZGET_l('DO_ZFIT_CHISQ_CUT',DO_ZFIT_CHISQ_CUT
     &       ,IER)
        IF (IER.EQ.0) CALL EZGET('MIN_ZFIT_CHISQ',MIN_ZFIT_CHISQ,IER)
        IF (IER.EQ.0) CALL EZGET_l('DO_DETA_DPHI_CUT'
     &     ,DO_DETA_DPHI_CUT,IER)
        IF (IER.EQ.0) CALL EZGET('DETA12_CUT',DETA12_CUT,IER)
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error getting RCP parameters',
     &    'TOP_LEPTONS_FIND_MUMU',' ','F')
        FIRST=.FALSE.
      ENDIF
C
      IFGOOD=.FALSE.
      I_MU=0
      I_MU_ISOL=0
      I_JT=0
      I_SET=1
      I_RESET=0
C
C *** Get all relevent bank pointers ***
C
C *** Start by extracting Bank pointers for golden muon tracks
C *** PMUO
C
      LPMUO=GZPMUO(0)
      DO WHILE(LPMUO.NE.0)
        VERSION=IQ(LPMUO+1)
        IF(TOP_LEPTONS_GOOD_MUON(LPMUO)) THEN
          I_MU=I_MU+1
          IF(I_MU.LT.6) LPMUO_VEC(I_MU)=LPMUO
   10     CONTINUE
        ENDIF
        LPMUO=LQ(LPMUO)
      ENDDO
C
      IF(I_MU.GT.5) CALL ERRMSG('Muon Store Truncated at 5',
     1 'TOP_LEPTONS_FIND_MUMU',' ','W')
C
C *** check how many golden PMUO muons we have if less than 1 drop out
C
      IF(I_MU.LT.1) GO TO 999
C
C *** PNUT3
C
      MISSET_CORR=0.
      LPNUT3=GZPNUT(3)
      IF(LPNUT3.NE.0) THEN
        MISSET_CORR=Q(LPNUT3+7)
        MISSET_CORR_PHI=Q(LPNUT3+10)
      ENDIF
C
C *** PNUT2
C
      MISSET_CORR2=0.
      LPNUT2=GZPNUT(4)
      IF(LPNUT2.LE.0) LPNUT2=GZPNUT(2)
      IF(LPNUT2.NE.0) THEN
        MISSET_PHI=Q(LPNUT2+10)
        MET2_X=Q(LPNUT2+3)+MET_VEC(1)
        MET2_Y=Q(LPNUT2+4)+MET_VEC(2)
        MISSET_CORR2=SQRT(MET2_X**2+MET2_Y**2)
      ENDIF
C
C *** JETS
C
      LJETS=GZJETS()
      DO WHILE(LJETS.GT.0)
        IF(TOP_LEPTONS_GOOD_JET(LJETS)) THEN
          I_JT=I_JT+1
          IF(I_JT.LT.11) LJETS_VEC(I_JT)=LJETS
        ENDIF
        LJETS=LQ(LJETS)
      ENDDO
      IF(I_JT.GT.10) I_JT=10
C
C ---------------------------------------------------------------------------
C *** check how many golden and isolated PMUO banks we have
C *** above the Pt threshold
C
      CALL TOP_LEPTONS_FIND_ISOLMU(I_MU,LPMUO_VEC,I_MU_ISOL,
     1  LPMUO_VEC_ISOL,I_JT,MODE)
C
C *** need two muons for 2, otherwise exit
C
      IF(I_MU_ISOL.LT.2) GO TO 999
C
C *** Corrected Missing Et
C
      IF(MISSET_CORR2.LT.MISSET_MIN_CALO) GO TO 999
      IF(MISSET_CORR.LT.MISSET_MIN_CORR) GO TO 999
C
C *** dPhi max (muon1-muon2)
C
      DPHI_LEP12=ABS(Q(LPMUO_VEC_ISOL(1)+17)-Q(LPMUO_VEC_ISOL(2)+17))
      IF(DPHI_LEP12.GT.PI) DPHI_LEP12=TWOPI-DPHI_LEP12
      DPHI_LEP12=DPHI_LEP12*CONV
      IF(DO_DPHI12_CUT) THEN
        IF(DPHI_LEP12.GT.DPHI_MAX_LEP12) GO TO 999
      ENDIF
C
C *** dPhi max & dEta max (muon1-muon2)
C
      IF(DO_DETA_DPHI_CUT) THEN
        DETA12=ABS(Q(LPMUO_VEC_ISOL(1)+16)+Q(LPMUO_VEC_ISOL(2)+16))
        IF(DETA12.LT.DETA12_CUT.AND
     &     .DPHI_LEP12.GT.DPHI_MAX_LEP12) GO TO 999
      ENDIF
C
C *** dPhi max (muon1-muon2) & PNUT3 min cut 
C
      IF(DO_DPHI_PNUT3_CUT) THEN
        IF(MISSET_CORR.LT.ASSOC_PNUT3_ETMIN.AND
     &     .DPHI_LEP12.GT.ASSOC_DPHI_L12) GO TO 999
      ENDIF
C
C *** dPhi min (muon1-muon2)
C
      IF(DO_DPHI12_MIN_CUT) THEN
        IF(DPHI_LEP12.LT.DPHI_MIN_LEP12) GO TO 999
      ENDIF
C
C
C *** dphi:dimuon pt, Et(miss2)
C
      IF(DO_DPHILEM_CUT) THEN
        CALL VZERO(VECT,4)
        CALL VADD(Q(LPMUO_VEC_ISOL(1)+10),VECT(1),VECT(1),4)
        CALL VADD(Q(LPMUO_VEC_ISOL(2)+10),VECT(1),VECT(1),4)
        PHI12=ATAN2(VECT(2),VECT(1))
        IF(PHI12.LT.0) PHI12=PHI12+TWOPI
        DPHI12_ECAL=ABS(PHI12-MISSET_PHI)
        IF(DPHI12_ECAL.GT.PI) DPHI12_ECAL=TWOPI-DPHI12_ECAL
        DPHI12_ECAL=CONV*DPHI12_ECAL
        IF(DPHI12_ECAL.LT.DPHI_MIN_LEMISS) GOTO 999
      ENDIF
C
C
C *** dphi:muon(1)pt,Et(miss3) 2layer and/or 3layer tracks max cut
C
      IF(DO_DPHI2LTEM_CUT.OR.DO_DPHI3LTEM_CUT) THEN
        DPHI_1=ABS(Q(LPMUO_VEC_ISOL(1)+17)-MISSET_CORR_PHI)
        IF(DPHI_1.GT.PI) DPHI_1=TWOPI-DPHI_1
        DPHI_1=DPHI_1*CONV
        ITEMP=2
        CALL TOP_LEPTONS_UTIL_DECODE_PLANES(LPMUO_VEC_ISOL(1),
     &                              ITEMP,WAM_HIT,SAM_HIT)
        IF(DO_DPHI2LTEM_CUT) THEN
          IF(WAM_HIT(1).LT.1.OR.WAM_HIT(2).LT.1.OR.
     &       WAM_HIT(3).LT.1) THEN
            IF(DPHI_1.GT.DPHI_MAX_2LTEMISS) GOTO 999
          ENDIF
        ENDIF
        IF(DO_DPHI3LTEM_CUT) THEN
          IF(WAM_HIT(1).GT.0.AND.WAM_HIT(2).GT.0.AND.
     &       WAM_HIT(3).GT.0) THEN
            IF(DPHI_1.GT.DPHI_MAX_3LTEMISS) GOTO 999
          ENDIF
        ENDIF
      ENDIF
C
C *** dimuon mass cut
C
      IF(DO_MASS12_CUT) THEN
        CALL VZERO(VECT,4)
        CALL VADD(Q(LPMUO_VEC_ISOL(1)+10),VECT(1),VECT(1),4)
        CALL VADD(Q(LPMUO_VEC_ISOL(2)+10),VECT(1),VECT(1),4)
        MASS12=TOP_LEPTONS_UTIL_MASS4(VECT)
        IF(MASS12.LT.MASS_L12_MIN.OR.MASS12.GT.MASS_L12_MAX) GO TO 999
      ENDIF
C
C
C *** Z fit Chi-square cut
C
      IF(DO_ZFIT_CHISQ_CUT) THEN
        LPROC=GZPROC()
        LFIT2=LQ(LPROC-12)
        IF(LFIT2.GT.0) THEN
          ZFIT_CHISQ=Q(LFIT2+4)
        ELSE
          ZFIT_CHISQ=0
        ENDIF
        IF(MIN_ZFIT_CHISQ.GT.ZFIT_CHISQ) GO TO 999
      ENDIF
C
C *** EF Cosmic rejection -B2B eta cut
C
      IF(DO_EF_COSMIC_CUT) THEN
        DO IV=1,I_MU
           IF(ABS(Q(LPMUO_VEC_ISOL(IV)+16)).LT.0.9) GO TO 120
        END DO
        EF_DETA=ABS(Q(LPMUO_VEC_ISOL(1)+16)+Q(LPMUO_VEC_ISOL(2)+16))
        IF(EF_DETA.LT.EF_DETA_CUT) GO TO 999
      ENDIF
  120 CONTINUE
C
C *** Jet multiplicity and Et selection
C
      IF(NOJT_MIN.LT.1) GO TO 200
      IF(I_JT.LT.NOJT_MIN) GO TO 999
      IF(I_JT.GT.1) THEN
C
C *** Apply Jet corrections if requested
C
        IF(CORR_JETS) THEN
          CALL TOP_LEPTONS_CORR_JETPARM(LJETS_VEC(1),JETS_E,ET1,
     1      JETS_EX,JETS_EY,JETS_EZ,JETS_PHI,JETS_ETA,IER)
          CALL TOP_LEPTONS_CORR_JETPARM(LJETS_VEC(2),JETS_E,ET2,
     1      JETS_EX,JETS_EY,JETS_EZ,JETS_PHI,JETS_ETA,IER)
          IF(IER.GT.0) THEN
            JET1_ET=ET1
            JET2_ET=ET2
          ELSE
            JET1_ET=Q(LJETS_VEC(1)+6)
            JET2_ET=Q(LJETS_VEC(2)+6)
          ENDIF
        ELSE
          JET1_ET=Q(LJETS_VEC(1)+6)
          JET2_ET=Q(LJETS_VEC(2)+6)
        ENDIF
        IF(JET1_ET.LT.PTMIN_JET1.OR.JET2_ET
     1    .LT.PTMIN_JET2) GO TO 999
      ELSEIF(I_JT.GT.0) THEN
C
C *** Apply Jet corrections if requested
C
        IF(CORR_JETS) THEN
          CALL TOP_LEPTONS_CORR_JETPARM(LJETS_VEC(1),JETS_E,ET1,
     1      JETS_EX,JETS_EY,JETS_EZ,JETS_PHI,JETS_ETA,IER)
          IF(IER.GT.0) THEN
            JET1_ET=ET1
          ELSE
            JET1_ET=Q(LJETS_VEC(1)+6)
          ENDIF
        ELSE
          JET1_ET=Q(LJETS_VEC(1)+6)
        ENDIF
        IF(JET1_ET.LT.PTMIN_JET1) GO TO 999
      ENDIF
  200 CONTINUE
C
C *** Good candidate
C
      IFGOOD=.TRUE.
C
C ---------------------------------------------------------------------------
  999 RETURN
      END
