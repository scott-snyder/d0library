      SUBROUTINE TOP_LEPTONS_FIND_WPAIR_MUMU(IFGOOD,NOMU,NOEL,NOPH,
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
C-
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
      LOGICAL DO_DPHI12_CUT
      LOGICAL DO_EF_COSMIC_CUT
C
      INTEGER IFAIL,IV,IER,I_SET,I_RESET,JBIT,VERSION,MODE
      INTEGER NOMU,NOEL,NOPH,NOJT,NOJT_MIN,NOMU_UNCUT
      INTEGER LPMUO,LPNUT2,LPNUT3,LJETS,GZPMUO,GZPNUT,GZJETS
      INTEGER LPMUO_VEC(5),I_MU,LPMUO_VEC2(5),J_MU
      INTEGER LJETS_VEC(10),I_JT,ITEMP,IJUNK,IOK
      INTEGER I_MU_ISOL,LPMUO_VEC_ISOL(5)
      INTEGER J_MU_ISOL,LPMUO_VEC2_ISOL(5)
C
      REAL PTMIN_JET1,PTMIN_JET2
      REAL MAX_EDIF_CONES,MAX_EDIF_CONES_CF,MAX_EDIF_CONES_EF
      REAL ERADG,ERAD,ECOR,EISO,P_MU_CORR,PT_MU_CORR
      REAL FACT,DR_MU_CORE,DR_MU_ISOL
      REAL MISSET_MIN_CORR,MISSET_CORR
      REAL MISSET_MIN_CALO,MISSET_CORR2
      REAL DPHI_LEP12,DPHI_MAX_LEP12
      REAL PI,TWOPI,CONV,ET1,ET2,JET1_ET,JET2_ET
      REAL JETS_EX,JETS_EY,JETS_EZ,JETS_E,JETS_ET,JETS_PHI,JETS_ETA
      REAL MET_VEC(3)
      REAL EF_DETA_CUT,EF_DETA
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
        CALL EZGET('DO_DPHI12_CUT',DO_DPHI12_CUT,IER)
        IF (IER.EQ.0) CALL EZGET('MU_CORR',CORR_MU,IER)
        IF (IER.EQ.0) CALL EZGET('CORECONE_SIZE',DR_MU_CORE,IER)
        IF (IER.EQ.0) CALL EZGET('ISOCONE_SIZE',DR_MU_ISOL,IER)
        IF (IER.EQ.0) CALL EZGET('ISOCONE_CUT_CF',MAX_EDIF_CONES_CF,IER)
        IF (IER.EQ.0) CALL EZGET('ISOCONE_CUT_EF',MAX_EDIF_CONES_EF,IER)
        IF (IER.EQ.0) CALL EZGET('MAX_DPHI_L12',DPHI_MAX_LEP12,IER)
        IF (IER.EQ.0) CALL EZGET('PNUT2_ETMIN',MISSET_MIN_CALO,IER)
        IF (IER.EQ.0) CALL EZGET('PNUT3_ETMIN',MISSET_MIN_CORR,IER)
        IF (IER.EQ.0) CALL EZGET('JETS_CORR',CORR_JETS,IER)
        IF (IER.EQ.0) CALL EZGET('JET_MULT_MIN',NOJT_MIN,IER)
        IF (IER.EQ.0) CALL EZGET('JET1_PTMIN',PTMIN_JET1,IER)
        IF (IER.EQ.0) CALL EZGET('JET2_PTMIN',PTMIN_JET2,IER)
        IF (IER.EQ.0) CALL EZGET('DO_EF_COSMIC_CUT'
     &     ,DO_EF_COSMIC_CUT,IER)
        IF (IER.EQ.0) CALL EZGET('EF_DETA_CUT',EF_DETA_CUT,IER)
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error getting RCP parameters',
     &    'TOP_LEPTONS_FIND_MUMU',' ','F')
        FIRST=.FALSE.
      ENDIF
C
      IFGOOD=.FALSE.
      I_MU=0
      J_MU=0
      I_MU_ISOL=0
      J_MU_ISOL=0
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
      IF(I_MU.LT.1) GO TO 999
      IF(I_MU.GT.5) CALL ERRMSG('Muon Store Truncated at 5',
     1 'TOP_LEPTONS_FIND_MUMU',' ','W')
C
C *** PNUT3
C
      MISSET_CORR=0.
      LPNUT3=GZPNUT(3)
      IF(LPNUT3.NE.0) MISSET_CORR=Q(LPNUT3+7)
C
C *** PNUT2
C
      MISSET_CORR2=0.
      LPNUT2=GZPNUT(4)
      IF(LPNUT2.LE.0) LPNUT2=GZPNUT(2)
      IF(LPNUT2.NE.0) THEN
        MISSET_CORR2= (Q(LPNUT2+3)+MET_VEC(1))**2 +
     1   (Q(LPNUT2+4)+MET_VEC(2))**2
        MISSET_CORR2=SQRT(MISSET_CORR2)
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
C *** check how many golden PMUO banks we have
C *** for 2 then test on the two banks, otherwise look for an additional
C *** 'silver' muon
C *** Look for isolated muons above Pt threshold
C
      CALL TOP_LEPTONS_FIND_ISOLMU(I_MU,LPMUO_VEC,I_MU_ISOL,
     1  LPMUO_VEC_ISOL,I_JT,MODE)
C
      IF(I_MU_ISOL.LT.2) GO TO 100
C
C *** Corrected Missing Et
C
      IF(MISSET_CORR2.LT.MISSET_MIN_CALO) GO TO 999
      IF(MISSET_CORR.LT.MISSET_MIN_CORR) GO TO 999
C
C *** dPhi max (muon1-muon2)
C
      DPHI_LEP12=ABS( Q(LPMUO_VEC_ISOL(1)+17)-Q(LPMUO_VEC_ISOL(2)+17) )
      IF(DPHI_LEP12.GT.PI) DPHI_LEP12=TWOPI-DPHI_LEP12
      DPHI_LEP12=DPHI_LEP12*CONV
      IF(DO_DPHI12_CUT) THEN
        IF(DPHI_LEP12.GT.DPHI_MAX_LEP12) GO TO 999
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
          CALL TOP_LEPTONS_CORR_JETPARM(LJETS_VEC(1),JETS_E,ET2,
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
      GO TO 999
C
C ---------------------------------------------------------------------------
C
C *** Now attempt to look for a second 'silver' quality muon track
C *** First, check that there at least 2 muon candidates and that we
C *** have at least 1 isolated 'golden muon
C
  100 IF(NOMU_UNCUT.LT.2) GO TO 999
      IF(I_MU_ISOL.LT.1) GO TO 999
C
C *** Loop over muon banks and look for 'silver' muon candidates
C *** provided that at least one 'golden' muon has been found
C
      LPMUO=GZPMUO(0)
      DO WHILE (LPMUO.NE.0)
        IF(LPMUO.EQ.LPMUO_VEC_ISOL(1)) GO TO 110
C
C *** rejected PMUO Bank - now look to see how bad the track is
C ***  ... if EF Lvl2 cut is set then veto candidate. Then :
C ***     
C ***  ... loop over flag bits and allow 1 failure out of
C ***      IFW4,Impact Parameter,CD Match, Calorimeter Cut
C ***  ... always demand Cosmic cut and crossing Octant veto
C ***      MUHTPLN, Ptmin, etamax
C
        IF(VERSION.LT.3) THEN
          IF(JBIT(IQ(LPMUO+44),19).EQ.1) GO TO 110
          IJUNK=0
          DO IV=26,31
            IF(JBIT(IQ(LPMUO+44),IV).EQ.1) IJUNK=-1
          ENDDO
          IF(IJUNK.LT.0) GO TO 110
        ELSE
          IF(JBIT(IQ(LPMUO+45),6).EQ.1) GO TO 110
          IF(JBIT(IQ(LPMUO+45),7).EQ.1) GO TO 110
          IF(JBIT(IQ(LPMUO+45),14).EQ.1) GO TO 110
          IF(JBIT(IQ(LPMUO+45),15).EQ.1) GO TO 110
          IJUNK=0
          DO IV=29,31
            IF(JBIT(IQ(LPMUO+44),IV).EQ.1) IJUNK=-1
          ENDDO
          IF(IJUNK.LT.0) GO TO 110
        ENDIF
C
C *** Test for fails of other criteria
C
        IFAIL=0
        IF(VERSION.LT.3) THEN
          DO IV=20,25
            IF(JBIT(IQ(LPMUO+44),IV).EQ.1) IFAIL=IFAIL+1
          ENDDO
          IF(JBIT(IQ(LPMUO+44),18).EQ.1) IFAIL=IFAIL+1
          IF(JBIT(IQ(LPMUO+44),28).EQ.1) IFAIL=IFAIL+1
        ELSE
          DO IV=8,13
            IF(JBIT(IQ(LPMUO+44),IV).EQ.1) IFAIL=IFAIL+1
          ENDDO
          DO IV=18,28
            IF(JBIT(IQ(LPMUO+44),IV).EQ.1) IFAIL=IFAIL+1
          ENDDO
        ENDIF
        IF(IFAIL.LT.2) THEN
C
C *** Re-set good_muon flag for selected 'silver' muon
C
          IF(VERSION.LT.3) THEN
            CALL SBIT(I_RESET,IQ(LPMUO+44),17)
          ELSE
            CALL SBIT(I_SET,IQ(LPMUO+45),2)
          ENDIF
          J_MU=J_MU+1
          IF(J_MU.LT.6) THEN
            LPMUO_VEC2(J_MU)=LPMUO
C
C *** first do muon dE/dx correction - if appropriate
C
            IF(CORR_MU) THEN
C
C *** Check if correction has already been applied
C
              IF(IQ(LPMUO+3).LT.3) THEN
                IF(IQ(LPMUO+7).GT.4) THEN
                  MAX_EDIF_CONES=MAX_EDIF_CONES_EF
                ELSE
                  MAX_EDIF_CONES=MAX_EDIF_CONES_CF
                ENDIF
                CALL TOP_LEPTONS_UTIL_MUISO_CORE(LPMUO,
     1            DR_MU_CORE,DR_MU_ISOL,MAX_EDIF_CONES,
     2            ERADG,ERAD,ECOR,EISO,IOK)
                IF(IOK.GT.0) THEN
                  P_MU_CORR = Q(LPMUO+13) - ERADG + ERAD
                  PT_MU_CORR = P_MU_CORR * Q(LPMUO+14) / Q(LPMUO+13)
                  FACT = P_MU_CORR / Q(LPMUO+13)
                  Q(LPMUO+10) = Q(LPMUO+10) * FACT
                  Q(LPMUO+11) = Q(LPMUO+11) * FACT
                  Q(LPMUO+12) = Q(LPMUO+12) * FACT
                  Q(LPMUO+13) = P_MU_CORR
                  Q(LPMUO+14) = PT_MU_CORR
                  IQ(LPMUO+7) = 3
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
  110   CONTINUE
        LPMUO=LQ(LPMUO)
      ENDDO
      IF(J_MU.GT.5) J_MU=5
C
C *** If no candidate is found then quit
C
      ITEMP=I_MU_ISOL+J_MU
      IF(ITEMP.LT.2) GO TO 999
C
C *** Look for isolated muons above Pt threshold
C
      CALL TOP_LEPTONS_FIND_ISOLMU(J_MU,LPMUO_VEC2,J_MU_ISOL,
     1  LPMUO_VEC2_ISOL,I_JT,MODE)
C
      ITEMP=I_MU_ISOL+J_MU_ISOL
      IF(ITEMP.LT.2) GO TO 999
C
C *** Re-build PNUT3 to take account of silver muon(s)
C
      CALL TOP_LEPTONS_REBUILD_PNUT3(MET_VEC)
C
C *** re-calculate PNUT3 Etmiss taking account of silever muons
C
      MISSET_CORR=0.
      LPNUT3=GZPNUT(3)
      IF(LPNUT3.NE.0) MISSET_CORR=Q(LPNUT3+7)
C
C *** Corrected Missing Et CUTS
C
      IF(MISSET_CORR2.LT.MISSET_MIN_CALO) GO TO 999
      IF(MISSET_CORR.LT.MISSET_MIN_CORR) GO TO 999
C
C *** dPhi max (muon1-muon2)
C
      DPHI_LEP12=ABS( Q(LPMUO_VEC_ISOL(1)+17)-Q(LPMUO_VEC2_ISOL(1)+17))
      IF(DPHI_LEP12.GT.PI) DPHI_LEP12=TWOPI-DPHI_LEP12
      DPHI_LEP12=DPHI_LEP12*CONV
      IF(DO_DPHI12_CUT) THEN
        IF(DPHI_LEP12.GT.DPHI_MAX_LEP12) GO TO 999
      ENDIF
C
C *** EF Cosmic rejection -B2B eta cut
C
      IF(DO_EF_COSMIC_CUT) THEN
        IF(ABS(Q(LPMUO_VEC_ISOL(1)+16)).LT.0.9) GO TO 220
        DO IV=1,J_MU          
           IF(ABS(Q(LPMUO_VEC2_ISOL(IV)+16)).LT.0.9) GO TO 220
        END DO                   
        EF_DETA=ABS(Q(LPMUO_VEC_ISOL(1)+16)+Q(LPMUO_VEC2_ISOL(1)+16))
        IF(EF_DETA.LT.EF_DETA_CUT) GO TO 999
      END IF 
  220 CONTINUE
C
C
C *** Jet multiplicity and Et selection
C
      IF(NOJT_MIN.LT.1) GO TO 250
      IF(I_JT.LT.NOJT_MIN) GO TO 999
      IF(I_JT.GT.1) THEN
C
C *** Apply Jet corrections if requested
C
        IF(CORR_JETS) THEN
          CALL TOP_LEPTONS_CORR_JETPARM(LJETS_VEC(1),JETS_E,ET1,
     1      JETS_EX,JETS_EY,JETS_EZ,JETS_PHI,JETS_ETA,IER)
          CALL TOP_LEPTONS_CORR_JETPARM(LJETS_VEC(1),JETS_E,ET2,
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
  250 CONTINUE
C
C *** Good candidate
C
      IFGOOD=.TRUE.      
C ---------------------------------------------------------------------------
  999 RETURN
      END
