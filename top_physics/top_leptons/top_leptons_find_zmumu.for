      SUBROUTINE TOP_LEPTONS_FIND_ZMUMU(IFGOOD,NOMU,NOEL,NOPH,NOJT,
     1  NOMU_UNCUT,MET_VEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : event search routine to look for Z0->mu mu
C-                                event candidates
C-
C-   Inputs  : 
C-             NOMU         - no of 'good' PMUO candidates
C-             NOEL         - no of 'good' PELC candidates
C-             NOPH         - no of 'good' PPHO candidates
C-             NOJT         - no of 'good' JETS candidates
C-
C-   RCP file parameters :
C-
C-   Outputs : 
C-              IFGOOD = .TRUE./.FALSE. - event is/isnt a candidate
C-
C-   Controls: 
C-
C-   Created  29-Jan-1993   Stephen J. Wimpenny
C-   Modified 10-Feb-1993   Skeleton selection code added with PMUO 
C-                          version 1 - 3 compatibility
C-   Modified 15-Mar-1993   Change in name of Good_Muon logical
C-   Modified 16-Jun-1993   Missing Et re-calculated if silver muon(s) are
C-                          used
C-   Modified 17-Jul-1993   Uses new isolated muon logic
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
C
      INTEGER NOMU,NOEL,NOPH,NOJT,NOMU_UNCUT
      INTEGER IJUNK,IV,IER,ITEMP,JBIT,IOK
      INTEGER LPMUO,LJETS,GZPMUO,GZJETS
      INTEGER I_MU,I_SET,I_RESET,IFAIL,VERSION
      INTEGER LPMUO_VEC(5),LPMUO_VEC2(5),J_MU
      INTEGER MODE,I_MU_ISOL,J_MU_ISOL,I_JT
      INTEGER LPMUO_VEC_ISOL(5),LPMUO_VEC2_ISOL(5),LJETS_VEC(10)
C
      REAL MET_VEC(3)
      REAL MAX_EDIF_CONES,MAX_EDIF_CONES_CF,MAX_EDIF_CONES_EF
      REAL ERADG,ERAD,ECOR,EISO,P_MU_CORR,PT_MU_CORR
      REAL FACT,DR_MU_CORE,DR_MU_ISOL
C
      DATA FIRST,MODE/.TRUE.,1/
C
C *** Read cut parameters from RCP file
C
      IER=0
      I_SET=1
      I_RESET=0
C
      IF(FIRST) THEN
        CALL EZPICK('TOP_LEPTONS_RCP')
        CALL EZGET('MU_CORR',CORR_MU,IER)
        IF(IER.EQ.0) CALL EZGET('CORECONE_SIZE',DR_MU_CORE,IER)
        IF(IER.EQ.0) CALL EZGET('ISOCONE_SIZE',DR_MU_ISOL,IER)
        IF(IER.EQ.0) CALL EZGET('ISOCONE_CUT_CF',MAX_EDIF_CONES_CF,IER)
        IF(IER.EQ.0) CALL EZGET('ISOCONE_CUT_EF',MAX_EDIF_CONES_EF,IER)
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error getting RCP parameters',
     &    'TOP_LEPTONS_FIND_ZMUMU',' ','F')
        FIRST=.FALSE.
      ENDIF
C
      IFGOOD=.FALSE.
      I_MU=0
      I_MU_ISOL=0
      J_MU_ISOL=0
C
C *** Require at least 1 'good' PMUO bank
C
      IF(NOMU.LT.1) GO TO 999
C
      IF(NOMU.GT.5) CALL ERRMSG('Muon Store Trucated at 5',
     1 'TOP_LEPTONS_FIND_WMU',' ','W')
C
C *** Get all relevent pointers
C *** PMUO
C
      LPMUO=GZPMUO(0)
      DO WHILE(LPMUO.NE.0)
        VERSION=IQ(LPMUO+1)
        IF(TOP_LEPTONS_GOOD_MUON(LPMUO)) THEN
          I_MU=I_MU+1
          IF(I_MU.LT.6) LPMUO_VEC(I_MU)=LPMUO
        ENDIF
        LPMUO=LQ(LPMUO)
      ENDDO
      IF(I_MU.LT.1) GO TO 999
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
C *** check how many golden PMUO banks we have
C *** for 2 then test on the two banks, otherwise look for an additional
C *** 'silver' muon
C *** Look for isolated muons above threshold
C
      CALL TOP_LEPTONS_FIND_ISOLMU(I_MU,LPMUO_VEC,I_MU_ISOL,
     1  LPMUO_VEC_ISOL,I_JT,MODE)
C
      IF(I_MU_ISOL.LT.2) GO TO 100
C
C *** Good candidate
C
      IFGOOD=.TRUE.
      GO TO 999
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
C *** do muon dE/dx correction for silver muon(s)
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
      IF(J_MU.LT.1) GO TO 999
C
C *** Look for isolated muons above threshold
C
      CALL TOP_LEPTONS_FIND_ISOLMU(J_MU,LPMUO_VEC2,J_MU_ISOL,
     1  LPMUO_VEC2_ISOL,I_JT,MODE)
C
      ITEMP=I_MU_ISOL+J_MU_ISOL
      IF(ITEMP.LT.2) GO TO 999
C
C *** Now re-make PNUT3 bank taking silver muon(s) into account
C
      CALL TOP_LEPTONS_REBUILD_PNUT3(MET_VEC)
C
C *** Good candidate
C
      IFGOOD=.TRUE.
C----------------------------------------------------------------------
  999 RETURN
      END
