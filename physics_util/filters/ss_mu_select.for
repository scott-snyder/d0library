      SUBROUTINE SS_MU_SELECT(LPMUO)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Offline Muon id and Cosmic Ray Rejection
C-                          Routine
C-
C-   Inputs  : 
C-              LPMUO - Pointer to PMUO bank to be tested
C-
C-   Outputs : Sets upper bits of IFW2 copy which is stored in word
C-             44 of PMUO. If bit is set => track failed this cut
C-
C-         Bit No.           Cut
C-           17       Failed at least one selection/id cut
C-           18       IFW1 cuts 
C-           19       Reserved for later use (CT cuts)
C-           20       IFW4 cut
C-           21       Muon - Central tracking match
C-           22       Impact parameter w.r.t. CT vertex
C-           23       Reserved for later use (CT match cuts)
C-           24       Reserved for later use (CT match cuts)
C-           25       Calorimeter Minimum ionizing deposition
C-           26       MUCTAG opposite track or hits
C-           27       Crossing Trigger Octant cut
C-           28       Inegral B.dL cut
C-           29-32    Reserved for Physics cuts (eta,Pt etc.)
C-
C-   Controls: None
C-
C-   Created  28-SEP-1992   Stephen J. Wimpenny
C-   Modified  4-Oct-1992   Cuts on quality of Muon-Ztrak match added
C-   Modified 23-Oct-1992   Cuts on IFW1 and Integral B.dl added
C-   Modified 25-Oct-1992   Separate cuts on A,B,C-layers added
C-   This version for SS filt, 10-jan-1993, jtwhite
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      LOGICAL FIRST,DO_MUCD_CUT,DO_MUIFW4_CUT,DO_MUCTAG_CUT
      LOGICAL DO_OCTANT_CUT,DO_CALDEP_CUT,DO_IMPACT_CUT
      LOGICAL DO_BDL_CUT,DO_MUIFW1_CUT,DO_MCSMEAR
      LOGICAL DO_PT_CUT,DO_ETA_CUT
      REAL CONV,TEMP,IMPACT_MU_MAX,MIN_ENERGY_02
      REAL MU_MAX_ZTRAK_DPHI,MU_MAX_ZTRAK_DTHETA,MIN_INTEG_BDL
      REAL DPHI_SMEAR,DTHETA_SMEAR
      REAL PT_CUT,ETA_CUT,PT_CUT2
      INTEGER IFW1_CUT_A,IFW1_CUT_B,IFW1_CUT_C,IFW4_CUT
      INTEGER IFW2_BIT7_CUT,IFW2_BIT8_CUT,IFW2_BIT9_CUT
      INTEGER I_SET,I_RESET,IER,I,ITEMP,JTEMP,JBIT
      INTEGER LPMUO,LMUOT,GZPMUO
C
      DATA FIRST,CONV/.TRUE.,57.29578/
C
      IF(FIRST) THEN
        IER = 0
C
C *** Muon id - from SS_MU_SELECT_RCP -
C
        CALL INRCP('SS_MU_SELECT_RCP',IER)
        IF(IER.NE.0)
     1    CALL ERRMSG('SS_MU_SELECT_RCP not found',
     2      'SS_MU_SELECT',' ','W')
        CALL EZPICK('SS_MU_SELECT_RCP')
        CALL ERRMSG('Reading SS_MU_SELECT_RCP',
     1      'SS_MU_SELECT',' ','W')
C
C *** Muon id cuts
C
        CALL EZGET('MU_DO_MUCTAG_CUT',DO_MUCTAG_CUT,IER)
        IF(DO_MUCTAG_CUT) THEN
          IF(IER.EQ.0) CALL EZGET('MU_IFW2_BIT7_CUT',IFW2_BIT7_CUT,IER)
          IF(IER.EQ.0) CALL EZGET('MU_IFW2_BIT8_CUT',IFW2_BIT8_CUT,IER)
        ENDIF
        IF(IER.EQ.0) CALL EZGET('MU_DO_OCTANT_CUT',DO_OCTANT_CUT,IER)
        IF(DO_OCTANT_CUT) THEN
          IF(IER.EQ.0) CALL EZGET('MU_IFW2_BIT9_CUT',IFW2_BIT9_CUT,IER)
        ENDIF
C
        IF(IER.EQ.0) CALL EZGET('MU_DO_IFW4_CUT',DO_MUIFW4_CUT,IER)
        IF(DO_MUIFW4_CUT) THEN
          IF(IER.EQ.0) CALL EZGET('MU_IFW4_CUT',IFW4_CUT,IER)
        ENDIF
C
        IF(IER.EQ.0) CALL EZGET('MU_DO_IFW1_CUT',DO_MUIFW1_CUT,IER)
        IF(DO_MUIFW1_CUT) THEN
          IF(IER.EQ.0) CALL EZGET('MU_IFW1_NOMISS_A',IFW1_CUT_A,IER)
          IF(IER.EQ.0) CALL EZGET('MU_IFW1_NOMISS_B',IFW1_CUT_B,IER)
          IF(IER.EQ.0) CALL EZGET('MU_IFW1_NOMISS_C',IFW1_CUT_C,IER)
        ENDIF
C
        IF(IER.EQ.0) CALL EZGET('MU_DO_CDCUT',DO_MUCD_CUT,IER)
        IF(DO_MUCD_CUT) THEN
          IF(IER.EQ.0) 
     1      CALL EZGET('MU_MAX_ZTRAK_DTHETA',MU_MAX_ZTRAK_DTHETA,IER)
          IF(IER.EQ.0)
     1      CALL EZGET('MU_MAX_ZTRAK_DPHI',MU_MAX_ZTRAK_DPHI,IER)
        ENDIF
C
        IF(IER.EQ.0) CALL EZGET('MU_DO_IMPACT_CUT',DO_IMPACT_CUT,IER)
        IF(DO_IMPACT_CUT) THEN
          IF(IER.EQ.0) CALL EZGET('MU_MAX_IMPACT',IMPACT_MU_MAX,IER)
        ENDIF
C
        IF(IER.EQ.0) CALL EZGET('MU_DO_CALDEP_CUT',DO_CALDEP_CUT,IER)
        IF(DO_CALDEP_CUT) THEN
          IF(IER.EQ.0) CALL EZGET('MU_MIN_ENERGY_02',MIN_ENERGY_02,IER)
        ENDIF
        IF(IER.EQ.0) CALL EZGET('MU_DO_BDL_CUT',DO_BDL_CUT,IER)
        IF(DO_BDL_CUT) THEN
          IF(IER.EQ.0) CALL EZGET('MU_MIN_BDL',MIN_INTEG_BDL,IER)
        ENDIF
        CALL EZGET('MU_DO_PT_CUT',DO_PT_CUT,IER)
        IF (DO_PT_CUT) CALL EZGET('MU_PT_CUT',PT_CUT,IER)
        IF (DO_PT_CUT) CALL EZGET('MU_PT_CUT2',PT_CUT2,IER)
        CALL EZGET('MU_DO_ETA_CUT',DO_ETA_CUT,IER)
        IF (DO_ETA_CUT) CALL EZGET('MU_ETA_CUT',ETA_CUT,IER)
        IF (IER.NE.0) CALL ERRMSG('Error in SS_MU_SELECT_RCP',
     &    'SS_MU_SELECT',' ','F')
C
        CALL EZRSET
        FIRST=.FALSE.
      ENDIF
C
      IF(LPMUO.GT.0) THEN
        I_SET=1
        I_RESET=0
        ITEMP=0
C
C *** Re-set selection bits in case they have been set in a
C *** previous pass of the code
C
        DO I=17,32
          CALL SBIT(I_RESET,IQ(LPMUO+44),I)
        ENDDO
C
C *** 1.) Cosmic Ray rejection (muon system info only)
C
        LMUOT=LQ(LPMUO-2)
        IF(LMUOT.GT.0) THEN
          IQ(LPMUO+44)=IQ(LMUOT+5)
          IQ(LPMUO+9)=IQ(LMUOT+7)
        ENDIF
C
C *** Check on MUCTAG flags
C
        IF(DO_MUCTAG_CUT) THEN
          IF(JBIT(IQ(LPMUO+44),7).GT.IFW2_BIT7_CUT) ITEMP=ITEMP+1
          IF(JBIT(IQ(LPMUO+44),8).GT.IFW2_BIT8_CUT) ITEMP=ITEMP+1
          IF(ITEMP.GT.0) CALL SBIT(I_SET,IQ(LPMUO+44),26)
        ENDIF
C
C *** Check that muon does not cross octant boundaries
C
        IF(DO_OCTANT_CUT) THEN
          IF(JBIT(IQ(LPMUO+44),9).GT.IFW2_BIT9_CUT) THEN
            ITEMP=ITEMP+1
            CALL SBIT(I_SET,IQ(LPMUO+44),27)
          ENDIF
        ENDIF
C
C *** 2.) Track validation/quality cuts
C ***      ....Cut on IFW4 flags
C
        IF(DO_MUIFW4_CUT) THEN
          IF(IQ(LPMUO+9).GT.IFW4_CUT) THEN
            CALL SBIT(I_SET,IQ(LPMUO+44),20)
            ITEMP=ITEMP+1
          ENDIF
        ENDIF
C
C ***      .... Cut on IFW1 flags
C
        IF(DO_MUIFW1_CUT) THEN
          IF(LMUOT.GT.0) THEN
            JTEMP=IQ(LMUOT+4)
            IF(JTEMP.GE.10) JTEMP=JTEMP-10
            IF(JTEMP.EQ.0) GO TO 10
            GO TO (11,12,13,14),JTEMP
   11       CONTINUE
C
C *** Missing A-layer ... test against IFW1_CUT_A
C
            IF(JTEMP.GT.IFW1_CUT_A) THEN
              CALL SBIT(I_SET,IQ(LPMUO+44),18)
              ITEMP=ITEMP+1
            ENDIF
            GO TO 10
   12       CONTINUE
C
C *** Missing B-layer ... test against IFW1_CUT_B
C
            JTEMP=JTEMP-1
            IF(JTEMP.GT.IFW1_CUT_B) THEN
              CALL SBIT(I_SET,IQ(LPMUO+44),18)
              ITEMP=ITEMP+1
            ENDIF
            GO TO 10
   13       CONTINUE
C
C *** Missing C-layer ... test against IFW1_CUT_C
C
            JTEMP=JTEMP-2
            IF(JTEMP.GT.IFW1_CUT_C) THEN
              CALL SBIT(I_SET,IQ(LPMUO+44),18)
              ITEMP=ITEMP+1
            ENDIF
            GO TO 10
   14       CONTINUE
C
C *** Mising A and B-layers ====> junk ====> reject
C
            CALL SBIT(I_SET,IQ(LPMUO+44),18)
            ITEMP=ITEMP+1
   10       CONTINUE
          ENDIF
        ENDIF
C
C ***      .... Cut on minimum integral B.dl (remove badly measured
C ***           momenta)
C
        IF(DO_BDL_CUT) THEN
          IF(LMUOT.GT.0) THEN
            IF(Q(LMUOT+22).LT.MIN_INTEG_BDL) THEN
              CALL SBIT(I_SET,IQ(LPMUO+44),28)
              ITEMP=ITEMP+1
            ENDIF
          ENDIF
        ENDIF
C
C *** 3.) Central tracking system muon validation
C ***      .....Require good matching CD track cut
C
        IF(DO_MUCD_CUT) THEN
C
C *** Require at least 1 'matching track'
C
          IF(IQ(LPMUO+6).LT.1) GO TO 20
C
C *** test on quality of track match
C
          TEMP=Q(LPMUO+38)/CONV
          IF(DO_MCSMEAR) TEMP=TEMP*DPHI_SMEAR
          IF(TEMP.GT.MU_MAX_ZTRAK_DPHI) GO TO 20
          TEMP=Q(LPMUO+39)/CONV
          IF(DO_MCSMEAR) TEMP=TEMP*DTHETA_SMEAR
          IF(TEMP.GT.MU_MAX_ZTRAK_DTHETA) GO TO 20
          GO TO 30
   20     CALL SBIT(I_SET,IQ(LPMUO+44),21)
          ITEMP=ITEMP+1
        ENDIF
C
C *** Maximum Impact parameter w.r.t. CT vertex position
C
   30   IF(DO_IMPACT_CUT) THEN
          IF(Q(LPMUO+41).GT.IMPACT_MU_MAX) THEN
            CALL SBIT(I_SET,IQ(LPMUO+44),22)
            ITEMP=ITEMP+1
          ENDIF
        ENDIF
C
C *** 4.) Calorimeter muon validation
C ***     .....Check that the calorimeter deposition is consistent 
C ***          with min-ionizing track in tight cone (dR=0.2)
C
        IF(DO_CALDEP_CUT) THEN
          IF(Q(LPMUO+34).LT.MIN_ENERGY_02) THEN
            ITEMP=ITEMP+1
            CALL SBIT(I_SET,IQ(LPMUO+44),25)
          ENDIF
        ENDIF
C
C ***  PT CUT
C
        IF (DO_PT_CUT) THEN
          IF (Q(LPMUO+14) .LT. PT_CUT) THEN
            ITEMP = ITEMP + 1
            CALL SBIT(I_SET,IQ(LPMUO+44),29)
          ENDIF
          IF (Q(LPMUO+14) .LT. PT_CUT2) THEN
            ITEMP = ITEMP + 1
            CALL SBIT(I_SET,IQ(LPMUO+44),31)
          ENDIF
        ENDIF
C
C ***  ETA CUT
C
        IF (DO_ETA_CUT) THEN
          IF (ABS(Q(LPMUO+16)) .GT. ETA_CUT) THEN
            ITEMP = ITEMP + 1
            CALL SBIT(I_SET,IQ(LPMUO+44),30)
          ENDIF
        ENDIF
C
C ***  ----> End of Selection/Validation <------
C ***     check value of cut flag and set global reject bit,
C ***     - if appropriate.
C
        IF(ITEMP.GT.0) THEN
          CALL SBIT(I_SET,IQ(LPMUO+44),17)
        ENDIF
      ELSE
        GO TO 999
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
