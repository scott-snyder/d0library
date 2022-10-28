       SUBROUTINE TOP_LEPTONS_MUON_CODE(LPMUO)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Offline Muon id and Cosmic Ray Rejection
C-                          Routine - Versions 3 of PMUO onwards.
C-
C-   Inputs  : 
C-              LPMUO - Pointer to PMUO bank to be tested
C-
C-   Outputs : Sets bit pattern in user flag word of PMUO (word 45)
C-                 If bit is set => track failed this cut
C-
C-         Bit No.           Cut
C-                    i.) global select flag bits
C-           1        Golden Muon Candidate (passes all cuts)
C-           2        Silver Muon Candidate (user defined)
C-           3        spare
C-           4        spare
C-           5        spare
C-
C-                    ii.) Cosmic Ray rejection 
C-           6        MUCTAG opposite track or hits
C-           7        Crossing Trigger Octant cut
C-           8        3-D Impact parameter cut
C-           9        Bend-view Impact parameter cut
C-           10       Non-bend view Impact parameter cut
C-           11       Floating t0 offset cut
C-           12       Calorimter opposite side mip cut
C-
C-                    iii.) Muon Track Quality
C-           13       IFW1 plane cuts
C-           14       WAMUS/CF min hit multiplicity cuts
C-           15       WAMUS/EF min hit multiplicity cuts
C-           16       reserved for SAMUS
C-           17       IFW4 cut
C-           18       Bend view quality of fit cut
C-           19       Non-bend view quality of fit cut
C-           24       Rejected A-layer stub
C-
C-                    iv.) CD Track match
C-           20       CD Match - dtheta cut
C-           21       CD Match - dphi cut
C-           22       CD Match - min match multiplicity
C-           23       CD Match - max match multiplicity
C-
C-                    v.) Calorimeter MIP Deposition
C-           25       Calorimeter Mip energy cut
C-           26       spare
C-            
C-                    vi.) Fiducial cuts
C-           27       Minimum Integral B.dl
C-           28       spare
C-
C-                    vii.) Physics cuts
C-           29-32    reserved for Physics cuts
C-
C-   Controls: None
C-
C-   Created  27-Jan-1993   Stephen J. Wimpenny
C-   Modified 10-Feb-1993   Definition of bits 1 and 2 changed
C-   Modifoed 14-Feb-1993   Diehl : Samus plane counts added into EF
C-                          A/B/C Wamus sum
C-   Modified 21-Mar-1993   Name change to Decode_Muon_Plane_Info for
C-                          library compatibility
C-   Modified 22-Apr-1993   fix one-sided cut on floating T0 offset
C-   Modified 20-Sep-1993   fix Integer/Real swop in Quad No tests
C-   Modified  4-Dec-1993   MUCTAG and OCTANT Cuts changed to allow
C-                          explicit Cosmic selection.
C-   Modified 12-Feb-1994   Code to reject A-layer stubs added
C-   Modified 29-Mar-1994   Cut on mip trace using cells + 1NN instead
C-                          of 2NN
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      LOGICAL FIRST,DO_MUCD_CUT,DO_MUIFW4_CUT,DO_MUCTAG_CUT
      LOGICAL DO_OCTANT_CUT,DO_CALDEP_CUT,DO_IMPACT_CUT
      LOGICAL DO_BDL_CUT,DO_MUIFW1_CUT,DO_REJECT_ASTUB
      LOGICAL DO_T0FLOAT_CUT,DO_OPP_MIP_CUT,DO_MUHITPLN_CUTS
      LOGICAL DO_EF_LVL2_CUT,DO_BEND_QUAL_CUT,DO_NBEND_QUAL_CUT
C
      REAL CONV,TEMP,IMPACT_MU_MAX,MIN_ENERGY_02,MIN_OPP_MIPEN
      REAL MAX_ZTRAK_DPHI,MAX_ZTRAK_DTHETA,MIN_INTEG_BDL
      REAL BEND_IMPACT_MU_MAX,NONBEND_IMPACT_MU_MAX,T0FLOAT_MU_MAX
      REAL BEND_VIEW_QUAL,NBEND_VIEW_QUAL,MIN_MIP_NOCD
C
      INTEGER IFW1_CUT_A,IFW1_CUT_B,IFW1_CUT_C,IFW4_CUT
      INTEGER IFW2_BIT7_CUT,IFW2_BIT8_CUT,IFW2_BIT9_CUT
      INTEGER CF_WAM_MINA,CF_WAM_MINB,CF_WAM_MINC,CF_WAM_MINTOT
      INTEGER EF_WAM_MINA,EF_WAM_MINB,EF_WAM_MINC,EF_WAM_MINTOT
      INTEGER MIN_NO_ZTRAKS,MAX_NO_ZTRAKS
      INTEGER I_SET,I_RESET,IER,I,ITEMP,JTEMP,JBIT,IOK
      INTEGER WAM_HIT(6),SAM_HIT(3)
      INTEGER LPMUO,LMUOT
C
      DATA FIRST,CONV/.TRUE.,57.29578/
C
      IF(FIRST) THEN
        IER = 0
C
C *** Muon id - from MUON_SELECT_RCP -
C
        CALL INRCP('MUON_SELECT_RCP',IER)
        IF(IER.NE.0)
     1    CALL ERRMSG('MUON_SELECT_RCP not found',
     2      'MUON_SELECT',' ','W')
        CALL EZPICK('MUON_SELECT_RCP')
        CALL ERRMSG('Reading MUON_SELECT_RCP',
     1      'MUON_SELECT',' ','W')
C
C *** Muon id cuts
C
        CALL EZGET_l('MU_DO_MUCTAG_CUT',DO_MUCTAG_CUT,IER)
        IF(IER.EQ.0) CALL EZGET_i('MU_IFW2_BIT7_CUT',IFW2_BIT7_CUT,IER)
        IF(IER.EQ.0) CALL EZGET_i('MU_IFW2_BIT8_CUT',IFW2_BIT8_CUT,IER)
        IF(IER.EQ.0) CALL EZGET_l('MU_DO_OCTANT_CUT',DO_OCTANT_CUT,IER)
        IF(IER.EQ.0) CALL EZGET_i('MU_IFW2_BIT9_CUT',IFW2_BIT9_CUT,IER)
C
        IF(IER.EQ.0) CALL EZGET_l('MU_DO_IMPACT_CUT',DO_IMPACT_CUT,IER)
        IF(IER.EQ.0) CALL EZGET('MU_MAX_IMPACT',IMPACT_MU_MAX,IER)
        IF(IER.EQ.0) 
     1     CALL EZGET('MU_MAX_BEND_IMPACT',BEND_IMPACT_MU_MAX,IER)
        IF(IER.EQ.0) 
     1     CALL EZGET('MU_MAX_NONBEND_IMPACT',NONBEND_IMPACT_MU_MAX,IER)
C
        IF(IER.EQ.0) CALL EZGET_l('MU_DO_T0FLOAT_CUT',DO_T0FLOAT_CUT,
     &       IER)
        IF(IER.EQ.0) CALL EZGET('MU_MAX_T0FLOAT',T0FLOAT_MU_MAX,IER)
C
        IF(IER.EQ.0) CALL EZGET_l('MU_DO_OPP_MIP_CUT',DO_OPP_MIP_CUT,
     &       IER)
        IF(IER.EQ.0) CALL EZGET('MU_MIN_OPP_MIPEN',MIN_OPP_MIPEN,IER)
C
        IF(IER.EQ.0) CALL EZGET_l('MU_DO_IFW1_CUT',DO_MUIFW1_CUT,IER)
        IF(IER.EQ.0) CALL EZGET_i('MU_IFW1_NOMISS_A',IFW1_CUT_A,IER)
        IF(IER.EQ.0) CALL EZGET_i('MU_IFW1_NOMISS_B',IFW1_CUT_B,IER)
        IF(IER.EQ.0) CALL EZGET_i('MU_IFW1_NOMISS_C',IFW1_CUT_C,IER)
C
        IF(IER.EQ.0) 
     1    CALL EZGET_l('MU_DO_HITPLN_CUTS',DO_MUHITPLN_CUTS,IER)
        IF(IER.EQ.0) CALL EZGET_i('MU_CF_WAMA_MIN',CF_WAM_MINA,IER)
        IF(IER.EQ.0) CALL EZGET_i('MU_CF_WAMB_MIN',CF_WAM_MINB,IER)
        IF(IER.EQ.0) CALL EZGET_i('MU_CF_WAMC_MIN',CF_WAM_MINC,IER)
        IF(IER.EQ.0) 
     1    CALL EZGET_i('MU_CF_WAMTOT_MIN',CF_WAM_MINTOT,IER)
        IF(IER.EQ.0) CALL EZGET_i('MU_EF_WAMA_MIN',EF_WAM_MINA,IER)
        IF(IER.EQ.0) CALL EZGET_i('MU_EF_WAMB_MIN',EF_WAM_MINB,IER)
        IF(IER.EQ.0) CALL EZGET_i('MU_EF_WAMC_MIN',EF_WAM_MINC,IER)
        IF(IER.EQ.0) 
     1    CALL EZGET_i('MU_EF_WAMTOT_MIN',EF_WAM_MINTOT,IER)     
        IF(IER.EQ.0) CALL EZGET_l('MU_DO_EF_LVL2_CUT',DO_EF_LVL2_CUT,
     &       IER)
C
        IF(IER.EQ.0)
     1     CALL EZGET_l('MU_DO_REJECT_A_STUB',DO_REJECT_ASTUB,IER)
C
        IF(IER.EQ.0) CALL EZGET_l('MU_DO_IFW4_CUT',DO_MUIFW4_CUT,IER)
        IF(IER.EQ.0) CALL EZGET_i('MU_IFW4_CUT',IFW4_CUT,IER)
C
        IF(IER.EQ.0) 
     1       CALL EZGET_l('MU_DO_BEND_QUAL_CUT',DO_BEND_QUAL_CUT,IER)
         IF(IER.EQ.0) 
     1       CALL EZGET('MU_MAX_BEND_QUAL',BEND_VIEW_QUAL,IER)
        IF(IER.EQ.0) 
     1      CALL EZGET_l('MU_DO_NBEND_QUAL_CUT',DO_NBEND_QUAL_CUT,IER)
        IF(IER.EQ.0) 
     1      CALL EZGET('MU_MAX_NO_BEND_QUAL',NBEND_VIEW_QUAL,IER)
C
        IF(IER.EQ.0) CALL EZGET_l('MU_DO_CDCUT',DO_MUCD_CUT,IER)
        IF(IER.EQ.0) 
     1      CALL EZGET('MU_MAX_ZTRAK_DTHETA',MAX_ZTRAK_DTHETA,IER)
        IF(IER.EQ.0)
     1      CALL EZGET('MU_MAX_ZTRAK_DPHI',MAX_ZTRAK_DPHI,IER)
        IF(IER.EQ.0)
     1      CALL EZGET_i('MU_MIN_CD_MULT',MIN_NO_ZTRAKS,IER)
        IF(IER.EQ.0)
     1      CALL EZGET_i('MU_MAX_CD_MULT',MAX_NO_ZTRAKS,IER)
C
        IF(IER.EQ.0) CALL EZGET_l('MU_DO_CALDEP_CUT',DO_CALDEP_CUT,IER)
        IF(IER.EQ.0) CALL EZGET('MU_MIN_ENERGY_02',MIN_ENERGY_02,IER)
        IF(IER.EQ.0) CALL EZGET('MU_MIN_MIP_NOCD',MIN_MIP_NOCD,IER)
C
        IF(IER.EQ.0) CALL EZGET_l('MU_DO_BDL_CUT',DO_BDL_CUT,IER)
        IF(IER.EQ.0) CALL EZGET('MU_MIN_BDL',MIN_INTEG_BDL,IER)
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error in MUON_SELECT_RCP',
     &    'MUON_SELECT',' ','F')
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
        DO I=1,32
          CALL SBIT(I_RESET,IQ(LPMUO+45),I)
        ENDDO
C
C *** 1.) Cosmic Ray rejection (muon system info only)
C
        LMUOT=LQ(LPMUO-2)
C
C *** Check on MUCTAG flags
C
        IF(DO_MUCTAG_CUT) THEN
          IF(JBIT(IQ(LPMUO+44),7).NE.IFW2_BIT7_CUT) ITEMP=ITEMP+1
          IF(JBIT(IQ(LPMUO+44),8).NE.IFW2_BIT8_CUT) ITEMP=ITEMP+1
          IF(ITEMP.GT.0) CALL SBIT(I_SET,IQ(LPMUO+45),6)
        ENDIF
C
C *** Check that muon does not cross octant boundaries
C
        IF(DO_OCTANT_CUT) THEN
          IF(JBIT(IQ(LPMUO+44),9).NE.IFW2_BIT9_CUT) THEN
            ITEMP=ITEMP+1
            CALL SBIT(I_SET,IQ(LPMUO+45),7)
          ENDIF
        ENDIF
C
C *** Maximum Impact parameter cuts w.r.t. CT vertex position :
C ***      3-D Impact
C ***      bend view
C ***      non-bend view
C
      IF(DO_IMPACT_CUT) THEN
        IF(Q(LPMUO+41).GT.IMPACT_MU_MAX) THEN
          CALL SBIT(I_SET,IQ(LPMUO+45),8)
          ITEMP=ITEMP+1
        ENDIF
        IF(ABS(Q(LPMUO+56)).GT.BEND_IMPACT_MU_MAX) THEN
          CALL SBIT(I_SET,IQ(LPMUO+45),9)
          ITEMP=ITEMP+1
        ENDIF
        IF(ABS(Q(LPMUO+57)).GT.NONBEND_IMPACT_MU_MAX) THEN
          CALL SBIT(I_SET,IQ(LPMUO+45),10)
          ITEMP=ITEMP+1
        ENDIF
      ENDIF
C
C *** floating t0 offset
C
      IF(DO_T0FLOAT_CUT) THEN
        IF(ABS(Q(LPMUO+24)).GT.T0FLOAT_MU_MAX) THEN
          CALL SBIT(I_SET,IQ(LPMUO+45),11)
          ITEMP=ITEMP+1
        ENDIF
      ENDIF
C
C *** calorimeter back-to-back mip cut
C
      IF(DO_OPP_MIP_CUT) THEN
        IF(Q(LPMUO+88).GT.MIN_OPP_MIPEN) THEN
          CALL SBIT(I_SET,IQ(LPMUO+45),12)
          ITEMP=ITEMP+1
        ENDIF
      ENDIF
C
C *** 2.) Track validation/quality cuts
C ***      .... Cuts on IFW1 flags
C
        IF(DO_MUIFW1_CUT) THEN
          IF(LMUOT.GT.0) THEN
            JTEMP=IQ(LMUOT+4)
            IF(JTEMP.GE.10) JTEMP=JTEMP-10
            IF(JTEMP.EQ.0) GO TO 10
            GO TO (11,12,13,14,10),JTEMP
   11       CONTINUE
C
C *** Missing A-layer ... test against IFW1_CUT_A
C
            IF(JTEMP.GT.IFW1_CUT_A) THEN
              CALL SBIT(I_SET,IQ(LPMUO+45),13)
              ITEMP=ITEMP+1
            ENDIF
            GO TO 10
   12       CONTINUE
C
C *** Missing B-layer ... test against IFW1_CUT_B
C
            JTEMP=JTEMP-1
            IF(JTEMP.GT.IFW1_CUT_B) THEN
              CALL SBIT(I_SET,IQ(LPMUO+45),13)
              ITEMP=ITEMP+1
            ENDIF
            GO TO 10
   13       CONTINUE
C
C *** Missing C-layer ... test against IFW1_CUT_C
C
            JTEMP=JTEMP-2
            IF(JTEMP.GT.IFW1_CUT_C) THEN
              CALL SBIT(I_SET,IQ(LPMUO+45),13)
              ITEMP=ITEMP+1
            ENDIF
            GO TO 10
   14       CONTINUE
C
C *** Mising A and B-layers ====> junk ====> reject
C
            CALL SBIT(I_SET,IQ(LPMUO+45),13)
            ITEMP=ITEMP+1
            GO TO 10
   15       CONTINUE
   10       CONTINUE
          ENDIF
        ENDIF
C
C *** A-layer stub rejection
C
        IF(LMUOT.GT.0) THEN
          JTEMP=IQ(LMUOT+4)
          IF(JTEMP.GE.10) JTEMP=JTEMP-10
          IF(JTEMP.EQ.5) THEN
            IF(DO_REJECT_ASTUB) THEN
              CALL SBIT(I_SET,IQ(LPMUO+45),24)
              ITEMP=ITEMP+1
            ENDIF
          ENDIF
        ENDIF
C
C *** WAMUS ABC plane hit counts
C
        IF(DO_MUHITPLN_CUTS) THEN          
          CALL TOP_LEPTONS_UTIL_DECODE_PLANES(LPMUO,1,WAM_HIT,SAM_HIT)
C
C *** Branch of EF/CF/SAMUS
C
          IF(IQ(LPMUO+7).LT.5) THEN
C
C *** CF - Quads 1-4
C
            IF(WAM_HIT(1).LT.CF_WAM_MINA) THEN
              CALL SBIT(I_SET,IQ(LPMUO+45),14)
              ITEMP=ITEMP+1
            ENDIF
            IF(WAM_HIT(2).LT.CF_WAM_MINB) THEN
              CALL SBIT(I_SET,IQ(LPMUO+45),14)
              ITEMP=ITEMP+1
            ENDIF
            IF(WAM_HIT(3).LT.CF_WAM_MINC) THEN
              CALL SBIT(I_SET,IQ(LPMUO+45),14)
              ITEMP=ITEMP+1
            ENDIF
            JTEMP=WAM_HIT(1)+WAM_HIT(2)+WAM_HIT(3)
            IF(JTEMP.LT.CF_WAM_MINTOT) THEN
              CALL SBIT(I_SET,IQ(LPMUO+45),14)
              ITEMP=ITEMP+1
            ENDIF
          ELSEIF(IQ(LPMUO+7).LT.13) THEN
C
C *** EF Quads 5-12
C
            JTEMP=WAM_HIT(1)+WAM_HIT(5)+SAM_HIT(1)
            IF(JTEMP.LT.EF_WAM_MINA) THEN
              CALL SBIT(I_SET,IQ(LPMUO+45),15)
              ITEMP=ITEMP+1
            ENDIF
            JTEMP=WAM_HIT(2)+WAM_HIT(6)+SAM_HIT(2)
            IF(JTEMP.LT.EF_WAM_MINB) THEN
              CALL SBIT(I_SET,IQ(LPMUO+45),15)
              ITEMP=ITEMP+1
            ENDIF
            JTEMP=WAM_HIT(3)+WAM_HIT(4)+SAM_HIT(3)
            IF(JTEMP.LT.EF_WAM_MINC) THEN
              CALL SBIT(I_SET,IQ(LPMUO+45),15)
              ITEMP=ITEMP+1
            ENDIF
            JTEMP=JTEMP+WAM_HIT(1)+WAM_HIT(2)+WAM_HIT(5)+WAM_HIT(6)
            JTEMP=JTEMP+SAM_HIT(1)+SAM_HIT(2)
            IF(JTEMP.LT.EF_WAM_MINTOT) THEN 
              CALL SBIT(I_SET,IQ(LPMUO+45),15)
              ITEMP=ITEMP+1
            ENDIF
            IF(DO_EF_LVL2_CUT) THEN
              IOK=1
              CALL TOP_LEPTONS_UTIL_LVL2_HTPLN(WAM_HIT,SAM_HIT,IOK)
              IF(IOK.LT.0) THEN
                CALL SBIT(I_SET,IQ(LPMUO+45),15)
                ITEMP=ITEMP+1
              ENDIF
            ENDIF
          ELSE
C
C *** Samus
C
          ENDIF
        ENDIF
C
C *** IFW4 flag
C
        IF(DO_MUIFW4_CUT) THEN
          IF(IQ(LPMUO+9).GT.IFW4_CUT) THEN
            CALL SBIT(I_SET,IQ(LPMUO+45),17)
            ITEMP=ITEMP+1
          ENDIF
        ENDIF
C
C *** Bend View and Non-bend View Quality of fit
C
        IF(DO_BEND_QUAL_CUT) THEN
          IF(LMUOT.GT.0) THEN
            IF(Q(LMUOT+20).GT.BEND_VIEW_QUAL) THEN
              CALL SBIT(I_SET,IQ(LPMUO+45),18)
              ITEMP=ITEMP+1
            ENDIF
          ENDIF
        ENDIF
        IF(DO_NBEND_QUAL_CUT) THEN
          IF(LMUOT.GT.0) THEN
            IF(Q(LMUOT+21).GT.NBEND_VIEW_QUAL) THEN
              CALL SBIT(I_SET,IQ(LPMUO+45),19)
              ITEMP=ITEMP+1
            ENDIF
          ENDIF
        ENDIF
C
C *** 3.) Central tracking system muon validation
C ***      .....Require good matching CD track cut
C
        JTEMP=0
        IF(DO_MUCD_CUT) THEN
C
C *** test of CD-track match multiplicity
C
          IF(IQ(LPMUO+6).LT.1) GO TO 20
C
          IF(IQ(LPMUO+6).LT.MIN_NO_ZTRAKS) THEN
            GO TO 20
          ENDIF
C
          IF(IQ(LPMUO+6).GT.MAX_NO_ZTRAKS) THEN
            ITEMP=ITEMP+1
            CALL SBIT(I_SET,IQ(LPMUO+45),23)
          ENDIF
C
C *** test on CD-muon angle matching
C
          TEMP=Q(LPMUO+38)/CONV
          IF(TEMP.GT.MAX_ZTRAK_DPHI) THEN
            GO TO 20
          ELSE
            GO TO 22
          ENDIF
C
   22     TEMP=Q(LPMUO+39)/CONV
          IF(TEMP.GT.MAX_ZTRAK_DTHETA) THEN
            GO TO 20
          ELSE
            GO TO 25
          ENDIF
   20     CONTINUE
C
C *** no track match found -> test against increased calorimeter mip
C *** requirement, allow muon to be kept if it passes
C
          IF(Q(LPMUO+34).LT.MIN_MIP_NOCD) THEN
            ITEMP=ITEMP+1
            CALL SBIT(I_SET,IQ(LPMUO+45),22)
          ELSE
            CALL SBIT(I_RESET,IQ(LPMUO+45),22)
            CALL SBIT(I_RESET,IQ(LPMUO+45),21)
            CALL SBIT(I_RESET,IQ(LPMUO+45),20)
          ENDIF
   25     CONTINUE
        ENDIF
C
C *** 4.) Calorimeter muon validation
C ***     .....Check that the calorimeter deposition is consistent 
C ***          with min-ionizing track in tight cone (hit cells + 1NN)
C
        IF(DO_CALDEP_CUT) THEN
          IF(Q(LPMUO+84).LT.MIN_ENERGY_02) THEN
            ITEMP=ITEMP+1
            CALL SBIT(I_SET,IQ(LPMUO+45),25)
          ENDIF
        ENDIF
C
C *** 5.) Fiducial cuts
C ***  Cut on minimum integral B.dl (remove badly measured
C ***           momenta)
C
        IF(DO_BDL_CUT) THEN
          IF(LMUOT.GT.0) THEN
            IF(Q(LMUOT+22).LT.MIN_INTEG_BDL) THEN
              CALL SBIT(I_SET,IQ(LPMUO+45),27)
              ITEMP=ITEMP+1
            ENDIF
          ENDIF
        ENDIF
C
C ***  ----> End of Selection/Validation <------
C ***     check value of cut flag and set global reject bit,
C ***     - if appropriate.
C
        IF(ITEMP.EQ.0) THEN
          CALL SBIT(I_SET,IQ(LPMUO+45),1)
          CALL SBIT(I_SET,IQ(LPMUO+45),2)
        ENDIF
      ELSE
        GO TO 999
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
