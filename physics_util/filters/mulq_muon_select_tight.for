       SUBROUTINE MULQ_MUON_SELECT_TIGHT(LPMUO,STATUS,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Offline Muon id and Cosmic Ray Rejection
C-                          Routine - Versions 3 of PMUO onwards.
C-
C-   Inputs  : 
C-              LPMUO   (I) - Pointer to PMUO bank to be tested
C-
C-   Outputs : 
C-              STATUS  (I) - bit pattern (0/1) indicating which cuts
C-                            were satisfied/failed. 
C-              OK      (I) - 1 if muon track passed all cuts
C-                            specified by MUON_MASK (-1 otherise)
C-   Controls: None
C-
C----------------------------------------------------------------------
C-    
C-     Key to STATUS BITS :
C-
C-         Bit No.           Cut
C-
C-                  i.) Cosmic Ray rejection cuts
C-
C-            1       :   MUCTAG - cosmic ray flag (hits and/or track B-2-B)
C-            2       :   Trigger octant boundary cut
C-            3       :   3-D Impact parameter cut
C-            4       :   Bend view impact parameter cut
C-            5       :   Non-Bend view impact parameter cut
C-            6       :   Floating t0 offset cuts
C-            7       :   Back-to-back Calorimeter mip trace cut
C-            8       :   spare
C-                  
C-                  ii.) Muon track quality cuts
C-
C-            9       :   Global A/B/C missing layer cuts (IFW1)
C-           10       :   Minimum hit requirements/plane WAMUS,SAMUS/WAMUS
C-           11       :   Level 2 Muhtpln cuts
C-           12       :   Minimum hit requirements/plane SAMUS
C-           13       :   IFW4 cut
C-           14       :   Bend view qulity of fit
C-           15       :   Non Bend view quality of fit
C-           16       :   spare
C-
C-                  iii.) Ztrak verification cuts
C-
C-           17       :   Ztrak match multiplicity cuts
C-           18       :   Best Ztrak angle match quality cuts
C-           19       :   spare
C-                  
C-                  iv.) Calorimeter verification cuts
C-
C-           20       :   Calorimeter min mip deposition cut
C-           21       :   spare
C-
C-                  v.) Fiducial cuts
C-
C-           22       :   Integral B.dl cut
C-           23       :   spare
C-
C-           24-28    :   spare
C-
C-           29-32    :   reserved for Physics cuts (eta,Pt etc.)
C-                        - please do not use !
C-
C-   Created  27-Jun-1993   Stephen J. Wimpenny
C-   Modified  1-Jul-1993   Cuts dependent on muon region. 
C-                          Key :
C-                          1 - CF,  2 - EF(only), 3 - SAMUS(A) + EF(B,C)
C-                          4 - SAMUS (A,B) + WAMUS (C), 5 - SAMUS(only)
C-   Modified 16-Aug-1993   Ztrak multiplicity logic changed for tight mip
C-                          bypass cuts. Impact parameter cuts modified.
C-   Modified 19-Aug-1993   D. Wood - add momentum dependence to 
C-                          nonbend impact cuts
C-                          Replace TOP_LEPTON... calls with equivalent
C-                          calls to routines in MUON_UTIL
C-   Modified 1-Oct-1993   D. Wood - status word is no longer copied into
C-                         IQ(LPMUO+45).  This is done in COMPUTE_MU_QUALITY.  
C-                         Also, correct some minor bugs: bit assignment 
C-                         for calmip, swap of delta-phi and delta-theta,
C-                         region-by-region ztrak cuts.
C-                                   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      LOGICAL FIRST,DO_NOCD_GOOD_CAL
C
      INTEGER STATUS,I,N,ITEMP,I_SET,I_RESET,IOK,IER
      INTEGER LPMUO,LMUOT,JBIT,IQUAD,IRGN,OK
      INTEGER MUON_MASK(32),MUMASK,WAM_HIT(6),SAM_HIT(3)
      INTEGER IFW1_NOMISS_A(5),IFW1_NOMISS_B(5),IFW1_NOMISS_C(5)
      INTEGER MIN_HIT_WAM_CF(4),MIN_HIT_WAM_EF(4),MIN_HIT_WAS_EF(4)
      INTEGER MIN_HIT_WSS_EF(4),MIN_HIT_SAMUS(4)
      INTEGER MAX_IFW4(5),IHITS
      INTEGER MIN_ZTRAK_MULT(5),MAX_ZTRAK_MULT(5)
C
      REAL TEMP,CONV
      REAL MAX_3D_IMPACT(5),MAX_BEND_IMPACT(5,3),MAX_NONBEND_IMPACT(5,3)
      REAL MIN_T0FLOAT(5),MAX_T0FLOAT(5),MIN_OPP_MIPEN(5),MIN_BDL
      REAL MAX_BEND_QUAL(5),MAX_NONBEND_QUAL(5),MIN_ETA_BDL
      REAL MAX_ZTRAK_DTHETA(5),MAX_ZTRAK_DPHI(5)
      REAL MIN_CALMIP_CD(5),MIN_CALMIP_NOCD(5)
C
      DATA FIRST,CONV/.TRUE.,57.29578/
C
      IF(FIRST) THEN
        IER = 0
C
C *** Muon id - from MULQ_MUON_SELECT_TIGHT_RCP -
C
        CALL INRCP('MULQ_MUON_SELECT_TIGHT_RCP',IER)
        IF(IER.NE.0)
     1    CALL ERRMSG('MULQ_MUON_SELECT_TIGHT_RCP not found',
     2      'MULQ_MUON_SELECT_TIGHT',' ','W')
        CALL EZPICK('MULQ_MUON_SELECT_TIGHT_RCP')
        CALL ERRMSG('Reading MULQ_MUON_SELECT_TIGHT_RCP',
     1      'MULQ_MUON_SELECT_TIGHT',' ','W')
C
C *** Muon id cuts
C
        CALL EZGETA_i('MUON_MASK',0,0,0,N,IER)
        IF(IER.EQ.0) CALL EZGETA_iarr('MUON_MASK',1,N,1,MUON_MASK,IER)
        IF(N.NE.32) CALL ERRMSG('Error reading Muon Mask Array',
     1    'MULQ_MUON_SELECT_TIGHT',' ','F')
        MUMASK=0
        DO I = 1,N
          IF(MUON_MASK(I).EQ.1) THEN
            MUMASK=IOR(MUMASK,2**(I-1))
          ENDIF
        ENDDO
        IF(IER.EQ.0) CALL EZGETA_i('MAX_3D_IMPACT',0,0,0,N,IER)
        IF(IER.EQ.0) 
     1    CALL EZGETA('MAX_3D_IMPACT',1,N,1,MAX_3D_IMPACT,IER)
C
        IF(IER.EQ.0) CALL EZGETA_i('MAX_BEND_IMPACT',0,0,0,N,IER)
        IF(IER.EQ.0) 
     1    CALL EZGETA('MAX_BEND_IMPACT',1,N,1,MAX_BEND_IMPACT,IER)
C
        IF(IER.EQ.0) CALL EZGETA_i('MAX_NONBEND_IMPACT',0,0,0,N,IER)
        IF(IER.EQ.0) 
     1     CALL EZGETA('MAX_NONBEND_IMPACT',1,N,1,MAX_NONBEND_IMPACT,
     2      IER)
C
        IF(IER.EQ.0) CALL EZGETA_i('MIN_T0FLOAT',0,0,0,N,IER)
        IF(IER.EQ.0) 
     1    CALL EZGETA('MIN_T0FLOAT',1,N,1,MIN_T0FLOAT,IER)
C
        IF(IER.EQ.0) CALL EZGETA_i('MAX_T0FLOAT',0,0,0,N,IER)
        IF(IER.EQ.0) 
     1    CALL EZGETA('MAX_T0FLOAT',1,N,1,MAX_T0FLOAT,IER)
C
        IF(IER.EQ.0) CALL EZGETA_i('MIN_OPP_MIPEN',0,0,0,N,IER)
        IF(IER.EQ.0)
     1     CALL EZGETA('MIN_OPP_MIPEN',1,N,1,MIN_OPP_MIPEN,IER)
C
        IF(IER.EQ.0) CALL EZGETA_i('IFW1_NOMISS_A',0,0,0,N,IER)
        IF(IER.EQ.0) 
     1    CALL EZGETA_iarr('IFW1_NOMISS_A',1,N,1,IFW1_NOMISS_A,IER)
C
        IF(IER.EQ.0) CALL EZGETA_i('IFW1_NOMISS_B',0,0,0,N,IER)
        IF(IER.EQ.0) 
     1    CALL EZGETA_iarr('IFW1_NOMISS_B',1,N,1,IFW1_NOMISS_B,IER)
C
        IF(IER.EQ.0) CALL EZGETA_i('IFW1_NOMISS_C',0,0,0,N,IER)
        IF(IER.EQ.0) 
     1    CALL EZGETA_iarr('IFW1_NOMISS_C',1,N,1,IFW1_NOMISS_C,IER)
C
        IF(IER.EQ.0) CALL EZGET_i('TOTAL_OR_USED_HITS',IHITS,IER)
C
        IF(IER.EQ.0) CALL EZGETA_i('MIN_HIT_WAM_CF',0,0,0,N,IER)
        IF(IER.EQ.0)
     1     CALL EZGETA_iarr('MIN_HIT_WAM_CF',1,N,1,MIN_HIT_WAM_CF,IER)
C
        IF(IER.EQ.0) CALL EZGETA_i('MIN_HIT_WAM_EF',0,0,0,N,IER)
        IF(IER.EQ.0)
     1     CALL EZGETA_iarr('MIN_HIT_WAM_EF',1,N,1,MIN_HIT_WAM_EF,IER)
C
        IF(IER.EQ.0) CALL EZGETA_i('MIN_HIT_WAS_EF',0,0,0,N,IER)
        IF(IER.EQ.0)
     1     CALL EZGETA_iarr('MIN_HIT_WAS_EF',1,N,1,MIN_HIT_WAS_EF,IER)
C
        IF(IER.EQ.0) CALL EZGETA_i('MIN_HIT_WSS_EF',0,0,0,N,IER)
        IF(IER.EQ.0)
     1     CALL EZGETA_iarr('MIN_HIT_WSS_EF',1,N,1,MIN_HIT_WSS_EF,IER)
C
        IF(IER.EQ.0) CALL EZGETA_i('MIN_HIT_SAMUS',0,0,0,N,IER)
        IF(IER.EQ.0) 
     1    CALL EZGETA_iarr('MIN_HIT_SAMUS',1,N,1,MIN_HIT_SAMUS,IER)
C
        IF(IER.EQ.0) CALL EZGETA_i('MAX_IFW4',0,0,0,N,IER)
        IF(IER.EQ.0) CALL EZGETA_iarr('MAX_IFW4',1,N,1,MAX_IFW4,IER)
C
        IF(IER.EQ.0) 
     1      CALL EZGETA_i('MAX_BEND_QUAL',0,0,0,N,IER)
        IF(IER.EQ.0) 
     1      CALL EZGETA('MAX_BEND_QUAL',1,N,1,MAX_BEND_QUAL,IER)
C
        IF(IER.EQ.0) 
     1      CALL EZGETA_i('MAX_NONBEND_QUAL',0,0,0,N,IER)
        IF(IER.EQ.0) 
     1      CALL EZGETA('MAX_NONBEND_QUAL',1,N,1,MAX_NONBEND_QUAL,IER)
C
        IF(IER.EQ.0) CALL EZGETA_i('MAX_ZTRAK_DTHETA',0,0,0,N,IER)
        IF(IER.EQ.0) 
     1    CALL EZGETA('MAX_ZTRAK_DTHETA',1,N,1,MAX_ZTRAK_DTHETA,IER)
C
        IF(IER.EQ.0) CALL EZGETA_i('MAX_ZTRAK_DPHI',0,0,0,N,IER)
        IF(IER.EQ.0) 
     1    CALL EZGETA('MAX_ZTRAK_DPHI',1,N,1,MAX_ZTRAK_DPHI,IER)
C
        IF(IER.EQ.0) CALL EZGETA_i('MIN_ZTRAK_MULT',0,0,0,N,IER)
        IF(IER.EQ.0) 
     1   CALL EZGETA_iarr('MIN_ZTRAK_MULT',1,N,1,
     2     MIN_ZTRAK_MULT,IER)
C
        IF(IER.EQ.0) CALL EZGETA_i('MAX_ZTRAK_MULT',0,0,0,N,IER)
        IF(IER.EQ.0) 
     1   CALL EZGETA_iarr('MAX_ZTRAK_MULT',1,N,1,
     2     MAX_ZTRAK_MULT,IER)
C
        IF(IER.EQ.0) CALL EZGETA_i('MIN_CALMIP_CD',0,0,0,N,IER)
        IF(IER.EQ.0) CALL EZGET_l('DO_NOCD_GOOD_CAL',DO_NOCD_GOOD_CAL
     &       ,IER)
        IF(IER.EQ.0) CALL EZGETA('MIN_CALMIP_CD',1,N,1,
     1    MIN_CALMIP_CD,IER)
C
        IF(IER.EQ.0) CALL EZGETA_i('MIN_CALMIP_NOCD',0,0,0,N,IER)
        IF(IER.EQ.0) CALL EZGETA('MIN_CALMIP_NOCD',1,N,1,
     1    MIN_CALMIP_NOCD,IER)
C
        IF(IER.EQ.0) CALL EZGET('MIN_BDL',MIN_BDL,IER)
        IF(IER.EQ.0) CALL EZGET('MIN_ETA_BDL',MIN_ETA_BDL,IER)
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error in MULQ_MUON_SELECT_TIGHT_RCP',
     &    'MULQ_MUON_SELECT_TIGHT',' ','F')
        FIRST=.FALSE.
      ENDIF
C
      STATUS=0
      OK=-1
C
      IF(LPMUO.LE.0) GO TO 999 
      I_SET=1
      I_RESET=0
C
C *** determine which region this track is in
C
      IQUAD=IQ(LPMUO+7)
      IF(IQUAD.LT.5) THEN
C
C *** CF
C
        IRGN=1
      ELSEIF(IQUAD.LT.13) THEN
C
C *** EF + EF/SAMUS
C
        IRGN=2
      ELSE
C
C *** Samus only
C
        IRGN=5
      ENDIF
C
C *** Re-set selection bits in case they have been set in a
C *** previous pass of the code
C
      STATUS=0
C
C *** First do cuts which are common to all regions :
C ***
C ***  ====> Check that muon does not cross octant boundaries <====
C
      IF(JBIT(IQ(LPMUO+44),9).GT.0) CALL SBIT(I_SET,STATUS,2)
C
C ***  ====> Cut on minimum integral B.dl <====
C
      LMUOT=LQ(LPMUO-2)
      IF(LMUOT.GT.0) THEN
        IF(Q(LMUOT+22).LT.MIN_BDL) THEN
          IF(ABS(Q(LPMUO+16)).GT.MIN_ETA_BDL) THEN
            CALL SBIT(I_SET,STATUS,22)
          ENDIF
        ENDIF
      ENDIF
C
C *** unpack hit plane words 46/47 of PMUO - select available/used hits
C *** by setting IHITS to 1/2
C
      CALL DECODE_MUON_PLANE_INFO(LPMUO,IHITS,WAM_HIT,SAM_HIT)
C
C ***  ====>  Maximum Impact parameter cuts w.r.t. CT vertex position <====
C ***     3-D Impact, bend view, non-bend view
C
      IF(Q(LPMUO+41).GT.MAX_3D_IMPACT(IRGN)) 
     1  CALL SBIT(I_SET,STATUS,3)
      IF(Q(LPMUO+13).GE.MAX_BEND_IMPACT(IRGN,1)) THEN
C
C *** high momentum muons
C
        IF(ABS(Q(LPMUO+56)).GT.MAX_BEND_IMPACT(IRGN,3)) 
     1    CALL SBIT(I_SET,STATUS,4)
      ELSE
C
C *** low momentum muons
C
        TEMP=MAX_BEND_IMPACT(IRGN,2)/Q(LPMUO+13)
        IF(ABS(Q(LPMUO+56)).GT.TEMP) CALL SBIT(I_SET,STATUS,4)
      ENDIF
C
C *** nonbend
C
      IF(Q(LPMUO+13).GE.MAX_NONBEND_IMPACT(IRGN,1)) THEN
C
C *** high momentum muons
C
        IF(ABS(Q(LPMUO+57)).GT.MAX_NONBEND_IMPACT(IRGN,3)) 
     1    CALL SBIT(I_SET,STATUS,5)
      ELSE
C
C *** low momentum muons
C
        TEMP=MAX_NONBEND_IMPACT(IRGN,2)/Q(LPMUO+13)
        IF(ABS(Q(LPMUO+57)).GT.TEMP) CALL SBIT(I_SET,STATUS,5)
      ENDIF
C
C
C ***  ====> floating t0 offset <====
C
      IF(Q(LPMUO+24).LT.MIN_T0FLOAT(IRGN).OR.
     1  Q(LPMUO+24).GT.MAX_T0FLOAT(IRGN)) 
     2  CALL SBIT(I_SET,STATUS,6)
C
C ***  ====> calorimeter back-to-back mip cut <====
C
      IF(Q(LPMUO+88).GT.MIN_OPP_MIPEN(IRGN)) 
     1  CALL SBIT(I_SET,STATUS,7)
C
C ***  ====> cuts on IFW1 flags <====
C
      IF(LMUOT.GT.0) THEN
        ITEMP=IQ(LMUOT+4)
        IF(ITEMP.GE.10) ITEMP=ITEMP-10
        IF(ITEMP.EQ.0) GO TO 15
        GO TO (11,12,13,14),ITEMP
   11   CONTINUE
C
C *** Missing A-layer test 
C
          IF(ITEMP.GT.IFW1_NOMISS_A(IRGN)) 
     1      CALL SBIT(I_SET,STATUS,9)
          GO TO 15
   12   CONTINUE
C
C *** Missing B-layer test 
C
          ITEMP=ITEMP-1
          IF(ITEMP.GT.IFW1_NOMISS_B(IRGN)) 
     1      CALL SBIT(I_SET,STATUS,9)
          GO TO 15
   13   CONTINUE
C
C *** Missing C-layer test
C
          ITEMP=ITEMP-2
          IF(ITEMP.GT.IFW1_NOMISS_C(IRGN)) 
     1      CALL SBIT(I_SET,STATUS,9)
          GO TO 15
   14   CONTINUE
C
C *** Mising A and B-layers ====> junk ====> reject
C
          CALL SBIT(I_SET,STATUS,9)
   15   CONTINUE
      ENDIF
C
C ***  ====> IFW4 'quality' flag <====
C
      IF(IQ(LPMUO+9).GT.MAX_IFW4(IRGN)) 
     &  CALL SBIT(I_SET,STATUS,13)
C
C ***  ====> Bend View and Non-bend View Quality of fit <====
C
      IF(LMUOT.GT.0) THEN
        IF(Q(LMUOT+20).GT.MAX_BEND_QUAL(IRGN)) 
     1    CALL SBIT(I_SET,STATUS,14)
        IF(Q(LMUOT+21).GT.MAX_NONBEND_QUAL(IRGN)) 
     1    CALL SBIT(I_SET,STATUS,15)
      ENDIF
C
C ***  Central tracking system muon validation
C ***  ====> muon-Ztrak matching cuts <====
C ***        Ztrak-match multiplicity
C ***        Ztrak-angle matching
C
      IF(MIN_ZTRAK_MULT(IRGN).GT.0) THEN
C
C *** Ztrak match required -> test multiplicity
C
        IF((IQ(LPMUO+6).LT.MIN_ZTRAK_MULT(IRGN)).OR.
     1   (IQ(LPMUO+6).GT.MAX_ZTRAK_MULT(IRGN))) 
     2    THEN
          CALL SBIT(I_SET,STATUS,17)
          CALL SBIT(I_SET,STATUS,18)
        ELSE
          IF(IQ(LPMUO+6).GT.0) THEN
C
C *** multiplicity OK -> test match quality
C
            TEMP=Q(LPMUO+39)/CONV
            IF(TEMP.GT.MAX_ZTRAK_DTHETA(IRGN))
     1        CALL SBIT(I_SET,STATUS,18)
            TEMP=Q(LPMUO+38)/CONV
            IF(TEMP.GT.MAX_ZTRAK_DPHI(IRGN))
     1        CALL SBIT(I_SET,STATUS,18) 
          ENDIF
        ENDIF        
      ENDIF
C
C *** Now test for events for which the CD track is missing
C *** but which have a good mip trace
C
      IF(DO_NOCD_GOOD_CAL) THEN
        IF(MIN_ZTRAK_MULT(IRGN).GE.1.AND.IQ(LPMUO+6).LE.0) THEN
C
C *** match required but none found -> test against tight calmip cut
C *** to decide weather to keep this track or not
C
          IF(Q(LPMUO+34).GE.MIN_CALMIP_NOCD(IRGN)) THEN
            CALL SBIT(I_RESET,STATUS,17)
            CALL SBIT(I_RESET,STATUS,18)
          ENDIF
        ENDIF    
      ENDIF
C
C *** Calorimeter muon validation
C ***  ====> Check that the calorimeter deposition is consistent <==== 
C ***        with min-ionizing track in cone size of (hit cell + 2NN)
C
      IF(Q(LPMUO+34).LT.MIN_CALMIP_CD(IRGN))
     1  CALL SBIT(I_SET,STATUS,20)
C
C *** Now do region-dependent cuts
C *** Branch on muon region no.
C
      GO TO (16,20,30,40,50),IRGN
   16 CONTINUE
C--------------------------------------------------------------------------------
C *** Region 1 : Central chambers (CF)
C *** 
C ***  ====> Check on MUCTAG flags <====
C
      IF((JBIT(IQ(LPMUO+44),7).GT.0).OR.(JBIT(IQ(LPMUO+44),8).GT.0)) 
     1  CALL SBIT(I_SET,STATUS,1)
C
C *** Cuts on specific plane multiplicities
C *** CF - Quads 1-4
C
      IF(WAM_HIT(1).LT.MIN_HIT_WAM_CF(1)) 
     1  CALL SBIT(I_SET,STATUS,10)
      IF(WAM_HIT(2).LT.MIN_HIT_WAM_CF(2)) 
     1  CALL SBIT(I_SET,STATUS,10)
      IF(WAM_HIT(3).LT.MIN_HIT_WAM_CF(3)) 
     1  CALL SBIT(I_SET,STATUS,10)
      ITEMP=WAM_HIT(1)+WAM_HIT(2)+WAM_HIT(3)
      IF(ITEMP.LT.MIN_HIT_WAM_CF(4)) 
     1  CALL SBIT(I_SET,STATUS,10)
C
      GO TO 60
   20 CONTINUE
C ------------------------------------------------------------------------------
C *** Region 2 : Pure WAMUS EF tracks
C ***
C *** Cuts on specific plane multiplicities
C *** EF Quads 5-12
C
      ITEMP=WAM_HIT(1)+WAM_HIT(5)
      IF(ITEMP.LT.MIN_HIT_WAM_EF(1)) CALL 
     &  SBIT(I_SET,STATUS,10)
C
      ITEMP=WAM_HIT(2)+WAM_HIT(6)
      IF(ITEMP.LT.MIN_HIT_WAM_EF(2)) 
     &  CALL SBIT(I_SET,STATUS,10)
C
      ITEMP=WAM_HIT(3)+WAM_HIT(4)
      IF(ITEMP.LT.MIN_HIT_WAM_EF(3)) 
     &  CALL SBIT(I_SET,STATUS,10)
C
      ITEMP=ITEMP+WAM_HIT(1)+WAM_HIT(2)+WAM_HIT(5)+WAM_HIT(6)
      IF(ITEMP.LT.MIN_HIT_WAM_EF(4)) 
     &  CALL SBIT(I_SET,STATUS,10)
C
C *** EF Level 2 HITPLN cuts
C
      IOK=1
      CALL MUON_EF_LVL2_HTPLN(WAM_HIT,SAM_HIT,IOK)
      IF(IOK.LT.0) CALL SBIT(I_SET,STATUS,11)
C
      GO TO 60
   30 CONTINUE
C------------------------------------------------------------------------------
C *** Region 3 : SAMUS A + WAMUS B,C / EF
C ***
C
C *** Cuts on specific plane multiplicities
C *** EF Quads 5-12
C
      ITEMP=WAM_HIT(1)+WAM_HIT(5)+SAM_HIT(1)
      IF(ITEMP.LT.MIN_HIT_WAS_EF(1)) 
     &  CALL SBIT(I_SET,STATUS,10)
C
      ITEMP=WAM_HIT(2)+WAM_HIT(6)
      IF(ITEMP.LT.MIN_HIT_WAS_EF(2)) 
     &  CALL SBIT(I_SET,STATUS,10)
C
      ITEMP=WAM_HIT(3)+WAM_HIT(4)
      IF(ITEMP.LT.MIN_HIT_WAS_EF(3)) 
     &  CALL SBIT(I_SET,STATUS,10)
C
      ITEMP=ITEMP+WAM_HIT(1)+WAM_HIT(2)+WAM_HIT(5)+WAM_HIT(6)+SAM_HIT(1)
      IF(ITEMP.LT.MIN_HIT_WAS_EF(4)) 
     &  CALL SBIT(I_SET,STATUS,10)
C
C *** EF Level 2 HITPLN cuts
C
      IOK=1
      CALL MUON_EF_LVL2_HTPLN(WAM_HIT,SAM_HIT,IOK)
      IF(IOK.LT.0) CALL SBIT(I_SET,STATUS,11)
C
      GO TO 60
   40 CONTINUE
C------------------------------------------------------------------------------
C *** Region 4 : SAMUS A,B + WAMUS C /EF
C ***
C
C *** Cuts on specific plane multiplicities
C *** EF Quads 5-12
C
      ITEMP=WAM_HIT(1)+WAM_HIT(5)+SAM_HIT(1)
      IF(ITEMP.LT.MIN_HIT_WSS_EF(1)) 
     &  CALL SBIT(I_SET,STATUS,10)
C
      ITEMP=WAM_HIT(2)+WAM_HIT(6)+SAM_HIT(2)
      IF(ITEMP.LT.MIN_HIT_WSS_EF(2)) 
     &  CALL SBIT(I_SET,STATUS,10)
C
      ITEMP=WAM_HIT(3)+WAM_HIT(4)
      IF(ITEMP.LT.MIN_HIT_WSS_EF(3)) 
     &  CALL SBIT(I_SET,STATUS,10)
C
      ITEMP=ITEMP+WAM_HIT(1)+WAM_HIT(2)+WAM_HIT(5)+WAM_HIT(6)
     1  +SAM_HIT(1)+SAM_HIT(2)
      IF(ITEMP.LT.MIN_HIT_WSS_EF(4)) 
     &  CALL SBIT(I_SET,STATUS,10)
C
C *** EF Level 2 HITPLN cuts
C
      IOK=1
      CALL MUON_EF_LVL2_HTPLN(WAM_HIT,SAM_HIT,IOK)
      IF(IOK.LT.0) CALL SBIT(I_SET,STATUS,11)
C
      GO TO 60
   50 CONTINUE
C------------------------------------------------------------------------------
C *** Region 5 : SAMUS A,B,C
C ***
C
C *** Cuts on specific plane multiplicities
C *** Samus
C
      IF(SAM_HIT(1).LT.MIN_HIT_SAMUS(1)) 
     1  CALL SBIT(I_SET,STATUS,12)
      IF(SAM_HIT(2).LT.MIN_HIT_SAMUS(2)) 
     1  CALL SBIT(I_SET,STATUS,12)
      IF(SAM_HIT(3).LT.MIN_HIT_SAMUS(3)) 
     1  CALL SBIT(I_SET,STATUS,12)
      ITEMP=SAM_HIT(1)+SAM_HIT(2)+SAM_HIT(3)
      IF(ITEMP.LT.MIN_HIT_SAMUS(4)) 
     1  CALL SBIT(I_SET,STATUS,12)
C
C
C------------------------------------------------------------------------------
   60 CONTINUE
C
C ***  ----> End of Selection/Validation <------
C ***     check value of cut flag and set global reject bit,
C *** Finally check against MUMASK
C
      OK=1
      IF(IAND(STATUS,MUMASK).NE.0) THEN
        OK=-1
      ENDIF
C------------------------------------------------------------------------------
  999 RETURN
      END
