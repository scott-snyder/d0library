      SUBROUTINE TOP_LEPTONS_CLEANMU_INIT_SUMMRY(LUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CLEANMU Initialization Summary
C-
C-   Inputs  :
C-              LUN - i/o unit no for printout
C-   Outputs :
C-   Controls:
C-
C-   Created   3-FEB-1993   Stephen J. Wimpenny
C-   Modified 17-Mar-1993   Change in name for library compatibility
C-   Modified  4-Jul-1993   New version of CLEANMU
C-   Modified 13-Feb-1994   Dual Muon Quality Def'n Implemented
C-                          A-stub cut (bit 8) added
C-   Modified  1-Apr-1994   Loose IFW4 cut (bit 16) added
C-                          1NN mip cut (bit 21) added
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL USE_MUON_MASK2
      LOGICAL FIND_TOP,FIND_TOP_MUMU,FIND_TOP_MUJT
      LOGICAL FIND_Z,FIND_Z_MUMU
C
      INTEGER LUN,I,J,N,IER,ITEST
      INTEGER MUON_MASK_1(32),MUON_MASK_2(32)
      INTEGER IFW1_NOMISS_A(5),IFW1_NOMISS_B(5),IFW1_NOMISS_C(5)
      INTEGER MIN_HIT_WAM_CF(4),MIN_HIT_WAM_EF(4),MIN_HIT_WAS_EF(4)
      INTEGER MIN_HIT_WSS_EF(4),MIN_HIT_SAMUS(4)
      INTEGER MAX_IFW4_TIGHT(5),MAX_IFW4_LOOSE(5),IHITS
      INTEGER MIN_ZTRAK_MULT(5),MAX_ZTRAK_MULT(5)
      INTEGER MUJET_NOMU_MIN
C
      REAL MAX_3D_IMPACT(5),MAX_BEND_IMPACT(5,3),MAX_NONBEND_IMPACT(5,3)
      REAL MIN_T0FLOAT(5),MAX_T0FLOAT(5),MIN_OPP_MIPEN(5)
      REAL MIN_ETA_BDL,MIN_BDL
      REAL MAX_BEND_QUAL(5),MAX_NONBEND_QUAL(5)
      REAL MAX_ZTRAK_DTHETA(5),MAX_ZTRAK_DPHI(5)
      REAL MIN_CALMIP_CD_2NN(5),MIN_CALMIP_CD_1NN(5),MIN_CALMIP_NOCD(5)
C
      DATA ITEST/ 0/
C
      IER = 0
C
C *** Muon id masks - from TOP_LEPTONS_RCP 
C
      CALL EZPICK('TOP_LEPTONS_RCP')
C      CALL ERRMSG('Reading TOP_LEPTONS_RCP',
C     1  'TOP_LEPTONS_CLEANMU_INIT_SUMMRY',' ','W')
C
C *** User Muon_id mask (Golden Muons)
C
        CALL EZGETA('MUON_MASK_1',0,0,0,N,IER)
        IF(N.LT.32) WRITE(LUN,1000) N
        IF(IER.EQ.0) CALL EZGETA('MUON_MASK_1',1,N,1,MUON_MASK_1,IER)
        IF(N.NE.32) CALL ERRMSG('Error reading Golden Muon Mask Array',
     1    'TOP_LEPTONS_CLEANMU_INIT',' ','F')
C
C *** User Muon_id mask (Silver Muons)
C
        CALL EZGETA('MUON_MASK_2',0,0,0,N,IER)
        IF(N.LT.32) WRITE(LUN,1001) N
        IF(IER.EQ.0) CALL EZGETA('MUON_MASK_2',1,N,1,MUON_MASK_2,IER)
        IF(N.NE.32) CALL ERRMSG('Error reading Silver Muon Mask Array',
     1    'TOP_LEPTONS_CLEANMU_INIT',' ','F')
C
C *** Check to see if any of the finder algorithms which use silver
C *** muon definitions have been enabled. If so then force useage flag
C *** to true.
C
        IF(IER.EQ.0) CALL EZGET('FINDTOP_CAND',FIND_TOP,IER)
        IF(FIND_TOP) THEN
          IF(IER.EQ.0) CALL EZGET('FINDTOP_MUMU',FIND_TOP_MUMU,IER)
          IF(FIND_TOP_MUMU) THEN
            ITEST=ITEST+1
          ENDIF
          IF(IER.EQ.0) CALL EZGET('FINDTOP_MUJET',FIND_TOP_MUJT,IER)
          IF(IER.EQ.0) CALL EZGET('LJ_NOMUON_MIN',MUJET_NOMU_MIN,IER)
          IF(FIND_TOP_MUJT.AND.MUJET_NOMU_MIN.GT.1) THEN
            ITEST=ITEST+1
          ENDIF
        ENDIF
        IF(ITEST.GT.0) USE_MUON_MASK2=.TRUE.
C
        IF(IER.EQ.0) CALL EZGET('FINDZ_CAND',FIND_Z,IER)
        IF(FIND_Z) THEN
          IF(IER.EQ.0) CALL EZGET('FINDZ_MUMU',FIND_Z_MUMU,IER)
          IF(FIND_Z_MUMU) THEN
            ITEST=ITEST+1
          ENDIF
        ENDIF
C
      CALL EZRSET
      IF (IER.NE.0) CALL ERRMSG('Error in TOP_LEPTONS_RCP',
     &  'TOP_LEPTONS_CLEANMU_INIT_SUMMRY',' ','F')
C
C *** Muon id cuts - from CLEANMU_RCP 
C
      CALL INRCP('CLEANMU_RCP',IER)
      IF(IER.NE.0)
     1  CALL ERRMSG('CLEANMU_RCP not found',
     2    'TOP_LEPTONS_CLEANMU_INIT_SUMMRY',' ','W')
      CALL EZPICK('CLEANMU_RCP')
C
C *** Muon id cuts
C
      IF(IER.EQ.0) CALL EZGETA('MAX_3D_IMPACT',0,0,0,N,IER)
      IF(IER.EQ.0)
     1  CALL EZGETA('MAX_3D_IMPACT',1,N,1,MAX_3D_IMPACT,IER)
      IF(IER.EQ.0) CALL EZGETA('MAX_BEND_IMPACT',0,0,0,N,IER)
      IF(IER.EQ.0)
     1  CALL EZGETA('MAX_BEND_IMPACT',1,N,1,MAX_BEND_IMPACT,IER)
      IF(IER.EQ.0) CALL EZGETA('MAX_NONBEND_IMPACT',0,0,0,N,IER)
      IF(IER.EQ.0)
     1   CALL EZGETA('MAX_NONBEND_IMPACT',1,N,1,MAX_NONBEND_IMPACT,IER)
      IF(IER.EQ.0) CALL EZGETA('MIN_T0FLOAT',0,0,0,N,IER)
      IF(IER.EQ.0)
     1  CALL EZGETA('MIN_T0FLOAT',1,N,1,MIN_T0FLOAT,IER)
      IF(IER.EQ.0) CALL EZGETA('MAX_T0FLOAT',0,0,0,N,IER)
      IF(IER.EQ.0)
     1  CALL EZGETA('MAX_T0FLOAT',1,N,1,MAX_T0FLOAT,IER)
      IF(IER.EQ.0) CALL EZGETA('MIN_OPP_MIPEN',0,0,0,N,IER)
      IF(IER.EQ.0)
     1  CALL EZGETA('MIN_OPP_MIPEN',1,N,1,MIN_OPP_MIPEN,IER)
      IF(IER.EQ.0) CALL EZGETA('IFW1_NOMISS_A',0,0,0,N,IER)
      IF(IER.EQ.0)
     1  CALL EZGETA('IFW1_NOMISS_A',1,N,1,IFW1_NOMISS_A,IER)
      IF(IER.EQ.0) CALL EZGETA('IFW1_NOMISS_B',0,0,0,N,IER)
      IF(IER.EQ.0)
     1  CALL EZGETA('IFW1_NOMISS_B',1,N,1,IFW1_NOMISS_B,IER)
      IF(IER.EQ.0) CALL EZGETA('IFW1_NOMISS_C',0,0,0,N,IER)
      IF(IER.EQ.0)
     1  CALL EZGETA('IFW1_NOMISS_C',1,N,1,IFW1_NOMISS_C,IER)
      IF(IER.EQ.0) CALL EZGET('TOTAL_OR_USED_HITS',IHITS,IER)
      IF(IER.EQ.0) CALL EZGETA('MIN_HIT_WAM_CF',0,0,0,N,IER)
      IF(IER.EQ.0)
     1  CALL EZGETA('MIN_HIT_WAM_CF',1,N,1,MIN_HIT_WAM_CF,IER)
      IF(IER.EQ.0) CALL EZGETA('MIN_HIT_WAM_EF',0,0,0,N,IER)
      IF(IER.EQ.0)
     1  CALL EZGETA('MIN_HIT_WAM_EF',1,N,1,MIN_HIT_WAM_EF,IER)
      IF(IER.EQ.0) CALL EZGETA('MIN_HIT_WAS_EF',0,0,0,N,IER)
      IF(IER.EQ.0)
     1  CALL EZGETA('MIN_HIT_WAS_EF',1,N,1,MIN_HIT_WAS_EF,IER)
      IF(IER.EQ.0) CALL EZGETA('MIN_HIT_WSS_EF',0,0,0,N,IER)
      IF(IER.EQ.0)
     1  CALL EZGETA('MIN_HIT_WSS_EF',1,N,1,MIN_HIT_WSS_EF,IER)
      IF(IER.EQ.0) CALL EZGETA('MIN_HIT_SAMUS',0,0,0,N,IER)
      IF(IER.EQ.0)
     1  CALL EZGETA('MIN_HIT_SAMUS',1,N,1,MIN_HIT_SAMUS,IER)
C
      IF(IER.EQ.0) CALL EZGETA('MAX_IFW4',0,0,0,N,IER)
      IF(IER.EQ.0) CALL EZGETA('MAX_IFW4',1,N,1,
     1  MAX_IFW4_TIGHT,IER)
      IF(IER.EQ.0) CALL EZGETA('MAX_IFW4_LOOSE',0,0,0,N,IER)
      IF(IER.EQ.0) CALL EZGETA('MAX_IFW4_LOOSE',1,N,1,
     1  MAX_IFW4_LOOSE,IER)
C
      IF(IER.EQ.0)
     1  CALL EZGETA('MAX_BEND_QUAL',0,0,0,N,IER)
      IF(IER.EQ.0)
     1  CALL EZGETA('MAX_BEND_QUAL',1,N,1,MAX_BEND_QUAL,IER)
      IF(IER.EQ.0)
     1  CALL EZGETA('MAX_NONBEND_QUAL',0,0,0,N,IER)
      IF(IER.EQ.0)
     1  CALL EZGETA('MAX_NONBEND_QUAL',1,N,1,MAX_NONBEND_QUAL,IER)
      IF(IER.EQ.0) CALL EZGETA('MAX_ZTRAK_DTHETA',0,0,0,N,IER)
      IF(IER.EQ.0)
     1  CALL EZGETA('MAX_ZTRAK_DTHETA',1,N,1,MAX_ZTRAK_DTHETA,IER)
      IF(IER.EQ.0) CALL EZGETA('MAX_ZTRAK_DPHI',0,0,0,N,IER)
      IF(IER.EQ.0)
     1  CALL EZGETA('MAX_ZTRAK_DPHI',1,N,1,MAX_ZTRAK_DPHI,IER)
      IF(IER.EQ.0) CALL EZGETA('MIN_ZTRAK_MULT',0,0,0,N,IER)
      IF(IER.EQ.0)
     1  CALL EZGETA('MIN_ZTRAK_MULT',1,N,1,MIN_ZTRAK_MULT,IER)
      IF(IER.EQ.0) CALL EZGETA('MAX_ZTRAK_MULT',0,0,0,N,IER)
      IF(IER.EQ.0)
     1  CALL EZGETA('MAX_ZTRAK_MULT',1,N,1,MAX_ZTRAK_MULT,IER)
C
      IF(IER.EQ.0) CALL EZGETA('MIN_CALMIP_CD',0,0,0,N,IER)
      IF(IER.EQ.0) CALL EZGETA('MIN_CALMIP_CD',1,N,1,
     1  MIN_CALMIP_CD_2NN,IER)
      IF(IER.EQ.0) CALL EZGETA('MIN_CALMIP_1NN',0,0,0,N,IER)
      IF(IER.EQ.0) CALL EZGETA('MIN_CALMIP_1NN',1,N,1,
     1  MIN_CALMIP_CD_1NN,IER)
      IF(IER.EQ.0) CALL EZGETA('MIN_CALMIP_NOCD',0,0,0,N,IER)
      IF(IER.EQ.0) CALL EZGETA('MIN_CALMIP_NOCD',1,N,1,
     1  MIN_CALMIP_NOCD,IER)
C
      IF(IER.EQ.0) CALL EZGET('MIN_BDL',MIN_BDL,IER)
      IF(IER.EQ.0) CALL EZGET('MIN_ETA_BDL',MIN_ETA_BDL,IER)
C
      CALL EZRSET
      IF (IER.NE.0) CALL ERRMSG('Error in CLEANMU_RCP',
     &  'TOP_LEPTONS_CLEANMU_INIT_SUMMRY',' ','F')
C
C *** List Golden Muon id/validation cuts
C
      WRITE(LUN,2000)
C
      WRITE(LUN,2002)
C
C *** Loop over muon mask bits and print for chosen bits
C *** for golden muon definition
C
      DO J=1,32
        IF(MUON_MASK_1(J).GT.0) THEN
          GO TO (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
     &      20,21,22,23,24,25,26,27,28,29,30,31,32),J
        ELSE
          GO TO 500
        ENDIF
   1    CONTINUE
C
C *** Bit 1 - MUCTAG
C
        WRITE(LUN,2010)
        GO TO 500
   2    CONTINUE
C
C *** Bit 2 - Trigger Octant Boundary
C
        WRITE(LUN,2012)
        GO TO 500
   3    CONTINUE
C
C *** Bit 3 - 3-D Impact Parameter
C
        WRITE(LUN,2014) (MAX_3D_IMPACT(I),I=1,5)
        GO TO 500
   4    CONTINUE
C
C *** Bit 4 - Bend view impact parameter
C
        WRITE(LUN,2016) MAX_BEND_IMPACT
        GO TO 500
   5    CONTINUE
C
C *** Bit 5 - Non-bend view impact parameter
C
        WRITE(LUN,2018) MAX_NONBEND_IMPACT
        GO TO 500
   6    CONTINUE
C
C *** Bit 6 - Floating t0 offset
C
        WRITE(LUN,2020) (MIN_T0FLOAT(I),I=1,5),(MAX_T0FLOAT(I),I=1,5)
        GO TO 500
   7    CONTINUE
C
C *** Bit 7 - back-to-back mip trace
C
        WRITE(LUN,2022) (MIN_OPP_MIPEN(I),I=1,5)
        GO TO 500
   8    CONTINUE
C
C *** Bit 8 - A-layer stub killer
C
        WRITE(LUN,2024)
        GO TO 500
   9    CONTINUE
C
C *** Bit 9 - Global Plane Cuts (IFW1)
C
        WRITE(LUN,2026) (IFW1_NOMISS_A(I),I=1,5),(IFW1_NOMISS_B(I),I=1,
     &    5),(IFW1_NOMISS_C(I),I=1,5)
        GO TO 500
  10    CONTINUE
C
C *** Bit 10 - Minimum hit requirements WAMUS, WAMUS/SAMUS
C
        IF(IHITS.EQ.1) THEN 
          WRITE(LUN,2028) (MIN_HIT_WAM_CF(I),I=1,4),(MIN_HIT_WAM_EF(I),
     &    I=1,4),(MIN_HIT_WAS_EF(I),I=1,4),(MIN_HIT_WSS_EF(I),I=1,4)
        ELSE
          WRITE(LUN,2029) (MIN_HIT_WAM_CF(I),I=1,4),(MIN_HIT_WAM_EF(I),
     &    I=1,4),(MIN_HIT_WAS_EF(I),I=1,4),(MIN_HIT_WSS_EF(I),I=1,4)
        ENDIF
        GO TO 500
  11    CONTINUE
C
C *** Bit 11 - Level 2 Muhtpln cuts
C
        WRITE(LUN,2030)
        GO TO 500
  12    CONTINUE
C
C *** Bit 12 - minimum hit requirements SAMUS
C
        IF(IHITS.EQ.1) THEN
          WRITE(LUN,2032) (MIN_HIT_SAMUS(I),I=1,4)
        ELSE
          WRITE(LUN,2033) (MIN_HIT_SAMUS(I),I=1,4)
        ENDIF
        GO TO 500
  13    CONTINUE
C
C *** Bit 13 - IFW4 cut (tight)
C
        WRITE(LUN,2034) (MAX_IFW4_TIGHT(I),I=1,5)
        GO TO 500
  14    CONTINUE
C
C *** Bit 14 - Bend view quality of fit
C
        WRITE(LUN,2036) (MAX_BEND_QUAL(I),I=1,5)
        GO TO 500
  15    CONTINUE
C
C *** Bit 15 - Non-bend view quality of fit
C
        WRITE(LUN,2038) (MAX_NONBEND_QUAL(I),I=1,5)
        GO TO 500
  16    CONTINUE
C
C *** Bit 16 - IFW4 cut (loose)
C
        WRITE(LUN,2040) (MAX_IFW4_LOOSE(I),I=1,5)
        GO TO 500
  17    CONTINUE
C
C *** Bit 17 - Ztrak match multiplicity cuts
C
        WRITE(LUN,2042) (MIN_ZTRAK_MULT(I),I=1,5),
     1    (MAX_ZTRAK_MULT(I),I=1,5)
        GO TO 500
  18    CONTINUE
C
C *** Bit 18 - Ztrak match angle cuts
C
        WRITE(LUN,2044) (MAX_ZTRAK_DTHETA(I),I=1,5),
     1    (MAX_ZTRAK_DPHI(I),I=1,5)
        GO TO 500
  19    CONTINUE
C
C *** Bit 19 - not used
C
        GO TO 500
  20    CONTINUE
C
C *** Bit 20 - Calorimeter mip cut 2NN
C
        WRITE(LUN,2048) (MIN_CALMIP_CD_2NN(I),I=1,5),
     1    (MIN_CALMIP_NOCD(I),I=1,5)
        GO TO 500
  21    CONTINUE
C
C *** Bit 21 - Calorimeter mip cut 1NN
C
        WRITE(LUN,2050) (MIN_CALMIP_CD_1NN(I),I=1,5),
     1    (MIN_CALMIP_NOCD(I),I=1,5)
        GO TO 500
  22    CONTINUE
C
C ** Bit 22 - Integral B.dl cut
C
        WRITE(LUN,2052) MIN_BDL,MIN_ETA_BDL
        GO TO 500
  23    CONTINUE
C
C *** Bit 23 - not used
C
        GO TO 500
  24    CONTINUE
C
C *** Bit 24 - not used
C
        GO TO 500
  25    CONTINUE
C
C *** Bit 25 - not used
C
        GO TO 500
  26    CONTINUE
C
C *** Bit 26 - not used
C
        GO TO 500
  27    CONTINUE
C
C *** Bit 27 - not used
C
        GO TO 500
  28    CONTINUE
C
C *** Bit 28 - not used
C
        GO TO 500
  29    CONTINUE
C
C *** Bit 29 - not used
C
        GO TO 500
  30    CONTINUE
C
C *** Bit 30 - not used
C
        GO TO 500
  31    CONTINUE
C
C *** Bit 31 - not used
C
        GO TO 500
  32    CONTINUE
C
C *** Bit 32 - not used
C
  500   CONTINUE
      ENDDO
      IF(.NOT.USE_MUON_MASK2) GO TO 502
C
C *** List Silver Muon id/validation cuts
C
      WRITE(LUN,2004)
C
C *** Loop over muon mask bits and print for chosen bits
C *** for silver muon definition
C
      DO J=1,32
        IF(MUON_MASK_2(J).GT.0) THEN
          GO TO (41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,
     1      56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,
     2      72),J
        ELSE
          GO TO 501
        ENDIF
  41    CONTINUE
C
C *** Bit 1 - MUCTAG
C
        WRITE(LUN,2010)
        GO TO 501
  42    CONTINUE
C
C *** Bit 2 - Trigger Octant Boundary
C
        WRITE(LUN,2012)
        GO TO 501
  43    CONTINUE
C
C *** Bit 3 - 3-D Impact Parameter
C
        WRITE(LUN,2014) (MAX_3D_IMPACT(I),I=1,5)
        GO TO 501
  44    CONTINUE
C
C *** Bit 4 - Bend view impact parameter
C
        WRITE(LUN,2016) MAX_BEND_IMPACT
        GO TO 501
  45    CONTINUE
C
C *** Bit 5 - Non-bend view impact parameter
C
        WRITE(LUN,2018) MAX_NONBEND_IMPACT
        GO TO 501
  46    CONTINUE
C
C *** Bit 6 - Floating t0 offset
C
        WRITE(LUN,2020) (MIN_T0FLOAT(I),I=1,5),(MAX_T0FLOAT(I),I=1,5)
        GO TO 501
  47    CONTINUE
C
C *** Bit 7 - back-to-back mip trace
C
        WRITE(LUN,2022) (MIN_OPP_MIPEN(I),I=1,5)
        GO TO 501
  48    CONTINUE
C
C *** Bit 8 - A-layer stub removal
C
        WRITE(LUN,2024)
        GO TO 501
  49    CONTINUE
C
C *** Bit 9 - Global Plane Cuts (IFW1)
C
        WRITE(LUN,2026) (IFW1_NOMISS_A(I),I=1,5),(IFW1_NOMISS_B(I),I=1,
     &    5),(IFW1_NOMISS_C(I),I=1,5)
        GO TO 501
  50    CONTINUE
C
C *** Bit 10 - Minimum hit requirements WAMUS, WAMUS/SAMUS
C
        IF(IHITS.EQ.1) THEN 
          WRITE(LUN,2028) (MIN_HIT_WAM_CF(I),I=1,4),(MIN_HIT_WAM_EF(I),
     &    I=1,4),(MIN_HIT_WAS_EF(I),I=1,4),(MIN_HIT_WSS_EF(I),I=1,4)
        ELSE
          WRITE(LUN,2029) (MIN_HIT_WAM_CF(I),I=1,4),(MIN_HIT_WAM_EF(I),
     &    I=1,4),(MIN_HIT_WAS_EF(I),I=1,4),(MIN_HIT_WSS_EF(I),I=1,4)
        ENDIF
        GO TO 501
  51    CONTINUE
C
C *** Bit 11 - Level 2 Muhtpln cuts
C
        WRITE(LUN,2030)
        GO TO 501
  52    CONTINUE
C
C *** Bit 12 - minimum hit requirements SAMUS
C
        IF(IHITS.EQ.1) THEN
          WRITE(LUN,2032) (MIN_HIT_SAMUS(I),I=1,4)
        ELSE
          WRITE(LUN,2033) (MIN_HIT_SAMUS(I),I=1,4)
        ENDIF
        GO TO 501
  53    CONTINUE
C
C *** Bit 13 - IFW4 cut
C
        WRITE(LUN,2034) (MAX_IFW4_TIGHT(I),I=1,5)
        GO TO 501
  54    CONTINUE
C
C *** Bit 14 - Bend view quality of fit
C
        WRITE(LUN,2036) (MAX_BEND_QUAL(I),I=1,5)
        GO TO 501
  55    CONTINUE
C
C *** Bit 15 - Non-bend view quality of fit
C
        WRITE(LUN,2038) (MAX_NONBEND_QUAL(I),I=1,5)
        GO TO 501
  56    CONTINUE
C
C *** Bit 16 - IFW4 cut (loose)
C
        WRITE(LUN,2040) (MAX_IFW4_LOOSE(I),I=1,5)
        GO TO 501
  57    CONTINUE
C
C *** Bit 17 - Ztrak match multiplicity cuts
C
        WRITE(LUN,2042) (MIN_ZTRAK_MULT(I),I=1,5),
     1    (MAX_ZTRAK_MULT(I),I=1,5)
        GO TO 501
  58    CONTINUE
C
C *** Bit 18 - Ztrak match angle cuts
C
        WRITE(LUN,2044) (MAX_ZTRAK_DTHETA(I),I=1,5),
     1    (MAX_ZTRAK_DPHI(I),I=1,5)
        GO TO 501
  59    CONTINUE
C
C *** Bit 19 - not used
C
        GO TO 501
  60    CONTINUE
C
C *** Bit 20 - Calorimeter mip cut
C
        WRITE(LUN,2048) (MIN_CALMIP_CD_2NN(I),I=1,5),
     1    (MIN_CALMIP_NOCD(I),I=1,5)
        GO TO 501
  61    CONTINUE
C
C *** Bit 21 - Calorimeter mip cut 1NN
C
        WRITE(LUN,2050) (MIN_CALMIP_CD_1NN(I),I=1,5),
     1    (MIN_CALMIP_NOCD(I),I=1,5)
        GO TO 501
  62    CONTINUE
C
C ** Bit 22 - Integral B.dl cut
C
        WRITE(LUN,2052) MIN_BDL,MIN_ETA_BDL
        GO TO 501
  63    CONTINUE
C
C *** Bit 23 - not used
C
        GO TO 501
  64    CONTINUE
C
C *** Bit 24 not used
C
        GO TO 501
  65    CONTINUE
C
C *** Bit 25 - not used
C
        GO TO 501
  66    CONTINUE
C
C *** Bit 26 - not used
C
        GO TO 501
  67    CONTINUE
C
C *** Bit 27 - not used
C
        GO TO 501
  68    CONTINUE
C
C *** Bit 28 - not used
C
        GO TO 501
  69    CONTINUE
C
C *** Bit 29 - not used
C
        GO TO 501
  70    CONTINUE
C
C *** Bit 30 - not used
C
        GO TO 501
  71    CONTINUE
C
C *** Bit 31 - not used
C
        GO TO 501
  72    CONTINUE
C
C *** Bit 32 - not used
C
  501   CONTINUE
      ENDDO
  502 CONTINUE
C----------------------------------------------------------------------
  999 RETURN
 1000 FORMAT(5X,' TOP_LEPTONS_CLEANMU_INIT_SUMMRY : RCP Error in',
     1  ' Muon Bitmask 1 ',/15X,' expected 32 bits , found ',
     2  I3,' bits')
 1001 FORMAT(5X,' TOP_LEPTONS_CLEANMU_INIT_SUMMRY : RCP Error in',
     1  ' Muon Bitmask 2 ',/15X,' expected 32 bits , found ',
     2  I3,' bits')
C
 2000 FORMAT(
     1 10X,' Muon Detector Region index 1->5 corresponds to :',//
     2 15X,'   1   -   WAMUS CF only ',/,
     3 15X,'   2   -   WMUUS EF only ',/,
     4 15X,'   3   -   Overlap region  i.) SAMUS A + WAMUS BC ',/,
     5 15X,'   4   -   Overlap region ii.) SAMUS AB + WAMUS C ',/,
     6 15X,'   5   -   SAMUS only ',//)
 2002 FORMAT(5X,' Golden Muon id ( by region index ) :',//)
 2004 FORMAT(///,5X,' Silver Muon id ( by region index ) :',//)
 2010 FORMAT(10X,' MUCTAG Cosmic Ray VETO Applied to CF trcaks ')
 2012 FORMAT(10X,' Trigger Octant boundary cut applied to all regions ')
 2014 FORMAT(10X,' Max 3-D Impact Parmeter wrt Vertex (cm)   = ',5F5.1)
 2016 FORMAT(10X,' Max Bend Plane Impact Parameter Cuts (cm) = ',
     1 /,40X,5F6.1,/,40X,5F6.1,/,40X,5F6.1)
 2018 FORMAT(10X,' Max Nonbend Plane Impact Parameter Cuts (cm)= ',
     1 /,40X,5F6.1,/,40X,5F6.1,/,40X,5F6.1)
 2020 FORMAT(10X,' Min / Max t0 Offset from floating t0 fit (nsec) : ',
     &  /,35X,5F8.1,/,35X,5F8.1)
 2022 FORMAT(10X,' Min Energy for back-to-back mip veto (GeV) = ',5F4.1)
 2024 FORMAT(10X,' A-layer stubs excluded from primary event',
     1 ' selection ')
 2026 FORMAT(10X,' Global layer requirements :',/,
     1 15X,' A-layer (0=required,other=no cut) = ',5I3,/,
     2 15X,' B-layer (0=required,other=no cut) = ',5I3,/,
     3 15X,' C-layer (0=required,other=no cut) = ',5I3,/,
     4 15X,' reject missing A and B layer only tracks ')
 2028 FORMAT(10X,' Specific hit plane requirements (available hits) : ',
     &  /,
     1 15X,' CF  : Minimum A/B/C/Total layer hit count = ',4I3,/,
     2 15X,' EF  : Minimum A/B/C/Total layer hit count = ',4I3,/,
     3 15X,' WS  : Minimum A/B/C/Total layer hit count = ',4I3,/,
     4 15X,' WSS : Minimum A/B/C/Total layer hit count = ',4I3)
 2029 FORMAT(10X,' Specific hit plane requirements (used hits) : ',/,
     1 15X,' CF  : Minimum A/B/C/Total layer hit count = ',4I3,/,
     2 15X,' EF  : Minimum A/B/C/Total layer hit count = ',4I3,/,
     3 15X,' WS  : Minimum A/B/C/Total layer hit count = ',4I3,/,
     4 15X,' WSS : Minimum A/B/C/Total layer hit count = ',4I3)
 2030 FORMAT(10X,' WAMUS EF Level 2 Trigger cuts applied  ')
 2032 FORMAT(10X,' Specific hit plane requirements (available hits) : ',
     &  /,
     1 15X,' SAM : Minimum A/B/C/Total layer hit count = ',4I3)
 2033 FORMAT(10X,' Specific hit plane requirements (used hits) : ',/,
     1 15X,' SAM : Minimum A/B/C/Total layer hit count = ',4I3)
 2034 FORMAT(10X,' Max value of IFW4 (tight)           = ',5I3)
 2036 FORMAT(10X,' Max Bend View Residual (cm)     = ',5F5.1)
 2038 FORMAT(10X,' Max Non-bend View Residual (cm) = ',5F5.1)
 2040 FORMAT(10X,' Max value of IFW4 (loose)       = ',5I3)
 2042 FORMAT(10X,' Min/Max allowed no of matching ZTRAKS : ',/,
     1 30X,5I3,' / ',5I3)
 2044 FORMAT(10X,' Max allowed dTheta(muon-ZTRAK) (rad) = ',5F5.2,/,
     1 10X,' Max allowed dPhi(muon-ZTRAK) (rad)   = ',5F5.2)
 2048 FORMAT(10X,' Minimum mip energy(2NN) - with/without ZTRAK',
     1 ' match : ',/,20X,5F5.2,' / ',5F5.2)
 2050 FORMAT(10X,' Minimum mip energy(1NN) - with/without ZTRAK',
     1 ' match : ',/,20X,5F5.2,' / ',5F5.2)
 2052 FORMAT(10X,' Minimum Integral B.dl cut = ',F5.2,
     1 ' for |eta| > ',F5.2)
C
      END
