      SUBROUTINE TOP_DILEP_MUINFO(LPMUO,XVAR,KMUON)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get all muon realted information
C-                        used by CLEANMU to define a good muon
C-
C-   Inputs  : LPMUO : poniter to a good muon
C-    ?KMUON         NVAR_MUON : Total number of muon related variables
C-   Outputs : XVAR : output array of muon information
C-   Controls:
C-
C-   Created   9-OCT-1994   Meenakshi Narain
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INTEGER NVAR_MUON,NTAGS,KMUON,LMUON,LMTOF
C      INCLUDE 'D0$INC:TOP_DILEP_ANALYSIS.INC'
      PARAMETER( nvar_muon = 75 )
      CHARACTER*32 TAGS(512)
      REAL XVAR(nvar_muon),xdata(nvar_muon)
      REAL CONV
      REAL P, PX, PY, PZ, PT, THETA, ETA, PHI, QUAD
      REAL MUCTAG, XOCT, X3D_IMPACT, BEND_IMPACT, NON_BEND_IMPACT
      REAL FLOAT_T0, CHG, QIFW1, QIFW4, NZTRAK, DTHETA, DPHI
      REAL CALMIP_2NN, CALMIP_1NN, EN_CON04, EN_CON06, MTC_HFRAC
      REAL MTC_FRAC, GFITCSQ, BDL, ETA_BDL, ASTUB, MISS_LAYER
      REAL QUAL_BEND, QUAL_NONBEND, SCINT_TOF, SCINT_EXPTOF
      REAL MUL1, MUL15, CALISO_2NN, SCINT_ACTIVE, SCINT_CONF
      REAL MTC_ETRK, MTC_TRES, MTC_TRESV, MTC_GHFRAC, MTC_ECHI, MTC_EN3
      REAL MTC_EFRH1, MTC_LYRMU, MTC_ECHI2, ERRPTSQ
      INTEGER IFW2, IFW3, ITEMP, LPMUO, LMUOT, JBIT, I, NS, IERR
      INTEGER ICALL, WAM_HIT(6), SAM_HIT(3)
      INTEGER LJETS_DR, LJETS_DPHI
      REAL VERT_USED, VERT_NUMB_CAL
      REAL QIFW2, QIFW3, WAM_HIT1,WAM_HIT2,WAM_HIT3
      REAL DR_MIN, DPHI_MIN, PT_REL1, PT_REL2, P_FRAC
      REAL    DPHI_MAX, XXX, FARJET_ET
      REAL WAM_HITU1,WAM_HITU2,WAM_HITU3,SAM_HITU
      REAL EM_ECELL, EM_ECELL_1NN, EM_ECELL_2NN
      REAL EM_ECELL_4NN, EM_ECELL_6NN, DEDX_LOSS
      COMMON / MUINFO / P, PT, THETA, ETA, PHI, QUAD,
     &  MUCTAG, XOCT, X3D_IMPACT, BEND_IMPACT, NON_BEND_IMPACT,
     & FLOAT_T0, CHG, QIFW1, QIFW4, NZTRAK, DTHETA, DPHI,
     & CALMIP_2NN, CALMIP_1NN, EN_CON04, EN_CON06, MTC_HFRAC,
     & MTC_FRAC, GFITCSQ, BDL, ETA_BDL, ASTUB, MISS_LAYER,
     & QUAL_BEND, QUAL_NONBEND, SCINT_ACTIVE, MUL1, MUL15,
     & PX,PY,PZ, CALISO_2NN, SCINT_CONF, SCINT_TOF, SCINT_EXPTOF, QIFW2,
     & QIFW3, DR_MIN, MTC_ETRK, MTC_TRES, MTC_TRESV,MTC_GHFRAC,MTC_ECHI,
     & MTC_EN3, MTC_EFRH1, MTC_LYRMU, MTC_ECHI2, WAM_HIT1, WAM_HIT2,
     & WAM_HIT3, DPHI_MAX, FARJET_ET, DPHI_MIN, PT_REL1, PT_REL2,
     & P_FRAC, WAM_HITU1, WAM_HITU2, WAM_HITU3, EM_ECELL, EM_ECELL_1NN,
     & EM_ECELL_2NN, EM_ECELL_4NN, EM_ECELL_6NN, DEDX_LOSS, VERT_USED,
     & VERT_NUMB_CAL,SAM_HITU,ERRPTSQ
C-
      LOGICAL OPP_PHI
      EQUIVALENCE (XDATA,P)
      DATA CONV/ 57.29578/

C----------------------------------------------------------------------
      KMUON = NVAR_MUON
C
      P           = q(lpmuo+13) ! P
      PX          = q(lpmuo+10) ! PX
      PY          = q(lpmuo+11) ! PY
      PZ          = q(lpmuo+12) ! PZ
      PT          = q(lpmuo+14) ! Pt
      ERRPTSQ     = q(lpmuo+22) ! (sigPt)**2
      THETA       = q(lpmuo+15) ! theta
      ETA         = q(lpmuo+16) ! eta
      PHI         = q(lpmuo+17) ! phi
      QUAD        = IQ(lpmuo+7) ! quad 1-4 CF, 5-12 EF/EF+SAMUS
      CHG      = IQ(lpmuo+2) ! Sign of muon ! 7
C
      IF ((JBIT(IQ(LPMUO+44),7).GT.0).OR.
     &              (JBIT(IQ(LPMUO+44),8).GT.0)) THEN
        MUCTAG = 1
      ELSE
        MUCTAG = 0
      ENDIF
      XOCT        = JBIT(IQ(LPMUO+44),9).GT.0         ! 2
      X3D_IMPACT   = Q(LPMUO+41)                       ! 3
      BEND_IMPACT = Q(LPMUO+56)                       ! 4
      NON_BEND_IMPACT = Q(LPMUO+57)                   ! 5
      FLOAT_T0    = Q(LPMUO+24)                       ! 6
      QIFW4       = IQ(LPMUO+9)                            ! 13, 16
      NZTRAK      = IQ(LPMUO+6)                          ! 17,18
      IF(IQ(LPMUO+6).GT.0) THEN
        DTHETA    = Q(LPMUO+39)/CONV ! 18
        DPHI      = Q(LPMUO+38)/CONV ! 18
      ENDIF
      CALISO_2NN  = Q(LPMUO+30)
      DEDX_LOSS   = Q(LPMUO+33)
      CALMIP_2NN  = Q(LPMUO+34)    ! 20
      CALMIP_1NN  = Q(LPMUO+84)    ! 21
      EN_CON04    = Q(LPMUO+35)
      EN_CON06    = Q(LPMUO+36)
      MTC_HFRAC   = Q(LPMUO+94)    ! 23
      MTC_FRAC    = Q(LPMUO+93)    ! 23
      MTC_ETRK    = Q(LPMUO+90)
      MTC_TRES    = Q(LPMUO+91)
      MTC_TRESV   = Q(LPMUO+92)
      MTC_GHFRAC  = Q(LPMUO+95)
      MTC_ECHI    = Q(LPMUO+96)
      MTC_EN3     = Q(LPMUO+97)
      MTC_EFRH1   = Q(LPMUO+98)
      MTC_LYRMU   = Q(LPMUO+99)
      MTC_ECHI2   = Q(LPMUO+100)
      GFITCSQ     = Q(LPMUO+23)   ! 27
      SCINT_TOF   = Q(LPMUO+52)
      SCINT_EXPTOF = Q(LPMUO+53)
      VERT_USED   = FLOAT(IQ(LPMUO+54))
      VERT_NUMB_CAL = FLOAT(IQ(LPMUO+55))
      EM_ECELL = q(lpmuo+78)
      EM_ECELL_1NN = q(lpmuo+79)
      EM_ECELL_2NN = q(lpmuo+80)
      EM_ECELL_4NN = q(lpmuo+81)
      EM_ECELL_6NN = q(lpmuo+82)
      ICALL = 2
      CALL DECODE_MUON_PLANE_INFO(LPMUO,ICALL,WAM_HIT,SAM_HIT)
      WAM_HITU1 = WAM_HIT(1)
      WAM_HITU2 = WAM_HIT(2)
      WAM_HITU3 = WAM_HIT(3)
      SAM_HITU  = SAM_HIT(1)+SAM_HIT(2)+SAM_HIT(3)
      ICALL = 1
      CALL DECODE_MUON_PLANE_INFO(LPMUO,ICALL,WAM_HIT,SAM_HIT)
      WAM_HIT1 = WAM_HIT(1)
      WAM_HIT2 = WAM_HIT(2)
      WAM_HIT3 = WAM_HIT(3)
      OPP_PHI = .FALSE.
      CALL TOP_DILEP_UTIL_NEARJET(LPMUO,OPP_PHI,DR_MIN,LJETS_DR,
     &  DPHI_MIN,LJETS_DPHI)
      IF(LJETS_DR.GT.0) THEN
        CALL TOP_DILEP_UTIL_MUJET_REL(LPMUO,LJETS_DR,PT_REL1,
     &    PT_REL2,P_FRAC,IERR)
      ENDIF
      OPP_PHI = .TRUE.
      CALL TOP_DILEP_UTIL_NEARJET(LPMUO,OPP_PHI,XXX,LJETS_DR,
     &  DPHI_MAX,LJETS_DPHI)
      FARJET_ET=Q(LJETS_DPHI+6)
C
      LMUOT = 0
      NS = IQ(LPMUO-2)
      IF ( NS.GT.0 ) LMUOT = LQ(LPMUO-NS-1)
      IF(LMUOT.GT.0) THEN
        QIFW1   = IQ(LMUOT+4)
        BDL     = Q(LMUOT+22)
        ETA_BDL = Q(LPMUO+16)             ! 22
        ITEMP   =IQ(LMUOT+4)
        IF(ITEMP.GE.10) ITEMP=ITEMP-10
        IF(ITEMP.EQ.5) THEN
          ASTUB = 1                   ! 8
        ENDIF
        MISS_LAYER  = ITEMP
        QUAL_BEND   = Q(LMUOT+20)       ! 14
        QUAL_NONBEND = Q(LMUOT+21)   ! 15
        IFW2 = IQ(LMUOT+5)
        QIFW2 = IFW2
        IF (BTEST(IFW2,16)) THEN
          SCINT_ACTIVE = 1
        ELSE
          SCINT_ACTIVE = 0
        ENDIF
        IF (BTEST(IFW2,17)) THEN
          SCINT_CONF = 1
        ELSE
          SCINT_CONF = 0
        ENDIF
        IFW3 = IQ(LMUOT+6)
        QIFW3 = IFW3
        IF (BTEST(IFW3,16)) THEN   ! 24
          MUL1 = 1
        ELSE
          MUL1 = 0
        ENDIF
        IF (BTEST(IFW3,17)) THEN   ! 25
          MUL15 = 1
        ELSE
          MUL15 = 0
        ENDIF
      ENDIF
      do i=1,nvar_muon
        xvar(i)= xdata(i)
      enddo
  999 RETURN

C...........................................................................
      ENTRY MUINFO_TAGS(NTAGS,TAGS)
      ntags = nvar_muon
C
      TAGS(1)='P'
      TAGS(2)='PT'
      TAGS(3)='THETA'
      TAGS(4)='ETA'
      TAGS(5)='PHI'
      TAGS(6)='QUAD'
      TAGS(7) ='MUCTAG'
      TAGS(8) ='XOCT'
      TAGS(9) ='X3D_IMPACT'
      TAGS(10)='BEND_IMPACT'
      TAGS(11)='NON_BEND_IMPACT'
      TAGS(12)='FLOAT_T0'
      TAGS(13)='CHG'
      TAGS(14)='QIFW1'
      TAGS(15)='QIFW4'
      TAGS(16)='NZTRAK'
      TAGS(17)='DTHETA'
      TAGS(18)='DPHI'
      TAGS(19)='CALMIP_2NN'
      TAGS(20)='CALMIP_1NN'
      TAGS(21)='EN_CON04'
      TAGS(22)='EN_CON06'
      TAGS(23)='MTC_HFRAC'
      TAGS(24)='MTC_FRAC'
      TAGS(25)='GFITCSQ'
      TAGS(26)='BDL'
      TAGS(27)='ETA_BDL'
      TAGS(28)='ASTUB'
      TAGS(29)='MISS_LAYER'
      TAGS(30)='QUAL_BEND'
      TAGS(31)='QUAL_NONBEND'
      TAGS(32)='SCINT_ACTIVE'
      TAGS(33)='MUL1'
      TAGS(34)='MUL15'
      TAGS(35)='PX'
      TAGS(36)='PY'
      TAGS(37)='PZ'
      TAGS(38)='CALISO_2NN'
      TAGS(39)='SCINT_CONF'
      TAGS(40)='SCINT_TOF'
      TAGS(41)='SCINT_EXPTOF'
      TAGS(42)='QIFW2'
      TAGS(43)='QIFW3'
      TAGS(44)='DR_MIN'
      TAGS(45)='MTC_ETRK'
      TAGS(46)='MTC_TRES'
      TAGS(47)='MTC_TRESV'
      TAGS(48)='MTC_GHFRAC'
      TAGS(49)='MTC_ECHI'
      TAGS(50)='MTC_EN3'
      TAGS(51)='MTC_EFRH1'
      TAGS(52)='MTC_LYRMU'
      TAGS(53)='MTC_ECHI2'
      TAGS(54)='WAM_HIT1'
      TAGS(55)='WAM_HIT2'
      TAGS(56)='WAM_HIT3'
      TAGS(57)='DPHI_MAX'
      TAGS(58)='FARJET_ET'
      TAGS(59)='DPHI_MIN'
      TAGS(60)='PT_REL1'
      TAGS(61)='PT_REL2'
      TAGS(62)='P_FRAC'
      TAGS(63)='HITLYRU1'
      TAGS(64)='HITLYRU2'
      TAGS(65)='HITLYRU3'
      TAGS(66)='EM_ECELL'
      TAGS(67)='EM_ECELL_1NN'
      TAGS(68)='EM_ECELL_2NN'
      TAGS(69)='EM_ECELL_4NN'
      TAGS(70)='EM_ECELL_6NN'
      TAGS(71)='DEDXLOSS'
      TAGS(72)='VERTUSED'
      TAGS(73)='VERTCAL'
      TAGS(74)='SAM_HITU'
      TAGS(75)='ERRPTSQ'
C***  more in top_dilep_analysis.for
      RETURN
      END
