      SUBROUTINE ANLZMUMU
C----------------------------------------------------------------------
C-   Purpose and Methods : Analyzes events, looking for Z -> mu mu
C-
C-   Inputs  : PNUT and PMUO banks
C-   Outputs : NTUPLES and HISTOGRAMS
C-   Controls: RCP file
C-
C-   Created  22-SEP-1992   Cecilia E. Gerber
C-   Updated  24-MAY-1993   use new PMUO format
C-   Updated  25-NOV-1993   use new CLEANMU
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISAE.LINK'
C
      INTEGER MODMU,NTDIM
      INTEGER NTUPLE_ID_Z, NGOOD_PMUO, LENGTH
      INTEGER IER,  NSTRIP_Z, ISAIDMU1, ISAIDMU2
      INTEGER IERR,USER_ID, PMUO_VERS, STATUS
      INTEGER LSUP,LISAL,IDLM,IPMUO,NPMUO,LPMUO
      INTEGER IMU,IOFF,ILEP,GZPMUO,NMU,GZISAE,LISAE,NISAL
      INTEGER IDMAX(2),WAM_POL, SAM_POL,KTEMP1, KTEMP2
      INTEGER GZPARH,LPARH,GZPNUT,LPNUT,LJETS_DR, LJETS_DP
      INTEGER L1BITS,IRUN,IEVT, NRUN, NEVNT,IFW4CUT
      INTEGER LMUOT, NS,L2_WORD1, L2_WORD2,JTEMP1, JTEMP2
      INTEGER IFW1MU1, IFW1MU2, IFW2MU1, IFW2MU2, ITEMP1, ITEMP2
C
      PARAMETER(MODMU=70)
      PARAMETER(NTDIM=210)
C
      REAL PM(68),PHIM,THM,ETAM,IUNOUT,SSUNIT
      REAL PMAX(68,2),ETMAX(2),P3MU(3)
      REAL ISAPTMU1,ISAETAMU1,ISAPHIMU1,ISAPTMU2,ISAETAMU2,ISAPHIMU2
      REAL P3MU1(3),P3MU2(3),P3TOT(3),AKINZ(50)
      REAL ETM,COTTH, CHISQ_GFIT
      REAL PNT(NTDIM),ETA,PHI,TEMPLATE(3)
      REAL PTMIN1, PTMIN2, MISS_ET, ETACUT, PTCUT, MASS_HI, MASS_LO
      REAL ISOMAX, ISOMIN, IMPCXY, XY_IMPACT, COS_TRAN
      REAL IMPCRZ,MIN_ENERGY_02,MIN_INTEG_BDL
      REAL PT1(2), PT2(2), MODP1, MODP2, DOT, DPHI, DTHE, THE1, THE2
      REAL DELTA_PHI, DELTA_THE, MODPT1, MODPT2
      REAL VMOD, VDOT, DPHICUT
      REAL ET_SAME,ET_OPP,DR_MIN,DPHI_MIN,ET_JET,ET_BACK, DPHI_BACK
      REAL NUMJETS, MUJET_PTREL, MUJET_ENRAT
C
      LOGICAL WRITE_Z_EVENT, FILL_NT_ALL, FILL_NT_CUT, FILL_NT_ISA
      LOGICAL DO_Z_CUTS, PASSED, NEWFILE, FIRST
      LOGICAL DO_MU_SIGN, DO_LOOSE_Z_CUT, DO_TIGHT_Z_CUT, OK
      LOGICAL REJECT_Z_EVENT, NO_OPP_TRACK
      LOGICAL CHECK_MAGNET, DO_GFIT_CUTS, OCTANT_CUT, DO_DPHI_CUT
C
      CHARACTER*6 CHTOP_Z
      CHARACTER*64 TITLE
      CHARACTER*60 CHTAGS(NTDIM)
      CHARACTER*20 FILENAME
      CHARACTER*4 PATHOLD
C
      EXTERNAL GZPMUO,GZISAE,SSUNIT,GZPARH
      EXTERNAL UCOPY, VMOD, VDOT, VZERO
      COMMON /ZMUMU/ NTUPLE_ID_Z, NSTRIP_Z
C
      DATA FIRST /.TRUE./
      DATA CHTAGS/
     &'IDMU1','PXMU1','PYMU1','PZMU1','PTMU1','IFW4MU1','THEMU1',
     &'PHIMU1','ETAMU1','ISO1MU1','ISO2MU1','ISO02MU1','ISO04MU1',
     &'ISO06MU1','CAEMU1','CAE02MU1','CAE04MU1','CAE06MU1','NUMCD1',
     &'ANGLE1','DPHI1','DTHETA1','IMPC1MU1','IPMUOMU1','IMPC2MU1',
     &'GOODMU1','IFW2MU1','IFW1MU1','GFITMU1','CHISQMU1','TFLOAT1',
     &'BDLMU1','RESBMU1','RESNBMU1','QUADMU1','CDCONE1','ELOSSF1',
     &'HITSTMU1','HITSGMU1','VERT1MU1','VERT2MU1','IMPCB1', 'IMPCNB1',
     &'IMPCBCD1','IMPCNCD1','ENE0MU1','ENE1MU1','ENE2MU1','ENE4MU1',
     &'ENE6MU1','ENET0MU1','ENET1MU1','ENEB0MU1','ENEB1MU1','ENEB2MU1',
     &'ENEB3MU1','L1W1MU1','L1W2MU1','L15W1MU1','L15W2MU1','ETSAME1',
     &'ETOPP1','DRJET1','ETJET1','ETBACK1','DPHIB1','IMPCXY1',
     &'SPARE1M1','PTREL1','ENRAT1',
C
     &'IDMU2','PXMU2','PYMU2','PZMU2','PTMU2','IFW4MU2','THEMU2',
     &'PHIMU2','ETAMU2','ISO1MU2','ISO2MU2','ISO02MU2','ISO04MU2',
     &'ISO06MU2','CAEMU2','CAE02MU2','CAE04MU2','CAE06MU2','NUMCD2',
     &'ANGLE2','DPHI2','DTHETA2','IMPC1MU2','IPMUOMU2','IMPC2MU2',
     &'GOODMU2','IFW2MU2','IFW1MU2','GFITMU2','CHISQMU2','TFLOAT2',
     &'BDLMU2','RESBMU2','RESNBMU2','QUADMU2','CDCONE2','ELOSSF2',
     &'HITSTMU2','HITSGMU2','VERT1MU2','VERT2MU2','IMPCB2', 'IMPCNB2',
     &'IMPCBCD2','IMPCNCD2','ENE0MU2','ENE1MU2','ENE2MU2','ENE4MU2',
     &'ENE6MU2','ENET0MU2','ENET1MU2','ENEB0MU2','ENEB1MU2','ENEB2MU2',
     &'ENEB3MU2','L1W1MU2','L1W2MU2','L151WMU2','L15W2MU2','ETSAME2',
     &'ETOPP2','DRJET2','ETJET2','ETBACK2','DPHIB2','IMPCXY2',
     &'SPARE1M2','PTREL2','ENRAT2',
C
     &'NPMUO ','PMISSX','PMISSY','PMISSZ','EMISST','PTMISS','THENU',
     &'ETANU','PHINU','SCALET','L1WORD','IRUN','IEVNT','MIZ','PTZ',
     &'ZRAP','COSTHE','NGOODMU','DPHI','DTHETA','L2WORD1','L2WORD2',
     &'MAGPOL','ISAID1','ISAPT1','ISAETA1','ISAPHI1','ISAID2','ISAPT2',
     &'ISAETA2','ISAPHI2','NUMJETS','SPARE02','SPARE03','SPARE04',
     &'SPARE05','SPARE06','SPARE07','SPARE08','SPARE09',
C
     &'SPARE1','SPARE2','SPARE3','SPARE4','SPARE5','SPARE6','SPARE7',
     &'SPARE8','SPARE9','SPARE10','SPARE11','SPARE12','SPARE13',
     &'SPARE14','SPARE15','SPARE16','SPARE17','SPARE18','SPARE19',
     &'SPARE20','SPARE21','SPARE22','SPARE23','SPARE24','SPARE25',
     &'SPARE26','SPARE27','SPARE28','SPARE29','SPARE30'/
C----------------------------------------------------------------------
      IF (FIRST) THEN
C
C   Get parameters from WZ.RCP
        NSTRIP_Z = 0
        CALL EZPICK('WZ_RCP')
        CALL EZGET('DO_Z_CUTS', DO_Z_CUTS, IER)
        CALL EZGET('WRITE_Z_EVENT', WRITE_Z_EVENT, IER)
        CALL EZGET('FILL_NT_ALL', FILL_NT_ALL, IER)
        CALL EZGET('FILL_NT_CUT', FILL_NT_CUT, IER)
        CALL EZGET('FILL_NT_ISA', FILL_NT_ISA, IER)
        CALL EZGET('ETACUT', ETACUT, IER)
        CALL EZGET('PTCUT', PTCUT, IER)
        CALL EZGET('Z_PTMIN1', PTMIN1, IER)
        CALL EZGET('Z_PTMIN2', PTMIN2, IER)
        CALL EZGET('Z_MASS_LO',MASS_LO , IER)
        CALL EZGET('Z_MASS_HI',MASS_HI , IER)
        CALL EZGET('DO_MU_SIGN', DO_MU_SIGN, IER)
        CALL EZGET('Z_ISOMAX',ISOMAX , IER)
        CALL EZGET('Z_ISOMIN',ISOMIN , IER)
        CALL EZGET('Z_IMPCXY',IMPCXY , IER)
        CALL EZGET('DO_TIGHT_Z_CUT', DO_TIGHT_Z_CUT, IER)
        CALL EZGET('DO_LOOSE_Z_CUT', DO_LOOSE_Z_CUT, IER)
        CALL EZGET('OCTANT_CUT', OCTANT_CUT, IER)
        CALL EZGET('IFW4CUT', IFW4CUT, IER)
        CALL EZGET('NO_OPP_TRACK', NO_OPP_TRACK, IER)
        CALL EZGET('DELTA_PHI', DELTA_PHI, IER)
        CALL EZGET('DELTA_THE', DELTA_THE, IER)
        CALL EZGET('CHECK_MAGNET', CHECK_MAGNET, IER)
        CALL EZGET('Z_DO_GFIT_CUTS',DO_GFIT_CUTS , IER)
        CALL EZGET('Z_CHISQ_GFIT',CHISQ_GFIT , IER)
        CALL EZGET('DO_DPHI_CUT',DO_DPHI_CUT, IER)
        CALL EZGET('DPHICUT', DPHICUT, IER)
        CALL EZGET('REJECT_Z_EVENT', REJECT_Z_EVENT, IER)
        CALL EZGETS('Z_CHTOP',1,CHTOP_Z,LENGTH,IER)
        CALL EZRSET
C
C  user initialization hook
C
        CALL USRZINI(CHTAGS)
C
C first, do the special stuff required for a disk-resident ntuple
C (see D0$DOCS:NTUPLE.DOC)
        IF ((FILL_NT_ALL).OR.(FILL_NT_CUT).OR.(FILL_NT_ISA)) THEN
          USER_ID = 111
          NEWFILE = .TRUE.
          FILENAME = 'ZMUMU_N.HST4'
          CALL NTUPLE_FILE_OPEN(USER_ID,NEWFILE,FILENAME,
     &      1024,CHTOP_Z,IERR)
          IF(IERR.NE.0) THEN
            GOTO 999
          ENDIF
          CALL NTUPLE_BOOK(CHTOP_Z,NTDIM,       !Book Ntuple
     &      CHTAGS,'ZMUMU',NTUPLE_ID_Z,IERR)
          IF(IERR.NE.0) THEN
            GOTO 999
          ENDIF
        ENDIF
C
C now book the ordinary histograms
        CALL HCDIR ('//PAWC',' ')       ! GO TO TOP DIRECTORY
        CALL HMDIR ('ZMUMU','S')       ! CREATE ZMUMU DIRECTORY
        CALL HCDIR ('//PAWC',' ')       ! BACK TO TOP DIRECTORY
C
        FIRST = .FALSE.
      ENDIF
C----------------------------------------------------------------------
      CALL FLGSET('WRITE_STREAM_ZMU', .FALSE.)
      PASSED = .TRUE.
      ITEMP1 = 0
      ITEMP2 = 0
      JTEMP1 = 0
      JTEMP2 = 0
      KTEMP1 = 0
      KTEMP2 = 0
      IFW1MU1 = 0
      IFW1MU2 = 0
      IFW2MU1 = 0
      IFW2MU2 = 0
      NGOOD_PMUO = 0
      CALL VZERO(PNT,NTDIM)
      CALL HCDIR('//PAWC',' ') !Leave in TOP HBOOK DIRECTORY
C
C set muctag track bit for EF tracks
      CALL MUCTAG_TRK
C
C get unit number for output file
C
      IUNOUT = SSUNIT()
C
      CALL HCDIR('//PAWC/ZMUMU',' ')  ! go to ZMUMU histogram directory
C
C
C check if it's Monte Carlo (isajet)
      IF(LHEAD.GT.0) THEN
        LISAE = LQ(LHEAD-IZISAE)
        IF(LISAE.GT.0) THEN
          ISAIDMU1 = 0
          ISAPTMU1 = 0.
          ISAETAMU1 = 0.
          ISAPHIMU1 = 0.
          ISAIDMU2 = 0
          ISAPTMU2 = 0.
          ISAETAMU2 = 0.
          ISAPHIMU2 = 0.
          CALL ZMU_ISAJ(ISAPTMU1,ISAPTMU2,ISAETAMU1,ISAETAMU2,ISAPHIMU1,
     &      ISAPHIMU2,ISAIDMU1,ISAIDMU2)
          PNT(2*MODMU+24) = ISAIDMU1
          PNT(2*MODMU+25) = ISAPTMU1
          PNT(2*MODMU+26) = ISAETAMU1
          PNT(2*MODMU+27) = ISAPHIMU1
          PNT(2*MODMU+28) = ISAIDMU2
          PNT(2*MODMU+29) = ISAPTMU2
          PNT(2*MODMU+30) = ISAETAMU2
          PNT(2*MODMU+31) = ISAPHIMU2
        ENDIF
      ENDIF
C
C check pmuo bank
C
      CALL PATHGT(PATHOLD)
      CALL PATHST('RECO')
      LPARH=GZPARH()
      NPMUO = 0
      IF (LPARH.GT.0) THEN
        NPMUO=IQ(LPARH+2)
        PNT(2*MODMU+1) = FLOAT(NPMUO)
      ENDIF
C
C find two hightest Et muons in PMUO
C
      IF(NPMUO.GT.0) THEN
        CALL VZERO(ETMAX,2)
        CALL VZERO(PMAX,136)
        CALL VZERO(IDMAX,2)
        CALL VZERO(PM,68)
        DO IPMUO=1,NPMUO
          LPMUO = GZPMUO(IPMUO)
          IF(LPMUO.GT.0) THEN
C
C get PMUO information
C
C PMUO version
            PMUO_VERS=IQ(LPMUO+1)
C muon sign
            IDLM = IQ(LPMUO+2)
C 3-momentum
            CALL UCOPY(Q(LPMUO+10),PM,3)
C quality flag IFW4
            PM(4) = IQ(LPMUO+9)
C muon angles
            PM(5) = Q(LPMUO+15)
            PM(6) = Q(LPMUO+17)
            PM(7) = Q(LPMUO+16)
C muon isolation parameters
            PM(8)  = Q(LPMUO+28)
            PM(9)  = Q(LPMUO+29)
            PM(10) = Q(LPMUO+30)
            PM(11) = Q(LPMUO+31)
            PM(12) = Q(LPMUO+32)
C muon energy loss in calorimeter
            PM(13) = Q(LPMUO+33)
            PM(14) = Q(LPMUO+34)
            PM(15) = Q(LPMUO+35)
            PM(16) = Q(LPMUO+36)
            IF(PMUO_VERS.GE.3) THEN
              PM(49) = Q(LPMUO+83)
              PM(50) = Q(LPMUO+84)
            ENDIF
C muon and CD track match
            PM(17) = IQ(LPMUO+6)
            PM(18) = Q(LPMUO+37)
            PM(19) = Q(LPMUO+38)
            PM(20) = Q(LPMUO+39)
C impact parameter
            PM(21) = Q(LPMUO+41)
            PM(23) = Q(LPMUO+42)
            IF(PMUO_VERS.GE.3) THEN
              PM(40) = Q(LPMUO+56)
              PM(41) = Q(LPMUO+57)
              PM(42) = Q(LPMUO+58)
              PM(43) = Q(LPMUO+59)
            ENDIF
C value of IPMUO
            PM(22) = IPMUO
C quality flag IFW2
            PM(25) = IQ(LPMUO+44)
C global fit parameters
            PM(27) = IQ(LPMUO+4)
            PM(28) = Q(LPMUO+23)
C floating t0
            PM(29) = Q(LPMUO+24)
C quadrant
            PM(33) = IQ(LPMUO+7)
C CD search cone
            PM(34) = Q(LPMUO+40)
C muon energy loss in the toroid
            PM(35) = Q(LPMUO+43)
C hits on track
            IF(PMUO_VERS.GE.3) THEN
              PM(36) = IQ(LPMUO+46)
              PM(37) = IQ(LPMUO+47)
C vertex info
              PM(38) = IQ(LPMUO+54)
              PM(39) = IQ(LPMUO+55)
C EM energy from muon
              PM(44) = Q(LPMUO+78)
              PM(45) = Q(LPMUO+79)
              PM(46) = Q(LPMUO+80)
              PM(47) = Q(LPMUO+81)
              PM(48) = Q(LPMUO+82)
C Cal energy opposite to the muon
              PM(51) = Q(LPMUO+86)
              PM(52) = Q(LPMUO+87)
              PM(53) = Q(LPMUO+88)
              PM(54) = Q(LPMUO+89)
C level 1 and level 1.5 trigger info
              PM(55) = IQ(LPMUO+48)
              PM(56) = IQ(LPMUO+49)
              PM(57) = IQ(LPMUO+50)
              PM(58) = IQ(LPMUO+51)
            ENDIF
C MUOT info
            NS = IQ(LPMUO-2)
            LMUOT = LQ(LPMUO-NS-1)
            IF(LMUOT.GT.0) THEN
              PM(26) = IQ(LMUOT+4)
              PM(30) = Q(LMUOT+22)
              PM(31) = Q(LMUOT+20)
              PM(32) = Q(LMUOT+21)
              XY_IMPACT = 0.
              COS_TRAN = SQRT(Q(LMUOT+17)**2+Q(LMUOT+18)**2)
              IF(COS_TRAN.GT.0.) THEN
                XY_IMPACT = (Q(LMUOT+11)*Q(LMUOT+18) -
     &            Q(LMUOT+12)*Q(LMUOT+17))/COS_TRAN
                PM(65) = XY_IMPACT
              ENDIF
            ENDIF
C jet info
            CALL UCOPY(Q(LPMUO+10),P3MU,3)
            CALL JETOPP(P3MU,ET_SAME,ET_OPP,ET_BACK,DPHI_BACK,DR_MIN,
     &        ET_JET,NUMJETS,MUJET_PTREL,MUJET_ENRAT)
            PNT(2*MODMU+32) = NUMJETS
            IF(NUMJETS.GT.0) THEN
              PM(59) = ET_SAME
              PM(60) = ET_OPP
              PM(61) = DR_MIN
              PM(62) = ET_JET
              PM(63) = ET_BACK
              PM(64) = DPHI_BACK
              PM(67) = MUJET_PTREL
              PM(68) = MUJET_ENRAT
            ELSE
              PM(59) = -1.
              PM(60) = -1.
              PM(61) = -1.
              PM(62) = -1.
              PM(63) = -1.
              PM(64) = -1.
              PM(67) = -1.
              PM(68) = -1.
            ENDIF
C
C muon Pt
            ETM = SQRT(PM(1)**2+PM(2)**2)
C
C calculate the pseudorapidity of the muon
C
            IF(ETM.GT.0.) THEN
              COTTH = PM(3)/ETM
              ETAM = LOG(COTTH+SQRT(COTTH**2+1))
            ELSE
              ETAM = 5.
            ENDIF
C
C  use ETACUT and PTCUT as muon quality selection.
C
            IF ((ABS(ETAM).LT.ETACUT).AND.(ETM.GT.PTCUT)) THEN
C
C call Offline Muon id and Cosmic Ray Rejection Routine. 
C Selection cuts in CLEANMU.RCP
C
              CALL CLEANMU(LPMUO,STATUS,OK)
              IF(OK) PM(24) = 1.0
              IF(.NOT.OK) PM(24) = 2.0
              NGOOD_PMUO = NGOOD_PMUO + 1
              IF(ETM.GT.ETMAX(1)) THEN
                ETMAX(2) = ETMAX(1)
                IDMAX(2) = IDMAX(1)
                CALL UCOPY(PMAX(1,1),PMAX(1,2),68)
                ETMAX(1) = ETM
                IDMAX(1) = IDLM
                CALL UCOPY(PM,PMAX(1,1),68)
              ELSEIF(ETM.GT.ETMAX(2)) THEN
                ETMAX(2) = ETM
                IDMAX(2) = IDLM
                CALL UCOPY(PM,PMAX(1,2),68)
              ENDIF
            ENDIF
          ENDIF
        ENDDO
C
C store number of good muons in ntuple
        PNT(2*MODMU+18) = NGOOD_PMUO
C
C store two highest Pt good muons in ntuple
C
        DO IMU=1,2
          IOFF = MODMU*(IMU-1)
          PNT(IOFF+1) = FLOAT(IDMAX(IMU))
          CALL UCOPY(PMAX(1,IMU),PNT(IOFF+2),3)
          PNT(IOFF+5) = ETMAX(IMU)
          CALL UCOPY(PMAX(4,IMU),PNT(IOFF+6),65)
        ENDDO
C
C        missing ET
C
        LPNUT=GZPNUT(3)     ! pick missing ET bank with MUON correction
        IF(LPNUT.GT.0) THEN
          CALL UCOPY(Q(LPNUT+3),PNT(2*MODMU+2),8)
          PNT(2*MODMU+10) = Q(LPNUT+14)
        ENDIF
      ENDIF
C
C event header info
C
      IF(LHEAD.GT.0) THEN
        L1BITS = IQ(LHEAD+11)
        IRUN = IQ(LHEAD+6)
        IEVT = IQ(LHEAD+9)
        L2_WORD1 = IQ(LHEAD+15)
        L2_WORD2 = IQ(LHEAD+16)
        PNT(2*MODMU+11) = FLOAT(L1BITS)
        PNT(2*MODMU+12) = FLOAT(IRUN)
        PNT(2*MODMU+13) = FLOAT(IEVT)
        PNT(2*MODMU+21) = FLOAT(L2_WORD1)
        PNT(2*MODMU+22) = FLOAT(L2_WORD2)
      ENDIF
C magnet polarity
      IF(CHECK_MAGNET) THEN
        CALL MAG_POLARITY(WAM_POL,SAM_POL,IERR)
        PNT(2*MODMU+23) = WAM_POL
        IF (WAM_POL.EQ.0.) PASSED = .FALSE.
      ENDIF
C
C Z analysis
C
      IF(NGOOD_PMUO.GE.1) THEN
        CALL UCOPY(PMAX(1,1),P3MU1,3)
        LPNUT=GZPNUT(2)     ! pick missing ET bank without MUON correction
        IF(LPNUT.GT.0) THEN
          CALL UCOPY(Q(LPNUT+3),P3TOT,3)
        ELSE
          CALL VZERO(P3TOT,3)
        ENDIF
C subract off calor missing pt to get total pt
        CALL VSUB(P3MU1,P3TOT,P3TOT,3)
C add in second muon if present
        IF(NGOOD_PMUO.GE.2) THEN
          CALL UCOPY(PMAX(1,2),P3MU2,3)
          CALL VADD(P3TOT,P3MU2,P3TOT,3)
        ENDIF
C calculate Z kinematics
        IF(NGOOD_PMUO.GE.2) THEN
          CALL ZKINEM(P3MU1,P3MU2,P3TOT,AKINZ)
          PNT(2*MODMU+14) = AKINZ(1)      ! transverse mass
          PNT(2*MODMU+15) = AKINZ(4)      ! ptZ
          PNT(2*MODMU+16) = AKINZ(21)     ! Z rapidity
          PNT(2*MODMU+17) = AKINZ(26)     ! cosine theta*
C calculate opening angle between the two muons
          CALL UCOPY (P3MU1,PT1,2)
          CALL UCOPY (P3MU2,PT2,2)
          MODP1 = VMOD(P3MU1,3)
          MODP2 = VMOD(P3MU2,3)
          MODPT1 = VMOD(PT1,2)
          MODPT2 = VMOD(PT2,2)
          DOT = VDOT(PT1,PT2,2)/MODPT1/MODPT2
          IF (DOT.GT.1.) DOT = 1.0
          IF (DOT.LT.-1.) DOT = -1.0
          DPHI = ACOSD(DOT)
          THE1 = ACOSD(P3MU1(3)/MODP1)
          THE2 = ACOSD(P3MU2(3)/MODP2)
          IF (DPHI.LT.90.) THEN
            DTHE = ABS(THE1-THE2)
          ELSE
            DTHE = THE1 + THE2
            IF (DTHE.GT.180.) DTHE = 360. - DTHE
          ENDIF
          PNT(2*MODMU+19) = DPHI
          PNT(2*MODMU+20) = DTHE
        ENDIF
      ENDIF
C
      CALL HCDIR('//PAWC',' ')  ! Leave in TOP HBOOK DIRECTORY
C
C  user hook for filling ntuples
C
      CALL USRZNTUPLE(PNT)
C
C fill ntuple for all the events with an isajet bank
C
      IF (FILL_NT_ISA) THEN
        CALL NTUPLE_FILL(CHTOP_Z,NTUPLE_ID_Z,PNT,IERR)
        IF(IERR.NE.0) THEN
          GOTO 999
        ENDIF
      ENDIF
C
C fill ntuple for all the events that have at least one good muon
C
      IF (FILL_NT_ALL) THEN
        IF (NGOOD_PMUO.GE.1) THEN
          CALL NTUPLE_FILL(CHTOP_Z,NTUPLE_ID_Z,PNT,IERR)
          IF(IERR.NE.0) THEN
            GOTO 999
          ENDIF
        ENDIF
      ENDIF
C
C see if event passed selection
C
      IF (DO_Z_CUTS) THEN
        IF(NGOOD_PMUO.GE.2) THEN
C cut on minimum PT of first muon
          IF (PNT(5).LT.PTMIN1) PASSED = .FALSE.
C cut on minimum PT of second muon
          IF (PNT(75).LT.PTMIN2) PASSED = .FALSE.
CCC both muon requirements
C ask that both muons do not cross octants
          IF (OCTANT_CUT) THEN
            IFW2MU1 = INT(PNT(27))
            IFW2MU2 = INT(PNT(97))
            IF (BTEST(IFW2MU1,8)) ITEMP1 = 1
            IF (BTEST(IFW2MU2,8)) ITEMP2 = 1
            IF ((ITEMP1.EQ.1).OR.(ITEMP2.EQ.1)) PASSED = .FALSE.
          ENDIF
C ask that both muons have IFW4 = 0 or 1
          IF ((PNT(6).GT.IFW4CUT).OR.(PNT(76).GT.IFW4CUT))
     &          PASSED = .FALSE.
C ask that both muons have |eta| < ETACUT
          IF ((ABS(PNT(9)).GT.ETACUT).OR.(ABS(PNT(79)).GT.ETACUT))
     &        PASSED = .FALSE.
C do COSMIC rejection: no opp track in Dphi<DELTA_PHI, Dtheta<DELTA_THE
          IF (NO_OPP_TRACK) THEN
            IF (DPHI.GT.DELTA_PHI.AND.DTHE.GT.DELTA_THE)
     &          PASSED = .FALSE.
          ENDIF
C set flag for GOOD_MUON, isolation and IMPCXY for each muon
          IF (PNT(26).EQ.2.0.OR.PNT(12).GT.ISOMAX.OR.PNT(12).LT.
     &      ISOMIN.OR.ABS(PNT(67)).GT.IMPCXY) JTEMP1 = 1
          IF (PNT(96).EQ.2.0.OR.PNT(82).GT.ISOMAX.OR.PNT(82).LT.
     &      ISOMIN.OR.ABS(PNT(137)).GT.IMPCXY) JTEMP2 = 1
CCC
CCCCC LOOSE Z SELECTION
C ask that at least one muon is isolated and GOOD_MUON as defined by CLEANMU
CCC
          IF (DO_LOOSE_Z_CUT) THEN
            IF (JTEMP1.EQ.1.AND.JTEMP2.EQ.1) PASSED = .FALSE.
          ENDIF
CCC
CCCCC TIGHT Z SELECTION
C ask that both muons are isolated and GOOD_MUON  as defined by CLEANMU
CCC
          IF (DO_TIGHT_Z_CUT) THEN
            IF (JTEMP1.EQ.1.OR.JTEMP2.EQ.1) PASSED = .FALSE.
          ENDIF
C
C cut on global fitting parameters
          IF (DO_GFIT_CUTS) THEN
            IF (PNT(29).EQ.0.OR.PNT(29).EQ.2.OR.PNT(30).GT.CHISQ_GFIT)
     &          KTEMP1 = 1
            IF (PNT(99).EQ.0.OR.PNT(99).EQ.2.OR.PNT(100).GT.CHISQ_GFIT)
     &          KTEMP2 = 1
            IF (KTEMP1.EQ.1.AND.KTEMP2.EQ.1) PASSED = .FALSE.
          ENDIF
C cut on minimum opening angle between the two muons
          IF (DO_DPHI_CUT) THEN
            IF (PNT(2*MODMU+19).LT.DPHICUT) PASSED = .FALSE.
          ENDIF
C cut on Z invariant mass
          IF(AKINZ(1).LT.MASS_LO.OR.AKINZ(1).GT.MASS_HI) PASSED=.FALSE.
C  ask that the two muons have different charge
          IF (DO_MU_SIGN) THEN
            IF (PNT(1).EQ.PNT(71)) PASSED = .FALSE.
          ENDIF
C
C fill ntuple only for the events that passed the cuts
C
          IF (FILL_NT_CUT) THEN
            IF (PASSED) THEN
              CALL NTUPLE_FILL(CHTOP_Z,NTUPLE_ID_Z,PNT,IERR)
              IF(IERR.NE.0) THEN
                GOTO 999
              ENDIF
            ENDIF
          ENDIF
C
C write out the event that passed the cuts. Stream ZMU
C
          IF (WRITE_Z_EVENT) THEN
            IF (PASSED) THEN
              CALL FLGSET('WRITE_STREAM_ZMU',.TRUE.)
            ENDIF
          ENDIF
C
          IF (PASSED) THEN
            NSTRIP_Z = NSTRIP_Z + 1
          ENDIF
C
        ENDIF
      ENDIF
C
C use the code to reject events passing the Z selection. Can be used to remove
C Z candidates from the W sample.
C
      IF (REJECT_Z_EVENT) THEN
        IF (NGOOD_PMUO.LT.2) PASSED = .FALSE.
        IF (.NOT.PASSED) THEN
          CALL FLGSET('WRITE_STREAM_ZMU',.TRUE.)
        ENDIF
      ENDIF
C
  999 RETURN
      END
