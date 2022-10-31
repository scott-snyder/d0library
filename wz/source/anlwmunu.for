      SUBROUTINE ANLWMUNU
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Analyzes events, looking for W -> mu nu
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
      INTEGER LSUP,LISAL,IDLM,IPMUO,NPMUO,LPMUO
      INTEGER IMU,IOFF,ILEP,GZPMUO,NMU,GZISAE,LISAE,NISAL
      INTEGER IDMAX(2), PMUO_VERS, STATUS
      INTEGER GZPARH,LPARH,GZPNUT,LPNUT
      INTEGER L1BITS,IRUN,IEVT,LENGTH
      INTEGER MODMU,NTDIM, L2_WORD1, L2_WORD2
      INTEGER NTUPLE_ID, NGOOD_PMUO
      INTEGER IER, NSTRIP, NGOOD_EVENTS
      INTEGER IERR,USER_ID, WAM_POL, SAM_POL
      INTEGER LMUOT, NS, LJETS_DR, LJETS_DP,ISAIDMU
C
      PARAMETER(MODMU=70)
      PARAMETER(NTDIM=210)
C
      REAL PM(68),PHIM,THM,ETAM,IUNOUT,SSUNIT
      REAL PMAX(68,2),ETMAX(2),P3MU(3),ISAPTMU,ISAETAMU,ISAPHIMU
      REAL P3MU1(3),P3MU2(3),P3TOT(3),AKINW(50)
      REAL ETM,COTTH,DPHI,IMPCXY, XY_IMPACT, COS_TRAN
      REAL PNT(NTDIM),ETA,PHI,TEMPLATE(3)
      REAL PTMIN1, PTMAX2, MISS_ET, ETACUT, PTCUT, MASS_LO, MASS_HI
      REAL ISOMAX, ISOMIN, CHISQ_GFIT
      REAL ET_SAME,ET_OPP,DR_MIN,DPHI_MIN,ET_JET,ET_BACK, DPHI_BACK
C
      REAL NUMJETS, MUJET_PTREL,MUJET_ENRAT
C
      LOGICAL WRITE_W_EVENT, FILL_NT_ALL, FILL_NT_CUT, FILL_NT_ISA
      LOGICAL WRITE_GOOD_MUON,DO_W_CUTS,PASSED,NEWFILE,FIRST
      LOGICAL CHECK_MAGNET, DO_GFIT_CUTS, OK
C
      CHARACTER*4 PATHOLD
      CHARACTER*6 CHTOP
      CHARACTER*64 TITLE
      CHARACTER*60 CHTAGS(NTDIM)
      CHARACTER*20 FILENAME
C
      EXTERNAL GZPMUO,GZISAE,SSUNIT,GZPARH
      EXTERNAL UCOPY, VZERO
      COMMON /WMUNU/ NTUPLE_ID,NSTRIP, NGOOD_EVENTS
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
     &'SPAREM1','PTREL1','ENRAT1',
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
     &'ENEB3MU2','L1W1MU2','L1W2MU2','L15W1MU2','L15W2MU2','ETSAME2',
     &'ETOPP2','DRJET2','ETJET2','ETBACK2','DPHIB2','IMPCXY2',
     &'SPAREM2','PTREL2','ENRAT2',
C
     &'NPMUO ','PMISSX','PMISSY','PMISSZ','EMISST','PTMISS','THENU',
     &'ETANU','PHINU','SCALET','L1WORD','IRUN ','IEVNT ','MTW','PTNU',
     &'PTW','WRAP','COSTHE','NGOODMU','L2WORD1','L2WORD2','MAGPOL',
     &'ISAID','ISAPT','ISAETA','ISAPHI','NUMJETS','SPARE02','SPARE03',
     &'SPARE04','SPARE05','SPARE06','SPARE07','SPARE08','SPARE09',
     &'SPARE010','SPARE011','SPARE012','SPARE013','SPARE014',
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
        NGOOD_EVENTS = 0
        NSTRIP = 0
        CALL EZPICK('WZ_RCP')
        CALL EZGET_l('DO_W_CUTS', DO_W_CUTS, IER)
        CALL EZGET_l('WRITE_W_EVENT', WRITE_W_EVENT, IER)
        CALL EZGET_l('WRITE_GOOD_MUON', WRITE_GOOD_MUON, IER)
        CALL EZGET_l('FILL_NT_ALL', FILL_NT_ALL, IER)
        CALL EZGET_l('FILL_NT_CUT', FILL_NT_CUT, IER)
        CALL EZGET_l('FILL_NT_ISA', FILL_NT_ISA, IER)
        CALL EZGET('ETACUT', ETACUT, IER)
        CALL EZGET('PTCUT', PTCUT, IER)
        CALL EZGET('W_PTMIN1', PTMIN1, IER)
        CALL EZGET('W_PTMAX2', PTMAX2, IER)
        CALL EZGET('W_MISS_ET', MISS_ET, IER)
        CALL EZGET('W_MASS_LO',MASS_LO , IER)
        CALL EZGET('W_MASS_HI',MASS_HI , IER)
        CALL EZGET('W_ISOMAX',ISOMAX , IER)
        CALL EZGET('W_ISOMIN',ISOMIN , IER)
        CALL EZGET('W_IMPCXY',IMPCXY , IER)
        CALL EZGET_l('CHECK_MAGNET', CHECK_MAGNET, IER)
        CALL EZGET_l('W_DO_GFIT_CUTS',DO_GFIT_CUTS , IER)
        CALL EZGET('W_CHISQ_GFIT',CHISQ_GFIT , IER)
        CALL EZGETS('W_CHTOP',1,CHTOP,LENGTH,IER)
        CALL EZRSET
C
C  user initialization hook
C
        CALL USRWINI(CHTAGS)
C
C first, do the special stuff required for a disk-resident ntuple
C (see D0$DOCS:NTUPLE.DOC)
        IF ((FILL_NT_ALL).OR.(FILL_NT_CUT).OR.(FILL_NT_ISA)) THEN
          USER_ID = 111
          NEWFILE = .TRUE.
          FILENAME = 'WMUNU_N.HST4'
          CALL NTUPLE_FILE_OPEN(USER_ID,NEWFILE,FILENAME,
     &      1024,CHTOP,IERR)
          IF(IERR.NE.0) THEN
            GOTO 999
          ENDIF
          CALL NTUPLE_BOOK(CHTOP,NTDIM,       !Book Ntuple
     &      CHTAGS,'WMUNU',NTUPLE_ID,IERR)
          IF(IERR.NE.0) THEN
            GOTO 999
          ENDIF
        ENDIF
C
C  book the ordinary histograms (no histos for now)
        CALL HCDIR ('//PAWC',' ')       ! GO TO TOP DIRECTORY
        CALL HMDIR ('WMUNU','S')       ! CREATE WMUNU DIRECTORY
C
        CALL HCDIR ('//PAWC',' ')       ! BACK TO TOP DIRECTORY
C
        FIRST = .FALSE.
      ENDIF
C----------------------------------------------------------------------
      CALL FLGSET('WRITE_STREAM_WMU',.FALSE.)       ! WMU stream
      CALL FLGSET('WRITE_STREAM_GMU',.FALSE.)       ! GMU stream
      PASSED = .TRUE.
      CALL HCDIR('//PAWC',' ')  ! Leave in TOP HBOOK DIRECTORY
      NGOOD_PMUO = 0
C
C set muctag track bit for EF tracks
      CALL MUCTAG_TRK
C
C get unit number for output file
C
      IUNOUT = SSUNIT()
C
      CALL HCDIR('//PAWC/WMUNU',' ')  ! go to WMUNU histogram directory
C
      CALL VZERO(PNT,NTDIM)
C
C check if it's Monte Carlo (isajet)
      IF(LHEAD.GT.0) THEN
        LISAE = LQ(LHEAD-IZISAE)
        IF(LISAE.GT.0) THEN
          ISAIDMU = 0
          ISAPTMU = 0.
          ISAETAMU = 0.
          ISAPHIMU = 0.
          CALL WMU_ISAJ(ISAPTMU,ISAETAMU,ISAPHIMU,ISAIDMU)
          PNT(2*MODMU+23) = ISAIDMU
          PNT(2*MODMU+24) = ISAPTMU
          PNT(2*MODMU+25) = ISAETAMU
          PNT(2*MODMU+26) = ISAPHIMU
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
        CALL VZERO_i(IDMAX,2)
        CALL VZERO(PM,68)
        PMUO_VERS = 0
        IDLM = 0
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
            PM(8) =  Q(LPMUO+28)
            PM(9) =  Q(LPMUO+29)
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
C impact parameters
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
            IF(PMUO_VERS.GE.3) THEN
C hits on track
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
            PNT(2*MODMU+27) = NUMJETS
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
C apply ETACUT and PTCUT as good muon quality selection.
C
            IF ((ABS(ETAM).LT.ETACUT).AND.(ETM.GT.PTCUT)) THEN
C
C call Offline Muon id and Cosmic Ray Rejection Routine. 
C Selection cuts in CLEANMU.RCP
C
              CALL CLEANMU(LPMUO,STATUS,OK)
              PM(24) = 2.0  ! muon failed good muon selection
              IF(OK) THEN
                NGOOD_PMUO = NGOOD_PMUO + 1
                PM(24) = 1.0  ! muon passed good muon selection
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
          ENDIF
        ENDDO
C
C store number of good muons in ntuple
        PNT(2*MODMU+19) = NGOOD_PMUO
C
C store two highest Pt good muons in ntuple
C
        IF (NGOOD_PMUO.GE.1) THEN
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
        PNT(2*MODMU+20) = FLOAT(L2_WORD1)
        PNT(2*MODMU+21) = FLOAT(L2_WORD2)
      ENDIF
C magnet polarity
      IF(CHECK_MAGNET) THEN
        CALL MAG_POLARITY(WAM_POL,SAM_POL,IERR)
        PNT(2*MODMU+22) = WAM_POL
        IF (WAM_POL.EQ.0.) PASSED = .FALSE.
      ENDIF
C
C W analysis
C
      IF(NGOOD_PMUO.GE.1) THEN
        CALL UCOPY(PMAX(1,1),P3MU1,3)
        LPNUT=GZPNUT(2)     ! pick missing ET bank without MUON correction
        IF(LPNUT.GT.0) THEN
          CALL UCOPY(Q(LPNUT+3),P3TOT,3)
        ELSE
          CALL VZERO(P3TOT,3)
        ENDIF
C subtract off calor missing pt to get total pt
        CALL VSUB(P3MU1,P3TOT,P3TOT,3)
C add in second muon if present
        IF(NGOOD_PMUO.GE.2) THEN
          CALL UCOPY(PMAX(1,2),P3MU2,3)
          CALL VADD(P3TOT,P3MU2,P3TOT,3)
        ENDIF
C calculate W kinematics
        CALL WKINEM(P3MU1,P3TOT,AKINW)
        PNT(2*MODMU+14) = AKINW(1)      ! invariant mass
        PNT(2*MODMU+15) = AKINW(3)      ! pt neutrino
        PNT(2*MODMU+16) = AKINW(4)      ! ptW
        PNT(2*MODMU+17) = AKINW(21)     ! W rapidity
        PNT(2*MODMU+18) = AKINW(26)     ! cosine theta*
      ENDIF
C
      CALL HCDIR('//PAWC',' ')  ! Leave in TOP HBOOK DIRECTORY
C
C  user hook for filling ntuples
C
      CALL USRWNTUPLE(PNT)
C
C fill ntuple for all the events that have a isajet bank
C
      IF (FILL_NT_ISA) THEN
        CALL NTUPLE_FILL(CHTOP,NTUPLE_ID,PNT,IERR)
        IF(IERR.NE.0) THEN
          GOTO 999
        ENDIF
      ENDIF
C
C fill ntuple for all the events that have at least one good muon
C
      IF (FILL_NT_ALL) THEN
        IF (NGOOD_PMUO.GE.1) THEN
          CALL NTUPLE_FILL(CHTOP,NTUPLE_ID,PNT,IERR)
          IF(IERR.NE.0) THEN
            GOTO 999
          ENDIF
        ENDIF
      ENDIF
C
C see if event passed selection
C
      IF(NGOOD_PMUO.GE.1) THEN
        IF (DO_W_CUTS) THEN
C
C cut on minimum PT of highest Pt muon
          IF (PNT(5).LT.PTMIN1) PASSED = .FALSE.
C cut on maximum PT of second highest Pt muon
          IF (PNT(75).GT.PTMAX2) PASSED = .FALSE.
C cut on missing Et calculated by WKINEM
          IF (PNT(155).LT.MISS_ET) PASSED = .FALSE.
C cut on invariant mass
          IF (AKINW(1).LT.MASS_LO.OR.AKINW(1).GT.MASS_HI)
     &      PASSED = .FALSE.
C cut on muon isolation
          IF (PNT(12).GT.ISOMAX) PASSED = .FALSE.
          IF (PNT(12).LT.ISOMIN) PASSED = .FALSE.
C cut on XY impact parameter
          IF (ABS(PNT(67)).GT.IMPCXY) PASSED = .FALSE.
C cut on global fitting parameters
          IF (DO_GFIT_CUTS) THEN
            IF (PNT(29).EQ.0.OR.PNT(29).EQ.2) PASSED = .FALSE.
            IF (PNT(30).GT.CHISQ_GFIT) PASSED = .FALSE.
          ENDIF
C
C fill ntuple only for the events that passed the cuts
C
          IF (FILL_NT_CUT) THEN
            IF (PASSED) THEN
              CALL NTUPLE_FILL(CHTOP,NTUPLE_ID,PNT,IERR)
              IF(IERR.NE.0) THEN
                GOTO 999
              ENDIF
            ENDIF
          ENDIF
C
C write out the event that passed the cuts. Stream WMU
C
          IF (WRITE_W_EVENT) THEN
            IF (PASSED) THEN
              CALL FLGSET('WRITE_STREAM_WMU',.TRUE.)
            ENDIF
          ENDIF
C
          IF (PASSED) THEN
            NSTRIP = NSTRIP + 1
          ENDIF
C
        ENDIF
      ENDIF
C
      IF  (NGOOD_PMUO.GE.1) THEN
        NGOOD_EVENTS = NGOOD_EVENTS + 1
        IF (WRITE_GOOD_MUON)
     &      CALL FLGSET ('WRITE_STREAM_GMU',.TRUE.)
      ENDIF
C
  999 RETURN
      END

