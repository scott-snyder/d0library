      FUNCTION W_NTPL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Sample subroutine for DST analysis. It loops through all
C-     reconstructed particle and jet banks.
C-
C-     version for e + jets + muon
C-
C-   Returned value  : true
C-
C-   ENTRY W_DIAL : example of dialog for option 'User Dialog'
C-
C-   Created  26-JUL-1990   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL W_NTPL,W_DIAL,W_DUMP,W_FIN
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER NR,NFIX,NTRIG,NFILT,NTVAR
      CHARACTER*32 TNAME
      INTEGER LTSUM, GZTSUM, POINT
      INTEGER DMPUNI,DUNIT,N1,LISAL,GZISAL,IOK,INDX(4),IW,IV,MRB
      INTEGER LPMUO,LJETS,LPNUT,LPELC,LPELC_USED,LZTRK,LZFIT
      INTEGER GZPMUO,GZJETS,GZPNUT,GZPELC,GZPPHO,LPPHO,ADD_PHOT,ICH
      INTEGER NEL,NJETS,I,RUN,ID,LPPHO_USED,J,K,ISYS,MAKE_NTUPL,MN,NJ
      INTEGER LJNEP,LHMTP,LHMTE,ICHOICE,IER,LVERT,GZVERT,LHSTR,GZHSTR
      INTEGER MAXJ
      PARAMETER (MAXJ=6)
      REAL    ETA,PHI,TEMPLATE(5,4),PHINU,PHIE,PHIT,THT,ETIN,DETA
      REAL    ET,ETMIN,ETJ(MAXJ),EM_FRAC,PJETS(5,MAXJ),ETMU,ETAM,PHIMU
      REAL    ETAJ(MAXJ),PHIJ(MAXJ),THETA,THJ(MAXJ),EJ(MAXJ),EIN
      REAL    MET1,MET2,ZVTX,PNUT(4),RETS,PSUM(5),MWT,MET3
      REAL    ETEL,YELC,ETAE,DIST,CHSQ,ETE_MIN,EISO,ETM
      REAL    MIN_DIST,MIPS,ETW,DPHI,DTH,R,MWH,DCAL,XYZ(3),COSA,DPHIM
      REAL    MAX_RATIO,MIN_EMFRAC,MWJ,M3J,ETMC,YMMC,WPZ2,MISO
      REAL    PLEP(5),PWLEP(5),PWHAD(7),PTOP1(5),PTOP2(5),EMEX,EMOB
      REAL    DIFF,DIFF_MIN,ETE2,ETAE2,PHIE2,PLEP2(5),RMUJ,EMF
      REAL    MTOT,MTRA,METC,ETERR,MWJ1,M3J1,ETA_MAX
      REAL    PCORRJ(5),PCORRE(5),SCALE,EOUT,PV(4,10),STRK
      REAL    ELDEX2,ELDEY2,NUDEX2,NUDEY2,JDEX2(MAXJ),JDEY2(MAXJ)
      REAL    SPHE,APLA,PHJ1,PHJ2,DEX2,DEY2,P3_EIGVAL(3),P3_EIGVEC(3,3)
      REAL    OLDE(5),NEWE(5),CORRE(5),CONE,NEWETA,NEWPHI,STORE,STATUS
      REAL    RRUN,RID,SPH1,APL1,TRGB,Y,Y1,PTOT(4),MOBJ,TQUAN(30)
      REAL    METC_ERROR,PHINU_ERROR,CAL_ESCALE(3),RESCALE,DZ
      CHARACTER*4 DUMP_WHAT
      LOGICAL FIRST,DO_DUMP,OK,BADRUN,OK_TRGR,FOUND_MATCH
      LOGICAL SELECT,GOOD_EL,GOOD_PH,DO_DCAL,GOOD_MUON
      LOGICAL GOOD_ELECTRON,GOOD_PHOTON,MC,V10,DO_CORRS(3)
      LOGICAL CLEAN_CAL_JUNK,NEW_CORR
      INTEGER NUM_BAD
      PARAMETER (NUM_BAD=18)
      INTEGER BAD_RUNS(NUM_BAD)
      INTEGER NUM_NTPL
      PARAMETER (NUM_NTPL=56)
      REAL    NTPL(NUM_NTPL)
      EQUIVALENCE (MET1,NTPL(1)),(MET2,NTPL(2)),(RRUN,NTPL(3))
      EQUIVALENCE (PHINU,NTPL(4))
      EQUIVALENCE (ETEL,NTPL(5)),(ETAE,NTPL(6)),(PHIE,NTPL(7))
      EQUIVALENCE (MWT,NTPL(8)),(ETW,NTPL(9)),(CHSQ,NTPL(10))
      EQUIVALENCE (EISO,NTPL(11)),(MIPS,NTPL(12))
      EQUIVALENCE (DPHI,NTPL(13)),(STRK,NTPL(14)),(ZVTX,NTPL(15))
      EQUIVALENCE (ETMU,NTPL(22)),(ETAM,NTPL(23)),(PHIMU,NTPL(24))
      EQUIVALENCE (ETJ,NTPL(16))
      EQUIVALENCE (ETAJ,NTPL(25)),(PHJ1,NTPL(31)),(PHJ2,NTPL(32))
      EQUIVALENCE (ETE2,NTPL(33)),(ETAE2,NTPL(34)),(PHIE2,NTPL(35))
      EQUIVALENCE (MWH,NTPL(36))
      EQUIVALENCE (MWJ,NTPL(37)),(M3J,NTPL(38))
      EQUIVALENCE (ETMC,NTPL(39)),(YMMC,NTPL(40))
      EQUIVALENCE (MISO,NTPL(41)),(EMOB,NTPL(42))
      EQUIVALENCE (METC,NTPL(43)),(EJ,NTPL(44))
      EQUIVALENCE (SPHE,NTPL(50)),(APLA,NTPL(51))
      EQUIVALENCE (SPH1,NTPL(52)),(APL1,NTPL(53))
      EQUIVALENCE (MET3,NTPL(54)),(RID,NTPL(55)),(TRGB,NTPL(56))
      CHARACTER*4 TAGS(NUM_NTPL)
      DATA TAGS/
     &          'MET1','MET2','RUN','PHNU',
     &          'ETEL','ETAE','PHEL',
     &          'MWT','ETW','CHSQ','EISO','MIPS',
     &          'DPHI','STRK','ZVTX',
     &          'ETJ1','ETJ2', 'ETJ3','ETJ4','ETJ5','ETJ6',
     &          'ETMU','ETAM','PHMU',
     &          'ETA1','ETA2','ETA3','ETA4','ETA5','ETA6',
     &          'PHJ1','PHJ2',
     &          'ETE2','TAE2','PHE2',
     &          'MWH','MWJ','M3J',
     &          'ETMC','YMMC','MISO','EMOB',
     &          'METC','EJ1','EJ2','EJ3','EJ4','EJ5','EJ6',
     &          'SPHE','APLA','SPH1','APL1','MET3','RID','TRGB'/
      DATA ADD_PHOT/0/
      DATA FIRST/.TRUE./
      DATA ICHOICE,ETMIN/1,15./
      DATA MAKE_NTUPL,DO_DUMP/3,.FALSE./
      DATA DO_DCAL/.FALSE./
      DATA OK_TRGR/.TRUE./
      DATA DO_CORRS/3*.TRUE./
      DATA ETA_MAX/10./
      DATA CONE/0.7/
      DATA ISYS/0/
      DATA TEMPLATE/
     &  1.,6.,0.7,0.,0.,      ! CONE R=0.7
     &  1.,6.,0.5,0.,0.,      ! CONE R=0.5
     &  1.,6.,0.3,0.,0.,      ! CONE R=0.3
     &  2.,7.,2.,8.,2./       ! NN 2x2
      DATA BAD_RUNS/
c     &  51260,51261,51262,51263,51264,51265,51266,51267,51268, !
c     &  53752,54213,54752,54583,54654,54697,54788,54736,       ! old stuff
c     &  55000,55113,55114/                                     !
C
     &  56398,56412,56427,56428,57183,57188,57362,57363,57512,57437,
     &  57441,57545,57547,57551,57140,57066,57162,57180/
      DATA DUMP_WHAT/'NONE'/
      DATA CAL_ESCALE/1.025,1.072,1.015/
C----------------------------------------------------------------------
C
      W_NTPL=.FALSE.      ! set it to false to skip any additional
                          ! processing of current event
C
C       book histograms in directory DST
C
      IF(FIRST) THEN
        FIRST=.FALSE.
C
C           book histograms
        CALL HCDIR('//PAWC',' ')    ! go to top directory
        CALL HBOOK1(22,'MWJ1, 2 jets',50,0.,250.,0.)
        CALL HBOOK1(23,'MWJ1, 3 jets',50,0.,250.,0.)
        CALL HBOOK1(24,'MWJ1, 4 jets',50,0.,250.,0.)
        CALL HBOOK1(12,'MWH, 2 jets',50,0.,150.,0.)
        CALL HBOOK1(13,'MWH, 3 jets',50,0.,150.,0.)
        CALL HBOOK1(14,'MWH, 4 jets',50,0.,150.,0.)
        CALL LJTOP_READ_CUTS
        MN=MOD(MAKE_NTUPL,2)
        IF(MN.NE.0) CALL HBOOKN(1,'DST analysis',16,' ',200000,TAGS)
        IF(MAKE_NTUPL.GT.1)
     &     CALL HBOOKN(2,'DST analysis',NUM_NTPL,' ',200000,TAGS)
        CALL GRLINK('W_NTPL',N1)
      ENDIF
C
      V10=.FALSE.
      LHSTR=GZHSTR()
      IF(LHSTR.GT.0) V10=IQ(LHSTR+3).LT.11
      CALL HCDIR('//PAWC',' ')  ! go to top directory
      CALL EVNTID(RUN,ID)
      RRUN=RUN
      RID=ID
      MC=.FALSE.
      IF(IQ(LHEAD+1).GT.1000) MC=.TRUE.     ! MC data
      IV=11
      IF(V10) IV=10
      IF(MC) IV=0
C
C         remove bad runs, IGNORED FOR NOW, removed by CLEAN_CAL_JUNK
      BADRUN=.FALSE.
      DO I=1,NUM_BAD
        IF(RUN.EQ.BAD_RUNS(I)) BADRUN=.TRUE.
      ENDDO
C
C          select EM triggers
      TRGB=0
      IF(OK_TRGR) THEN
        LTSUM = GZTSUM()
        IF ( LTSUM.LE.0 ) GOTO 999
        OK=.FALSE.
        NFIX=IQ(LTSUM+2)
        NR  =IQ(LTSUM+3)
        NTRIG=IQ(LTSUM+4)
        NFILT=IQ(LTSUM+5)
        POINT=LTSUM+NFIX
        DO I=1,NTRIG
          CALL UHTOC(IQ(POINT+2),8,TNAME,32)
          POINT=POINT+NR
        ENDDO
        DO I=1,NFILT
          CALL UHTOC(IQ(POINT+2),8,TNAME,32)
          IF(TNAME(1:4).EQ.'ELE_') OK=.TRUE.
          IF(TNAME(1:8).EQ.'JET_MULT') TRGB=2
          POINT=POINT+NR
        ENDDO
        IF(OK) TRGB=TRGB+1
        IF(.NOT.OK.AND.ADD_PHOT.GT.-100) GOTO 999
      ENDIF
C
      IF(.NOT.CLEAN_CAL_JUNK()) GOTO 999 ! junk event
      MRB=IAND(IQ(LHEAD+30),1)
      IF(MRB.NE.0) GOTO 999   ! main ring event
      DO I=1,MAXJ
        ETJ(I)=-10.
        ETAJ(I)=-10.
        PHIJ(I)=-10.
        EJ(I)=-10.
      ENDDO
      ZVTX=-1000.
      LRLINK(N1)=0
      LVERT=GZVERT(1)
      IF(LVERT.GT.0) ZVTX=Q(LVERT+5)
      IF(MC.AND.ICHOICE.LT.0) CALL ZVERTX(ZVTX,DZ)
      IF(ZVTX.EQ.0.0) THEN
        CALL ERRMSG('ZVTX=0','W_NTPL', 'Vertex found','W')
      ELSEIF(ZVTX.LT.-999.) THEN
        CALL ERRMSG('ZVTX=0','W_NTPL', 'Vertex NOT found','W')
        ZVTX=0.
      ENDIF
      DO I=1,5
        PCORRJ(I)=0.
      ENDDO
C
C        missing ET
C
      LPNUT=GZPNUT(1)     ! pick missing ET bank without ICD correction
      IF(LPNUT.GT.0) THEN
        CALL UCOPY(Q(LPNUT+3),PNUT,4)
        MET1=SQRT(PNUT(1)**2+PNUT(2)**2)
      ENDIF
      LPNUT=GZPNUT(2)     ! pick missing ET bank with ICD correction
      IF(LPNUT.GT.0) THEN
        CALL UCOPY(Q(LPNUT+3),PNUT,4)
        MET2=SQRT(PNUT(1)**2+PNUT(2)**2)
        PHINU=Q(LPNUT+10)
        NUDEX2=Q(LPNUT+11)
        NUDEY2=Q(LPNUT+12)
      ENDIF
C        correct for energy scale
      NEW_CORR=.NOT.MC.AND.ICHOICE.LT.0
      ICH=IABS(ICHOICE)
      IF(NEW_CORR) THEN
        CALL MET_CORRECTION( .TRUE., .TRUE., .FALSE., .TRUE.,
     &    METC, METC_ERROR, PHINU, PHINU_ERROR, IER )
        IF(METC.LT.0.) METC=MET2
      ELSE
        CALL MISS_ET_CORR(IV,ZVTX,CAL_ESCALE,PCORRJ)
      ENDIF
      IF(.NOT.NEW_CORR) THEN
        DO I=1,2
          PNUT(I)=PNUT(I)-PCORRJ(I)
        ENDDO
        METC=SQRT(PNUT(1)**2+PNUT(2)**2)
        PHINU=ATAN2(PNUT(2),PNUT(1))
      ENDIF
      IF(MET2.LT.ETE_MIN.AND.METC.LT.ETE_MIN) GOTO 999
C
C       IF ADD_PHOT=1000 replace e's with mu's
C
      IF(ADD_PHOT.EQ.1000) THEN
        LPMUO=GZPMUO(0)
        ETEL=14.
        EISO=10.
        DO WHILE (LPMUO.GT.0)
          IF(Q(LPMUO+14).GT.ETEL.AND.IQ(LPMUO+9).LT.2) THEN
            CALL DET_ETA(ZVTX,Q(LPMUO+15),ETAE)
            IF(GOOD_MUON(LPMUO,ETAE)) THEN
              EMEX=Q(LPMUO+33)
              EMOB=Q(LPMUO+35)
              ETMU=Q(LPMUO+14)
              IF(EMOB-EMEX.LT.EISO) THEN
                EISO=EMOB-EMEX
                ETEL=Q(LPMUO+14)
                THETA=Q(LPMUO+15)
                PHIE=Q(LPMUO+17)
                CALL UCOPY(Q(LPMUO+10),PLEP,4)
                LRLINK(N1)=LPMUO
              ENDIF
            ENDIF
          ENDIF
          LPMUO=LQ(LPMUO)
        ENDDO
        IF(ETEL.LT.ETE_MIN) GOTO 999
        GOTO 900       ! Skip electrons
      ENDIF
C
C        find electron
      STRK=500.
      ETEL=ETE_MIN-1.
      IF(ETE_MIN.LT.15.) ETEL=14.
C
      IF(ADD_PHOT.NE.1) THEN
        LPELC=GZPELC()
        DO WHILE (LPELC.GT.0)
C          IF(MC) THEN
C            SELECT=Q(LPELC+7).GT.ETEL.AND.GOOD_EL(LPELC)
C          ELSE
          THETA=Q(LPELC+8)
          CALL DET_ETA(ZVTX,THETA,ETAE)
          IF(.NOT.NEW_CORR) THEN
            SCALE=CAL_ESCALE(2)
            IF(ETAE.GT.1.2) SCALE=CAL_ESCALE(3)
            IF(ETAE.LT.-1.2) SCALE=CAL_ESCALE(1)
            IF(IV.EQ.10) SCALE=SCALE*1.015
            IF(IV.EQ.0) SCALE=1.0
            DO I=3,7
              Q(LPELC+I)=Q(LPELC+I)*SCALE
            ENDDO
            Q(LPELC+7)=Q(LPELC+7)*SCALE
          ENDIF
          CALL CLEANEM(LPELC,1,OK,STATUS)
          SELECT=Q(LPELC+7).GT.ETEL.AND.GOOD_ELECTRON(LPELC,'TIGHT')
          IF(ICHOICE.LT.0.AND.MC)
     &      SELECT=Q(LPELC+7).GT.ETEL.AND.GOOD_EL(LPELC)
          IF(ADD_PHOT.LT.0) SELECT=.TRUE.
          IF(SELECT) THEN
            ETEL=Q(LPELC+7)
            PHIE=Q(LPELC+10)
            CALL UCOPY(Q(LPELC+3),PLEP,4)
            LRLINK(N1)=LPELC
            LHMTE=LQ(LPELC-1)
            CHSQ=Q(LHMTE+7)
            EISO=(Q(LPELC+16)-Q(LPELC+17))/(Q(LPELC+17)+.001)
            LZTRK=LQ(LPELC-3)
            IF(LZTRK.GT.0) THEN
              LZFIT=LQ(LZTRK-1)
              PHIT=Q(LZFIT+10)
              THT=Q(LZFIT+13)
              DPHI=PHIE-PHIT
              DTH= THETA-THT
              MIPS=Q(LZFIT+26)
              CALL CLEANEM_TQUANS(NTVAR,TQUAN)
              STRK=TQUAN(12)
            ENDIF
            ELDEX2=Q(LPELC+11)
            ELDEY2=Q(LPELC+12)
          ENDIF
          LPELC=LQ(LPELC)
        ENDDO
      ENDIF
      IF(ETEL.LT.20..AND.ADD_PHOT.LT.-100) LRLINK(N1)=0
C
C        find "photon"
      IF((ETEL.LT.ETE_MIN.OR.ETEL.LT.15.).AND.IABS(ADD_PHOT).GT.0) THEN
        STRK=1000.
        LPPHO=GZPPHO()
        DO WHILE (LPPHO.GT.0)
C          IF(Q(LPPHO+7).GT.ETEL.AND.GOOD_PH(LPPHO)) THEN
          SELECT=.TRUE.
          IF(ADD_PHOT.GT.0) SELECT=GOOD_PHOTON(LPPHO,'TIGHT')
          THETA=Q(LPPHO+8)
          CALL DET_ETA(ZVTX,THETA,ETAE)
          IF(.NOT.NEW_CORR) THEN
            SCALE=CAL_ESCALE(2)
            IF(ETAE.GT.1.2) SCALE=CAL_ESCALE(3)
            IF(ETAE.LT.-1.2) SCALE=CAL_ESCALE(1)
            IF(IV.EQ.10) SCALE=SCALE*1.015
            IF(IV.EQ.0) SCALE=1.0
            DO I=3,7
              Q(LPPHO+I)=Q(LPPHO+I)*SCALE
            ENDDO
          ENDIF
          IF(Q(LPPHO+7).GT.ETEL.AND.SELECT) THEN
            LRLINK(N1)=LPPHO
            ETEL=Q(LPPHO+7)
            PHIE=Q(LPPHO+10)
            CALL UCOPY(Q(LPPHO+3),PLEP,4)
            LPPHO_USED=LPPHO
            LHMTP=LQ(LPPHO-1)
            CHSQ=Q(LHMTP+5)
            EISO=(Q(LPPHO+16)-Q(LPPHO+17))/(Q(LPPHO+17)+.001)
            DPHI=-10.
            DTH=-10.
          ENDIF
          LPPHO=LQ(LPPHO)
        ENDDO
      ENDIF
      IF(CHSQ.GT.100..AND.ADD_PHOT.GE.0) GOTO 999
      IF((ETEL.LT.ETE_MIN.OR.ETEL.LT.15.).AND.ADD_PHOT.GT.-100) 
     &  GOTO 999 !
      IF(ETEL.LT.20..AND.ADD_PHOT.LT.-100) LRLINK(N1)=0
  900 CONTINUE
      CALL SECOND_E(LRLINK(N1),ETE2,PHIE2,THETA,PLEP2)
      IF(ETE2.GT.0) CALL DET_ETA(ZVTX,THETA,ETAE2)
C
C       jets
C
      IF(ICH.EQ.1) CALL SET_CAPH('CONE_JET',TEMPLATE(1,1),IER)
      IF(ICH.EQ.2) CALL SET_CAPH('CONE_JET',TEMPLATE(1,2),IER)
      IF(ICH.EQ.3) CALL SET_CAPH('CONE_JET',TEMPLATE(1,3),IER)
      IF(ICH.EQ.4) CALL SET_CAPH('NN_JET',TEMPLATE(1,4),IER)
C
      LJETS=GZJETS()
C          sort banks so they are in decreasing order of Et
C          NOTE: after each reordering of banks the pointer
C                LJETS must be refetched
      IF(LJETS.NE.0) THEN
        CALL ZSORT(IXCOM,LJETS,6)
        LJETS=GZJETS()
        CALL ZTOPSY(IXCOM,LJETS)
        LJETS=GZJETS()
C
C        loop over all jets but skip one with electron
        NJETS=0
        MAX_RATIO=0.
        MIN_EMFRAC=1.
        DO WHILE (LJETS.GT.0)
          LJNEP=LQ(LJETS-2)
          IF(Q(LJETS+19).GT.MAX_RATIO) MAX_RATIO=Q(LJETS+19)
          EMF=Q(LJETS+14)
          IF(EMF.LT.MIN_EMFRAC) MIN_EMFRAC=EMF
C               If ADD_PHOT < -100 use EM jets also as "electrons"
          IF(EMF.GT.0.9.AND.LRLINK(N1).EQ.0.AND.ADD_PHOT.LT.-100) THEN
            STRK=2000.
            ETEL=SQRT(Q(LJETS+2)**2+Q(LJETS+3)**2)
            THETA=Q(LJETS+7)
            PHIE=Q(LJETS+8)
            CALL UCOPY(Q(LJETS+2),PLEP,4)
            CALL DET_ETA(ZVTX,THETA,ETAE)
            SCALE=1.072
            IF(ETAE.GT.1.2) SCALE=1.015
            IF(ETAE.LT.-1.2) SCALE=1.025
            ETEL=ETEL*SCALE
            IF(ETEL.GT.20.) LRLINK(N1)=LJETS
            DO I=1,4
              PLEP(I)=PLEP(I)*SCALE
            ENDDO
            IF(LRLINK(N1).NE.0) GOTO 500
          ENDIF
          IF ( LQ(LJETS-3).NE.LRLINK(N1)
     &      .OR.LQ(LJETS-3).EQ.0) THEN
            ETIN=SQRT(Q(LJETS+2)**2+Q(LJETS+3)**2)
            CALL UCOPY(Q(LJETS+2),OLDE,4)
            OLDE(5)=ETIN
            EIN=Q(LJETS+5)
            ETA=Q(LJETS+9)
            THETA=Q(LJETS+7)
            PHI=Q(LJETS+8)
            DEX2=Q(LJETS+10)
            DEY2=Q(LJETS+11)
            CALL DET_ETA(ZVTX,THETA,DETA)
            IF ( MC ) THEN
              CALL MC_ET_CORR(OLDE,ETA,PHI,EMF,ICH,NEWE,ETA,PHI,
     &          PCORRJ,FOUND_MATCH)
            ELSE
              CALL JET_CONE_CORR(LJETS,.TRUE.,.TRUE.,.TRUE.,CAL_ESCALE,
     &            ZVTX,ISYS,NEWE(4),NEWE(5),ETA,PHI,NEWE,FOUND_MATCH)
            ENDIF
C
C              kludge to study effect of wrong E-scale
C              ETA_MAX used to avoid adding new parameter
            IF(ETA_MAX.GT.10.) THEN
              RESCALE=ETA_MAX-10.
              DO I=1,5
                NEWE(I)=NEWE(I)*RESCALE
              ENDDO
            ENDIF
C
            EOUT=NEWE(4)
            ET=SQRT(NEWE(1)**2+NEWE(2)**2)
C
            IF(ET.GT.ETMIN.AND.ABS(ETA).LT.ETA_MAX) THEN
              NJETS=NJETS+1
C
              IF(NJETS.LT.MAXJ+1) THEN
                ETAJ(NJETS)=DETA
                THJ(NJETS)=THETA
                PHIJ(NJETS)=PHI
                JDEX2(NJETS)=DEX2
                JDEY2(NJETS)=DEY2
                DO I=1,4
                  PJETS(I,NJETS)=NEWE(I)
                ENDDO
                ETJ(NJETS)=ET
                PJETS(5,NJETS)=ETJ(NJETS)
                EJ(NJETS)=NEWE(4)
              ENDIF
            ENDIF
          ELSEIF (Q(LJNEP+6).GT.ETMIN) THEN
            EMF=Q(LJNEP+14)
            EIN=Q(LJNEP+5)
            ETIN=SQRT(Q(LJNEP+2)**2+Q(LJNEP+3)**2)
            ETA=Q(LJNEP+9)
            THETA=Q(LJNEP+7)
            PHI=Q(LJNEP+8)
            DEX2=Q(LJNEP+10)
            DEY2=Q(LJNEP+11)
            CALL DET_ETA(ZVTX,THETA,DETA)
            CALL UCOPY(Q(LJNEP+2),OLDE,4)
            OLDE(5)=SQRT(OLDE(1)**2+OLDE(2)**2)
            IF ( MC ) THEN
              CALL MC_ET_CORR(OLDE,ETA,PHI,EMF,ICH,NEWE,ETA,PHI,
     &          PCORRJ,FOUND_MATCH)
              EOUT=NEWE(4)
              ET=SQRT(NEWE(1)**2+NEWE(2)**2)
            ELSE
              CALL QCD_JET_CORRECTION_2(OLDE(4),OLDE(5),ETA,EMF,
     &            0.,0.7,.TRUE.)
              CALL QCD_JET_CORRECTION(0,DO_CORRS(1),DO_CORRS(2),
     &            DO_CORRS(3),ZVTX,ISYS,NEWE(4),NEWE(5),NEWETA,IER)
              CORRE(1)= NEWE(5)/OLDE(5)
              CALL VSCALE(OLDE,CORRE,NEWE,3)
              EOUT=NEWE(4)
              ET=SQRT(NEWE(1)**2+NEWE(2)**2)
            ENDIF
            ETA=DETA
            IF(ET.GT.ETMIN) THEN
              PHI=Q(LJNEP+8)
              THETA=Q(LJNEP+7)
              NJETS=NJETS+1
              IF(NJETS.LT.MAXJ+1) THEN
                THJ(NJETS)=THETA
                PHIJ(NJETS)=PHI
                ETJ(NJETS)=ET
                JDEX2(NJETS)=DEX2
                JDEY2(NJETS)=DEY2
                CALL DET_ETA(ZVTX,THETA,ETAJ(NJETS))
                DO I=1,4
                  PJETS(I,NJETS)=NEWE(I)
                ENDDO
                PJETS(5,NJETS)=ET
              ENDIF
            ENDIF
          ENDIF
  500     LJETS=LQ(LJETS)  ! pointer to next jet
        ENDDO
C
      ENDIF
      CALL RESET_CAPH
      IF(NJETS.GT.MAXJ) NJETS=MAXJ          ! can't handle more than MAXJ
      YMMC=-1.
      IF(CHSQ.GT.50..AND.ABS(PHIE-PHIJ(1)).GT.2.9.AND.
     &    ABS(PHIE-PHIJ(1)).LT.3.38.AND.ADD_PHOT.GE.0) YMMC=1.
C
C                reorder jets
      DO K=1,NJETS-1
        DO I=K+1,NJETS
          IF(ETJ(I).GT.ETJ(I-1)) THEN
            STORE=ETJ(I)
            ETJ(I)=ETJ(I-1)
            ETJ(I-1)=STORE
            STORE=EJ(I)
            EJ(I)=EJ(I-1)
            EJ(I-1)=STORE
            STORE=ETAJ(I)
            ETAJ(I)=ETAJ(I-1)
            ETAJ(I-1)=STORE
            STORE=PHIJ(I)
            PHIJ(I)=PHIJ(I-1)
            PHIJ(I-1)=STORE
            CALL UCOPY(PJETS(1,I),OLDE,5)
            CALL UCOPY(PJETS(1,I-1),PJETS(1,I),5)
            CALL UCOPY(OLDE,PJETS(1,I-1),5)
          ENDIF
        ENDDO
      ENDDO
C
      IF(ADD_PHOT.EQ.1000) THEN  ! mu instead of e
        MISO=10.
        LPMUO=LRLINK(N1)
        DO I=1,NJETS
          DPHIM=ABS(PHIJ(I)-PHIE)
          IF(DPHIM.GT.PI) DPHI=TWOPI-DPHIM
          RMUJ=SQRT((ETAJ(I)-ETAE)**2+DPHIM**2)
          IF(RMUJ.LT.MISO) MISO=RMUJ
        ENDDO
        EMEX=Q(LPMUO+33)
        PNUT(1)=METC*COS(PHINU)-Q(LPMUO+10)+EMEX*COS(PHIMU)
        PNUT(2)=METC*SIN(PHINU)-Q(LPMUO+11)+EMEX*SIN(PHIMU)
        MET3=SQRT(PNUT(1)**2+PNUT(2)**2)
      ENDIF
C
      ETW=(PNUT(1)+PLEP(1))**2+(PNUT(2)+PLEP(2))**2
      MWT=(METC+ETEL)**2-ETW
      IF(MWT.LE.0.) GOTO 999
      MWT=SQRT(MWT)
      ETW=SQRT(ETW)
C
C            find first W-> leptons
      CALL FIND_WLNU(80.,PLEP,PNUT,PWLEP,WPZ2,OK)
      PSUM(5)=ETW
      DO K=1,NJETS
        PSUM(5)=SQRT(PJETS(1,K)**2+PJETS(2,K)**2)+PSUM(5)
      ENDDO
      DO I=1,4
        PSUM(I)=PWLEP(I)
        DO K=1,NJETS
          PSUM(I)=PSUM(I)+PJETS(I,K)
        ENDDO
      ENDDO
      MTOT=PSUM(4)**2-PSUM(1)**2-PSUM(2)**2-PSUM(3)**2
      IF(MTOT.GT.0.) MTOT=SQRT(MTOT)
      MTRA=PSUM(5)**2-PSUM(1)**2-PSUM(2)**2
      IF(MTRA.GT.0.) MTRA=SQRT(MTRA)
      MWH=-10.
      MWJ=-10.
      M3J=-10.
      IOK=0
      IF(ETJ(2).GT.0.) THEN
C
C
C        CALL LJFIND_TOPS1(NJETS,PJETS,PWLEP,
C     &      PTOP1,PTOP2,PWHAD,INDX,IOK)         ! 1ST algorithm
C        MWJ1=PTOP1(5)
C        M3J1=PTOP2(5)
        CALL UCOPY(PWLEP,PV,4)
        IF(ETJ(4).GT.0.) THEN
          CALL LJFIND_TOPS2(PWLEP,PJETS,ETMIN,
     &      PTOP1,PTOP2,PWHAD,INDX)         ! 2nd algorithm
          MWH=PWHAD(5)
          MWJ=PTOP1(5)
          M3J=PTOP2(5)
        ENDIF
      ENDIF
      LPMUO=GZPMUO(0)
      ETMU=-10.
      ETAM=-10.
      DO WHILE (LPMUO.GT.0)
        IF(Q(LPMUO+14).GT.ETMU.AND.IQ(LPMUO+9).LT.2
     &    .AND.LPMUO.NE.LRLINK(N1)) THEN
          CALL DET_ETA(ZVTX,Q(LPMUO+15),ETAM)
          IF(GOOD_MUON(LPMUO,ETAM)) THEN
            PHIMU=Q(LPMUO+17)
            EMEX=Q(LPMUO+33)
            EMOB=Q(LPMUO+36)
            ETMU=Q(LPMUO+14)
            MISO=10.
            DO I=1,NJETS
              DPHIM=ABS(PHIJ(I)-PHIMU)
              IF(DPHIM.GT.PI) DPHI=TWOPI-DPHIM
              RMUJ=SQRT((ETAJ(I)-ETAM)**2+DPHIM**2)
              IF(RMUJ.LT.MISO) MISO=RMUJ
            ENDDO
          ENDIF
        ENDIF
        IF (ADD_PHOT.LT.1000  ) THEN
          PNUT(1)=METC*COS(PHINU)-Q(LPMUO+10)+EMEX*COS(PHIMU)
          PNUT(2)=METC*SIN(PHINU)-Q(LPMUO+11)+EMEX*SIN(PHIMU)
        ELSE
          PNUT(1)=PNUT(1)-Q(LPMUO+10)+EMEX*COS(PHIMU)
          PNUT(2)=PNUT(2)-Q(LPMUO+11)+EMEX*SIN(PHIMU)
        ENDIF
        MET3=SQRT(PNUT(1)**2+PNUT(2)**2)
        LPMUO=LQ(LPMUO)
      ENDDO
C
C        Isajet information
      IF(MC) THEN
        ETMC=-10.
        YMMC=-10.
        LISAL=GZISAL()
        DO WHILE (LISAL.GT.0)
          IF(IABS(IQ(LISAL+1)).EQ.14) THEN
            ETM=SQRT(Q(LISAL+2)**2+Q(LISAL+3)**2)
            IF(ETM.GT.ETMC) THEN
              ETMC=ETM
              CALL DET_ETA(ZVTX,Q(LISAL+8),YMMC)
            ENDIF
          ENDIF
          LISAL=LQ(LISAL)
        ENDDO
      ENDIF
      DO J=1,NJETS
        CALL UCOPY(PJETS(1,J),PV(1,J+1),4)
        IF(PJETS(5,J).GT.20.) THEN
          CALL PAIR_MASS(PWLEP,PJETS(1,J),PWHAD)
          IF(NJETS.GT.1) CALL HFILL(22,PWHAD(5),0.,1.)
          IF(NJETS.GT.2) CALL HFILL(23,PWHAD(5),0.,1.)
          IF(NJETS.GT.3) CALL HFILL(24,PWHAD(5),0.,1.)
        ENDIF
        IF(PJETS(5,J).GT.25.) THEN
C
C         find a W -> hadrons
          DO K=1,NJETS
            IF(PJETS(5,K).GT.25..AND.K.NE.J) THEN
              CALL PAIR_MASS(PJETS(1,K),PJETS(1,J),PWHAD)
              CALL HFILL(12,PWHAD(5),0,1.)
              IF(NJETS.GT.2) CALL HFILL(13,PWHAD(5),0,1.)
              IF(NJETS.GT.3) CALL HFILL(14,PWHAD(5),0,1.)
            ENDIF
          ENDDO
C
        ENDIF
      ENDDO
      CALL SPHERICITY0(NJETS+1,PV,0,SPHE,Y,APLA,P3_EIGVAL,P3_EIGVEC,
     &  PTOT,MOBJ)
      CALL SPHERICITY0(NJETS,PV(1,2),0,SPH1,Y1,APL1,P3_EIGVAL,P3_EIGVEC,
     &  PTOT,MOBJ)
      IF(MN.NE.0) CALL HFN(1,NTPL)
      IF(ETJ(1).GT.0..AND.MAKE_NTUPL.GT.1) CALL HFN(2,NTPL)
      W_NTPL=.TRUE.      ! set it to false to skip any additional
      SELECT=.FALSE.
      DO_DUMP=DUMP_WHAT.NE.'NONE'
      IF(DO_DUMP) THEN
        IF(DUMP_WHAT(1:2).EQ.'4J') SELECT=ETJ(4).GT.15..AND.METC.GT.30.
        IF(DUMP_WHAT(1:2).EQ.'3J') SELECT=ETJ(3).GT.25.
        IF(DUMP_WHAT(1:2).EQ.'5J') SELECT=ETJ(5).GT.15.
        IF(DUMP_WHAT(1:2).EQ.'EE') SELECT=ETE2.GT.15.
        IF(DUMP_WHAT(1:3).EQ.'MWT') SELECT=MWT.GT.120.
        IF(DUMP_WHAT(1:3).EQ.'MUJ') SELECT=
     &      (ETMU.GT.4.0.AND.MISO.LT.0.5)
        IF(DUMP_WHAT(1:4).EQ.'MUNJ') SELECT=
     &      (ETMU.GT.4.0.AND.MISO.GT.0.5)
        IF(DUMP_WHAT(1:4).EQ.'APLA') SELECT=
     &      ETJ(4).GT.15..AND.APLA.GT.0.08.AND.METC.GT.30.
        IF(ADD_PHOT.GE.0.AND.ADD_PHOT.LT.10) 
     &    SELECT=SELECT.AND.EISO.LT.0.1.AND.STRK.LT.5.0
        IF(SELECT) CALL EVENT_LIST(RUN,ID)
        CALL FLGSET('DUMP_EVENT',SELECT)
        IF(DUMP_WHAT(1:4).EQ.'PV4J'.AND.NJETS.GT.3) THEN
          CALL D0OPEN(19,'PV4J.DAT','OF',OK)
          WRITE(19,*) NJETS+2,RUN,ID
          WRITE(19,*) (PLEP(I),I=1,4),ETEL,ETAE,PHIE,ELDEX2,ELDEY2
          WRITE(19,*) (PNUT(I),I=1,4),MET2,MET1,PHINU,NUDEX2,NUDEY2
          DO J=1,NJETS
            WRITE(19,*) (PJETS(I,J),I=1,4),ETJ(J),ETAJ(J),PHIJ(J),
     &          JDEX2(J),JDEY2(J)
          ENDDO
        ENDIF
      ENDIF
      GOTO 999
C
C
      ENTRY W_DIAL
C
      W_DIAL=.TRUE.
      ICHOICE=1
      CALL GETPAR(1,'Chose Cone jets (1,2,3) or NN jets (4) [1]>',
     &    'I',ICHOICE)
      ETMIN=10.
      CALL GETPAR(1,'ETMIN for jets [10.]>','R',ETMIN)
      ETA_MAX=10.
      CALL GETPAR(1,'ETA_MAX for jets [10.]>','R',ETA_MAX)
      ETE_MIN=15.
      CALL GETPAR(1,'ETMIN for electrons [15.]>','R',ETE_MIN)
      IF(ETE_MIN.LT.0) THEN
        ETE_MIN=-ETE_MIN
        CALL GOOD_EL_SET(.TRUE.)
        DO_DCAL=.TRUE.
      ENDIF
      OK_TRGR=.TRUE.
      CALL GETPAR(1,'Select e triggers only? [Y]>','L',OK_TRGR)
      ADD_PHOT=0
      CALL GETPAR(1,'Include photons? 0=no, 1=only photons,
     &    2= phot + el [0]>','I',ADD_PHOT)
      MAKE_NTUPL=3
      CALL GETPAR(1,'Make Ntuples 1,2,3? [3]>','I',MAKE_NTUPL)
      DUMP_WHAT='NONE'
      CALL GETPAR(1,'Dump selected events(3J,4J,5J,MET,EE,MUJ)? [NONE]>'
     &    ,'C',DUMP_WHAT)
      IF(DUMP_WHAT.EQ.' ') DUMP_WHAT='NONE'
      GOTO 999
C
C
      ENTRY W_DUMP
      W_DUMP=.TRUE.
      CALL DST_DUMP
      DUNIT=DMPUNI()
      IW=INDX(2)*10+INDX(3)
      WRITE(DUNIT,110) IW,MWH,MWT,ETW,MWJ,M3J
  110 FORMAT(//' W Mh','(',I2,')=',F8.2,' W Mt=',F8.2, ', W Et =',F7.2/
     &    ' MWJ=',F8.2,', M3J=',F8.2)
      NJ=NJETS
      IF(ETJ(4).GT.20..OR.ETJ(3).GT.25.) THEN
        IF(NJETS.GT.4) NJETS=4
        DO I=1,NJETS
          CALL PAIR_MASS(PWLEP,PJETS(1,I),PTOP1)
          DO K=1,4
            PTOP2(K)=0
          ENDDO
          DO J=1,NJETS
            IF(I.NE.J) THEN
              DO K=1,4
                PTOP2(K)=PTOP2(K)+PJETS(K,J)
              ENDDO
C
C         find a W -> hadrons
              DIFF_MIN=200.
              DIFF=DIFF_MIN+1.
              DO K=1,NJETS
                IF(K.NE.I.AND.K.NE.J) THEN
                  CALL PAIR_MASS(PJETS(1,K),PJETS(1,J),PWHAD)
                  DIFF=ABS(PWHAD(5)-80.0)
                  IF(DIFF.LT.DIFF_MIN) THEN
                    DIFF_MIN=DIFF
                    MWH=PWHAD(5)
                    IW=K*10+J
                  ENDIF
                ENDIF
              ENDDO
C
            ENDIF
          ENDDO
          PTOP2(5)=PTOP2(4)**2-PTOP2(1)**2-PTOP2(2)**2-PTOP2(3)**2
          IF(PTOP2(5).GT.0.) PTOP2(5)=SQRT(PTOP2(5))
          WRITE(DUNIT,112) IW,MWH,PTOP1(5),PTOP2(5)
  112     FORMAT(//' W Mh','(',I2,')=',F8.2,' Mtop 1=',F8.2,
     &        ', Mtop 2 =',F7.2/)
        ENDDO
      ENDIF
      WRITE(DUNIT,115) (ETJ(I),I=1,NJ)
  115 FORMAT(//' Jet ET corrected=',6F8.2)
      WRITE(DUNIT,116) ETEL,METC,SPHE,APLA
  116 FORMAT('Corr. el. ET=',F8.2,', Corrected missing ET=',F8.2,
     &  ', Sphericity=',F7.3, ', Aplanarity=',F7.3)
      GOTO 999
C
C
      ENTRY W_FIN
      W_FIN=.TRUE.
      IF(DO_DUMP) CALL EV_WRITE_LIST
  999 RETURN
      END
