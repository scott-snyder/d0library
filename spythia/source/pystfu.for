C*********************************************************************
 
      SUBROUTINE PYSTFU(KF,X,Q2,XPQ)
 
C...Gives electron, photon, pi+, neutron, proton and hyperon
C...structure functions according to a few different parametrizations.
C...Note that what is coded is x times the probability distribution,
C...i.e. xq(x,Q2) etc.
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      COMMON/PYINT1/MINT(400),VINT(400)
      SAVE /LUDAT1/,/LUDAT2/
      SAVE /PYPARS/,/PYINT1/
      DIMENSION XPQ(-25:25),XPEL(-25:25),XPGA(-6:6),XPPI(-6:6),
     &XPPR(-6:6)
 
C...Interface to PDFLIB.
      COMMON/W50513/XMIN,XMAX,Q2MIN,Q2MAX
      SAVE /W50513/
      DOUBLE PRECISION XX,QQ,UPV,DNV,USEA,DSEA,STR,CHM,BOT,TOP,GLU,
     &VALUE(20),XMIN,XMAX,Q2MIN,Q2MAX
      CHARACTER*20 PARM(20)
      DATA VALUE/20*0D0/,PARM/20*' '/
 
C...Reset structure functions.
      MINT(92)=0
      DO 100 KFL=-25,25
      XPQ(KFL)=0.
  100 CONTINUE
 
C...Check x and particle species.
      IF(X.LE.0..OR.X.GE.1.) THEN
        WRITE(MSTU(11),5000) X
        RETURN
      ENDIF
      KFA=IABS(KF)
      IF(KFA.NE.11.AND.KFA.NE.22.AND.KFA.NE.211.AND.KFA.NE.2112.AND.
     &KFA.NE.2212.AND.KFA.NE.3122.AND.KFA.NE.3112.AND.KFA.NE.3212
     &.AND.KFA.NE.3222.AND.KFA.NE.3312.AND.KFA.NE.3322.AND.
     &KFA.NE.3334.AND.KFA.NE.111) THEN
        WRITE(MSTU(11),5100) KF
        RETURN
      ENDIF
 
C...Electron structure function call.
      IF(KFA.EQ.11) THEN
        CALL PYSTEL(X,Q2,XPEL)
        DO 110 KFL=-25,25
        XPQ(KFL)=XPEL(KFL)
  110   CONTINUE
 
C...Photon structure function call (VDM+anomalous).
      ELSEIF(KFA.EQ.22.AND.MINT(109).LE.1) THEN
        IF(MSTP(56).EQ.1.AND.MSTP(55).EQ.1) THEN
          CALL PYSTGA(X,Q2,XPGA)
          DO 120 KFL=-6,6
          XPQ(KFL)=XPGA(KFL)
  120     CONTINUE
        ELSEIF(MSTP(56).EQ.2) THEN
C...Call PDFLIB structure functions.
          PARM(1)='NPTYPE'
          VALUE(1)=3
          PARM(2)='NGROUP'
          VALUE(2)=MSTP(55)/1000
          PARM(3)='NSET'
          VALUE(3)=MOD(MSTP(55),1000)
          IF(MINT(93).NE.3000000+MSTP(55)) THEN
            CALL PDFSET(PARM,VALUE)
            MINT(93)=3000000+MSTP(55)
          ENDIF
          XX=X
          QQ=SQRT(MAX(0.,SNGL(Q2MIN),Q2))
          IF(MSTP(57).EQ.0) QQ=SQRT(Q2MIN)
          CALL STRUCTM(XX,QQ,UPV,DNV,USEA,DSEA,STR,CHM,BOT,TOP,GLU)
          VINT(231)=Q2MIN
          XPQ(0)=GLU
          XPQ(1)=DNV
          XPQ(-1)=DNV
          XPQ(2)=UPV
          XPQ(-2)=UPV
          XPQ(3)=STR
          XPQ(-3)=STR
          XPQ(4)=CHM
          XPQ(-4)=CHM
          XPQ(5)=BOT
          XPQ(-5)=BOT
          XPQ(6)=TOP
          XPQ(-6)=TOP
        ELSE
          WRITE(MSTU(11),5200) KF,MSTP(56),MSTP(55)
        ENDIF
 
C...Pion/gammaVDM structure function call.
      ELSEIF(KFA.EQ.211.OR.KFA.EQ.111.OR.(KFA.EQ.22.AND.
     &MINT(109).EQ.2)) THEN
        IF(MSTP(54).EQ.1.AND.MSTP(53).GE.1.AND.MSTP(53).LE.3) THEN
          CALL PYSTPI(X,Q2,XPPI)
          DO 130 KFL=-6,6
          XPQ(KFL)=XPPI(KFL)
  130     CONTINUE
        ELSEIF(MSTP(54).EQ.2) THEN
C...Call PDFLIB structure functions.
          PARM(1)='NPTYPE'
          VALUE(1)=2
          PARM(2)='NGROUP'
          VALUE(2)=MSTP(53)/1000
          PARM(3)='NSET'
          VALUE(3)=MOD(MSTP(53),1000)
          IF(MINT(93).NE.2000000+MSTP(53)) THEN
            CALL PDFSET(PARM,VALUE)
            MINT(93)=2000000+MSTP(53)
          ENDIF
          XX=X
          QQ=SQRT(MAX(0.,SNGL(Q2MIN),Q2))
          IF(MSTP(57).EQ.0) QQ=SQRT(Q2MIN)
          CALL STRUCTM(XX,QQ,UPV,DNV,USEA,DSEA,STR,CHM,BOT,TOP,GLU)
          VINT(231)=Q2MIN
          XPQ(0)=GLU
          XPQ(1)=DSEA
          XPQ(-1)=UPV+DSEA
          XPQ(2)=UPV+USEA
          XPQ(-2)=USEA
          XPQ(3)=STR
          XPQ(-3)=STR
          XPQ(4)=CHM
          XPQ(-4)=CHM
          XPQ(5)=BOT
          XPQ(-5)=BOT
          XPQ(6)=TOP
          XPQ(-6)=TOP
        ELSE
          WRITE(MSTU(11),5200) KF,MSTP(54),MSTP(53)
        ENDIF
 
C...Anomalous photon structure function call.
      ELSEIF(KFA.EQ.22.AND.MINT(109).EQ.3) THEN
        IF(MSTP(56).NE.3) THEN
          CALL PYSTAG(X,Q2,XPGA)
        ELSE
          CALL PYSTHG(MSTP(55),X,Q2,PARP(15)**2,PARP(1),XPGA)
        ENDIF
        DO 140 KFL=-6,6
        XPQ(KFL)=XPGA(KFL)
  140   CONTINUE
 
 
C...Proton structure function call.
      ELSE
        IF(MSTP(52).EQ.1.AND.MSTP(51).GE.1.AND.MSTP(51).LE.11) THEN
          CALL PYSTPR(X,Q2,XPPR)
          DO 150 KFL=-6,6
          XPQ(KFL)=XPPR(KFL)
  150     CONTINUE
        ELSEIF(MSTP(52).EQ.2) THEN
C...Call PDFLIB structure functions.
          PARM(1)='NPTYPE'
          VALUE(1)=1
          PARM(2)='NGROUP'
          VALUE(2)=MSTP(51)/1000
          PARM(3)='NSET'
          VALUE(3)=MOD(MSTP(51),1000)
          IF(MINT(93).NE.1000000+MSTP(51)) THEN
            CALL PDFSET(PARM,VALUE)
            MINT(93)=1000000+MSTP(51)
          ENDIF
          XX=X
          QQ=SQRT(MAX(0.,SNGL(Q2MIN),Q2))
          IF(MSTP(57).EQ.0) QQ=SQRT(Q2MIN)
          CALL STRUCTM(XX,QQ,UPV,DNV,USEA,DSEA,STR,CHM,BOT,TOP,GLU)
          VINT(231)=Q2MIN
          XPQ(0)=GLU
          XPQ(1)=DNV+DSEA
          XPQ(-1)=DSEA
          XPQ(2)=UPV+USEA
          XPQ(-2)=USEA
          XPQ(3)=STR
          XPQ(-3)=STR
          XPQ(4)=CHM
          XPQ(-4)=CHM
          XPQ(5)=BOT
          XPQ(-5)=BOT
          XPQ(6)=TOP
          XPQ(-6)=TOP
        ELSE
          WRITE(MSTU(11),5200) KF,MSTP(52),MSTP(51)
        ENDIF
      ENDIF
 
C...Isospin average for pi0/gammaVDM.
      IF(KFA.EQ.111.OR.(KFA.EQ.22.AND.MINT(109).EQ.2)) THEN
        IF(KFA.EQ.22.AND.MINT(105).EQ.333) THEN
          XPS=0.5*(XPQ(1)+XPQ(-2))
          XPV=0.5*(XPQ(2)+XPQ(-1))-XPS
          XPQ(2)=XPS
          XPQ(-1)=XPS
          XPQ(3)=XPQ(3)+XPV
          XPQ(-3)=XPQ(-3)+XPV
        ELSEIF(KFA.EQ.22.AND.MINT(105).EQ.443) THEN
          XPS=0.5*(XPQ(1)+XPQ(-2))
          XPV=0.5*(XPQ(2)+XPQ(-1))-XPS
          XPQ(2)=XPS
          XPQ(-1)=XPS
          XPQ(4)=XPQ(4)+XPV
          XPQ(-4)=XPQ(-4)+XPV
        ELSE
          XPS=(XPQ(1)+XPQ(2)+XPQ(-1)+XPQ(-2))/4.
          XPQ(1)=XPS
          XPQ(2)=XPS
          XPQ(-1)=XPS
          XPQ(-2)=XPS
        ENDIF
 
C...Rescale for gammaVDM by effective gamma -> rho coupling.
        IF(KFA.EQ.22.AND.MINT(109).EQ.2) THEN
          DO 160 KFL=-6,6
          XPQ(KFL)=VINT(281)*XPQ(KFL)
  160     CONTINUE
        ENDIF
 
C...Isospin conjugation for neutron.
      ELSEIF(KFA.EQ.2112) THEN
        XPS=XPQ(1)
        XPQ(1)=XPQ(2)
        XPQ(2)=XPS
        XPS=XPQ(-1)
        XPQ(-1)=XPQ(-2)
        XPQ(-2)=XPS
 
C...Simple recipes for hyperon (average valence structure function).
      ELSEIF(KFA.EQ.3122.OR.KFA.EQ.3112.OR.KFA.EQ.3212.OR.KFA.EQ.3222
     &.OR.KFA.EQ.3312.OR.KFA.EQ.3322.OR.KFA.EQ.3334) THEN
        XPVAL=(XPQ(1)+XPQ(2)-XPQ(-1)-XPQ(-2))/3.
        XPSEA=0.5*(XPQ(-1)+XPQ(-2))
        XPQ(1)=XPSEA
        XPQ(2)=XPSEA
        XPQ(-1)=XPSEA
        XPQ(-2)=XPSEA
        XPQ(KFA/1000)=XPQ(KFA/1000)+XPVAL
        XPQ(MOD(KFA/100,10))=XPQ(MOD(KFA/100,10))+XPVAL
        XPQ(MOD(KFA/10,10))=XPQ(MOD(KFA/10,10))+XPVAL
      ENDIF
 
C...Charge conjugation for antiparticle.
      IF(KF.LT.0) THEN
        DO 170 KFL=1,25
        IF(KFL.EQ.21.OR.KFL.EQ.22.OR.KFL.EQ.23.OR.KFL.EQ.25) GOTO 170
        XPS=XPQ(KFL)
        XPQ(KFL)=XPQ(-KFL)
        XPQ(-KFL)=XPS
  170   CONTINUE
      ENDIF
 
C...Allow gluon also in position 21.
      XPQ(21)=XPQ(0)
 
C...Check positivity and reset above maximum allowed flavour.
      DO 180 KFL=-25,25
      XPQ(KFL)=MAX(0.,XPQ(KFL))
      IF(IABS(KFL).GT.MSTP(58).AND.IABS(KFL).LE.8) XPQ(KFL)=0.
  180 CONTINUE
 
C...Formats for error printouts.
 5000 FORMAT(' Error: x value outside physical range; x =',1P,E12.3)
 5100 FORMAT(' Error: illegal particle code for structure function;',
     &' KF =',I5)
 5200 FORMAT(' Error: unknown structure function; KF, library, set =',
     &3I5)
 
      RETURN
      END
