CDECK  ID>, RESET.  
      SUBROUTINE RESET
C          RESET ALL USER DEFINED VARIABLES
      IMPLICIT NONE
      COMMON/ITAPES/ITDKY,ITEVT,ITCOM,ITLIS
      SAVE /ITAPES/
      INTEGER   ITDKY,ITEVT,ITCOM,ITLIS
      COMMON/QCDPAR/ALAM,ALAM2,CUTJET,ISTRUC
      SAVE /QCDPAR/
      INTEGER   ISTRUC
      REAL      ALAM,ALAM2,CUTJET
      COMMON/DYLIM/QMIN,QMAX,QTMIN,QTMAX,YWMIN,YWMAX,XWMIN,XWMAX,THWMIN,
     2  THWMAX,PHWMIN,PHWMAX
     3  ,SETLMQ(12)
      SAVE /DYLIM/
      LOGICAL SETLMQ
      EQUIVALENCE(BLIM1(1),QMIN)
      REAL      QMIN,QMAX,QTMIN,QTMAX,YWMIN,YWMAX,XWMIN,XWMAX,THWMIN,
     +          THWMAX,PHWMIN,PHWMAX,BLIM1(12)
      COMMON/FRGPAR/PUD,PBARY,SIGQT,PEND,XGEN(8),PSPIN1(8),
     $PMIX1(3,2),PMIX2(3,2),XGENSS(9)
      SAVE /FRGPAR/
      EQUIVALENCE (PMIX1(1,1),PMIXX1(1))
      EQUIVALENCE (PMIX2(1,1),PMIXX2(1))
      EQUIVALENCE(FRPAR(1),PUD)
      REAL      PUD,PBARY,SIGQT,PEND,XGEN,PSPIN1,PMIX1,PMIX2,XGENSS,
     +          PMIXX1(6),PMIXX2(6),FRPAR(32)
      COMMON/HCON/ANWWWW(4,4,4),ADWWWW(2,4),AIWWWW(4)
     $,HMASS,HGAM,HGAMS(29),ETAHGG,MATCHH(29),ZSTARS(4,2)
     $,IHTYPE,HGAMSS(85,85)
      SAVE /HCON/
      DOUBLE PRECISION ANWWWW,ADWWWW,AIWWWW
      INTEGER   MATCHH,IHTYPE
      REAL      HMASS,HGAM,HGAMS,ETAHGG,ZSTARS,HGAMSS
      COMMON/JETLIM/PMIN(3),PMAX(3),PTMIN(3),PTMAX(3),YJMIN(3),YJMAX(3)
     1 ,PHIMIN(3),PHIMAX(3),XJMIN(3),XJMAX(3),THMIN(3),THMAX(3)
     2 ,SETLMJ(36)
      SAVE /JETLIM/
      EQUIVALENCE(BLIMS(1),PMIN(1))
      LOGICAL SETLMJ
      COMMON/FIXPAR/FIXP(3),FIXPT(3),FIXYJ(3),FIXPHI(3),FIXXJ(3)
     2   ,FIXQM,FIXQT,FIXYW,FIXXW,FIXPHW
      LOGICAL FIXQM,FIXQT,FIXYW,FIXXW,FIXPHW
      LOGICAL FIXP,FIXPT,FIXYJ,FIXPHI,FIXXJ
      COMMON/SGNPAR/CTHS(2,3),THS(2,3),YJS(2,3),XJS(2,3)
      REAL      PMIN,PMAX,PTMIN,PTMAX,YJMIN,YJMAX,PHIMIN,PHIMAX,XJMIN,
     +          XJMAX,THMIN,THMAX,BLIMS(36),CTHS,THS,YJS,XJS
      COMMON/JETPAR/P(3),PT(3),YJ(3),PHI(3),XJ(3),TH(3),CTH(3),STH(3)
     1 ,JETTYP(3),SHAT,THAT,UHAT,QSQ,X1,X2,PBEAM(2)
     2 ,QMW,QW,QTW,YW,XW,THW,QTMW,PHIW,SHAT1,THAT1,UHAT1,JWTYP
     3 ,ALFQSQ,CTHW,STHW,Q0W
     4 ,INITYP(2),ISIGS,PBEAMS(5)
      SAVE /JETPAR/
      INTEGER   JETTYP,JWTYP,INITYP,ISIGS
      REAL      P,PT,YJ,PHI,XJ,TH,CTH,STH,SHAT,THAT,UHAT,QSQ,X1,X2,
     +          PBEAM,QMW,QW,QTW,YW,XW,THW,QTMW,PHIW,SHAT1,THAT1,UHAT1,
     +          ALFQSQ,CTHW,STHW,Q0W,PBEAMS
      COMMON/NODCAY/NODCAY,NOETA,NOPI0,NONUNU,NOEVOL,NOHADR,NOGRAV
      SAVE /NODCAY/
      LOGICAL NODCAY,NOETA,NOPI0,NONUNU,NOEVOL,NOHADR,NOGRAV
      COMMON/PRIMAR/NJET,SCM,HALFE,ECM,IDIN(2),NEVENT,NTRIES,NSIGMA
      SAVE /PRIMAR/
      INTEGER   NJET,IDIN,NEVENT,NTRIES,NSIGMA
      REAL      SCM,HALFE,ECM
      COMMON/QLMASS/AMLEP(100),NQLEP,NMES,NBARY
      SAVE /QLMASS/
      INTEGER   NQLEP,NMES,NBARY
      REAL      AMLEP
      INTEGER MXGOQ
      PARAMETER (MXGOQ=85)
      COMMON/Q1Q2/GOQ(MXGOQ,3),GOALL(3),GODY(4),STDDY,GOWW(25,2),
     $ALLWW(2),GOWMOD(25,3)
      SAVE /Q1Q2/
      LOGICAL GOQ,GOALL,GODY,STDDY,GOWW,ALLWW,GOWMOD
      COMMON/SEED/XSEED
      SAVE /SEED/
      CHARACTER*24 XSEED
C          SUSY parameters
C          AMGLSS               = gluino mass
C          AMULSS               = up-left squark mass
C          AMELSS               = left-selectron mass
C          AMERSS               = right-slepton mass
C          AMNiSS               = sneutrino mass for generation i
C          TWOM1                = Higgsino mass = - mu
C          RV2V1                = ratio v2/v1 of vev's
C          AMTLSS,AMTRSS        = left,right stop masses
C          AMT1SS,AMT2SS        = light,heavy stop masses
C          AMBLSS,AMBRSS        = left,right sbottom masses
C          AMB1SS,AMB2SS        = light,heavy sbottom masses
C          AMLLSS,AMLRSS        = left,right stau masses
C          AML1SS,AML2SS        = light,heavy stau masses
C          AMZiSS               = signed mass of Zi
C          ZMIXSS               = Zi mixing matrix
C          AMWiSS               = signed Wi mass
C          GAMMAL,GAMMAR        = Wi left, right mixing angles
C          AMHL,AMHH,AMHA       = neutral Higgs h0, H0, A0 masses
C          AMHC                 = charged Higgs H+ mass
C          ALFAH                = Higgs mixing angle
C          AAT                  = stop trilinear term
C          THETAT               = stop mixing angle
C          AAB                  = sbottom trilinear term
C          THETAB               = sbottom mixing angle
C          AAL                  = stau trilinear term
C          THETAL               = stau mixing angle
C          AMGVSS               = gravitino mass
      COMMON/SSPAR/AMGLSS,AMULSS,AMURSS,AMDLSS,AMDRSS,AMSLSS
     $,AMSRSS,AMCLSS,AMCRSS,AMBLSS,AMBRSS,AMB1SS,AMB2SS
     $,AMTLSS,AMTRSS,AMT1SS,AMT2SS,AMELSS,AMERSS,AMMLSS,AMMRSS
     $,AMLLSS,AMLRSS,AML1SS,AML2SS,AMN1SS,AMN2SS,AMN3SS
     $,TWOM1,RV2V1,AMZ1SS,AMZ2SS,AMZ3SS,AMZ4SS,ZMIXSS(4,4)
     $,AMW1SS,AMW2SS
     $,GAMMAL,GAMMAR,AMHL,AMHH,AMHA,AMHC,ALFAH,AAT,THETAT
     $,AAB,THETAB,AAL,THETAL,AMGVSS
      REAL AMGLSS,AMULSS,AMURSS,AMDLSS,AMDRSS,AMSLSS
     $,AMSRSS,AMCLSS,AMCRSS,AMBLSS,AMBRSS,AMB1SS,AMB2SS
     $,AMTLSS,AMTRSS,AMT1SS,AMT2SS,AMELSS,AMERSS,AMMLSS,AMMRSS
     $,AMLLSS,AMLRSS,AML1SS,AML2SS,AMN1SS,AMN2SS,AMN3SS
     $,TWOM1,RV2V1,AMZ1SS,AMZ2SS,AMZ3SS,AMZ4SS,ZMIXSS
     $,AMW1SS,AMW2SS
     $,GAMMAL,GAMMAR,AMHL,AMHH,AMHA,AMHC,ALFAH,AAT,THETAT
     $,AAB,THETAB,AAL,THETAL,AMGVSS
      REAL AMZISS(4)
      EQUIVALENCE (AMZISS(1),AMZ1SS)
      SAVE /SSPAR/
      COMMON/TCPAR/TCMRHO,TCGRHO
      SAVE /TCPAR/
      REAL TCMRHO,TCGRHO
      COMMON/TYPES/LOC(100),NTYP,NJTTYP(3),NWWTYP(2),NWMODE(3)
      COMMON/XTYPES/PARTYP(40),TITLE(10),JETYP(30,3),WWTYP(30,2)
     $,WMODES(30,3)
      SAVE /TYPES/,/XTYPES/
      CHARACTER*8 JETYP,WWTYP,TITLE,PARTYP,WMODES
      INTEGER   LOC,NTYP,NJTTYP,NWWTYP,NWMODE
      COMMON/WCON/SIN2W,WMASS(4),WGAM(4),AQ(12,4),BQ(12,4),COUT(4),
     1MATCH(25,4),WCBR(25,4),CUTOFF,CUTPOW,TBRWW(4,2),RBRWW(12,4,2),EZ,
     2AQDP(12,4),BQDP(12,4),EZDP,WFUDGE
      SAVE /WCON/
      DOUBLE PRECISION AQDP,BQDP,EZDP
      INTEGER   MATCH
      REAL      SIN2W,WMASS,WGAM,AQ,BQ,COUT,WCBR,CUTOFF,CUTPOW,TBRWW,
     +          RBRWW,EZ,WFUDGE
      COMMON/WCON2/CUMWBR(25,3)
      REAL CUMWBR
      INTEGER   MXFORC
      PARAMETER (MXFORC=40)
      COMMON/FORCE/NFORCE,IFORCE(MXFORC),MFORCE(5,MXFORC)
     $,LOOK2(2,MXFORC),LOOKST(MXFORC)
      SAVE /FORCE/
      INTEGER   NFORCE,IFORCE,MFORCE,LOOK2,LOOKST
      INTEGER   LIMPOM
      PARAMETER (LIMPOM=20)
      COMMON/MBGEN/POMWT(LIMPOM),POMGEN(LIMPOM),MNPOM,MXPOM,PDIFFR,
     $NPOM,XBARY(2),DXBARY(2),XPOM(LIMPOM,2)
      SAVE /MBGEN/
      INTEGER   MNPOM,MXPOM,NPOM
      REAL      POMWT,POMGEN,PDIFFR,XBARY,DXBARY,XPOM
      COMMON/ISLOOP/NEVOLV,NFRGMN,IEVOL,IFRG
      SAVE /ISLOOP/
      INTEGER NEVOLV,NFRGMN,IEVOL,IFRG
      COMMON /LIMEVL/ ETTHRS,CONCUT,USELIM
      SAVE /LIMEVL/
      REAL ETTHRS,CONCUT
      LOGICAL USELIM
      COMMON/XMSSM/GOMSSM,GOSUG,GOGMSB
     $,XGLSS,XMUSS,XHASS,XTBSS
     $,XQ1SS,XDRSS,XURSS,XL1SS,XERSS
     $,XQ2SS,XSRSS,XCRSS,XL2SS,XMRSS
     $,XQ3SS,XBRSS,XTRSS,XL3SS,XTARSS,XATSS,XABSS,XATASS
     $,XM1SS,XM2SS,XM0SU,XMHSU,XA0SU,XTGBSU,XSMUSU
     $,XLAMGM,XMESGM,XN5GM,XCMGV,XMGVTO
      SAVE /XMSSM/
      REAL XGLSS,XMUSS,XHASS,XTBSS
     $,XQ1SS,XDRSS,XURSS,XL1SS,XERSS
     $,XQ2SS,XSRSS,XCRSS,XL2SS,XMRSS
     $,XQ3SS,XBRSS,XTRSS,XL3SS,XTARSS,XATSS,XABSS,XATASS
     $,XM1SS,XM2SS
     $,XM0SU,XMHSU,XA0SU,XTGBSU,XSMUSU
     $,XLAMGM,XMESGM,XN5GM,XCMGV,XMGVTO
      LOGICAL GOMSSM,GOSUG,GOGMSB

      COMMON/EEPAR/SGMXEE,PLEP,PLEM
      SAVE /EEPAR/
      REAL      SGMXEE,PLEP,PLEM
C
      INTEGER I,I1,I2,I3,J1,INDEX,J,K
      REAL UNDEF,AMASS
      CHARACTER*8 BLANK
      DATA BLANK/'        '/
      DATA UNDEF/-1.E9/
C          RESET DYLIM
      DO 110 I=1,12
      BLIM1(I)=UNDEF
      SETLMQ(I)=.TRUE.
110   CONTINUE
C          RESET FRGPAR
      PUD=.43
      PBARY=.10
      SIGQT=.35
      PEND=.14
      XGEN(1)=.96
      XGEN(2)=3.
      XGEN(3)=0.
      XGEN(4)=.8
      XGEN(5)=.5
      XGEN(6)=.5
      XGEN(7)=.5
      XGEN(8)=.5
      DO 111 K=1,9
111   XGENSS(K)=.5
      PSPIN1(1)=.5
      PSPIN1(2)=.5
      PSPIN1(3)=.5
      PSPIN1(4)=.75
      PSPIN1(5)=.75
      PSPIN1(6)=.75
      PSPIN1(7)=.75
      PSPIN1(8)=.75
      PMIXX1(1)=.25
      PMIXX1(2)=.25
      PMIXX1(3)=.5
      PMIXX1(4)=0.
      PMIXX1(5)=.5
      PMIXX1(6)=1.
      PMIXX2(1)=.5
      PMIXX2(2)=.5
      PMIXX2(3)=1.
      PMIXX2(4)=0.
      PMIXX2(5)=0.
      PMIXX2(6)=1.
C          RESET ISLOOP
      NEVOLV=1
      NFRGMN=1
C          RESET JETLIM
      DO 120 I=1,36
      BLIMS(I)=UNDEF
      SETLMJ(I)=.TRUE.
120   CONTINUE
C          RESET NODCAY
      NODCAY=.FALSE.
      NOETA=.FALSE.
      NOPI0=.FALSE.
      NONUNU=.FALSE.
      NOEVOL=.FALSE.
      NOHADR=.FALSE.
      NOGRAV=.FALSE.
C          RESET PRIMAR
      IDIN(1)=1120
      IDIN(2)=-1120
      NTRIES=1000
      NSIGMA=20
C          RESET QCDPAR
      ALAM=.2
      ALAM2=ALAM**2
      CUTJET=6.
      ISTRUC=6
C          RESET QLMASS
      AMLEP(6)=175.
      AMLEP(7)=-1.
      AMLEP(8)=-1.
      DO 125 I=1,9
      CALL FLAVOR(80+I,I1,I2,I3,J1,INDEX)
125   AMLEP(INDEX)=0.
      CALL FLAVOR(29,I1,I2,I3,J1,INDEX)
      AMLEP(INDEX)=100.
      CALL FLAVOR(30,I1,I2,I3,J1,INDEX)
      AMLEP(INDEX)=0.
      CALL FLAVOR(39,I1,I2,I3,J1,INDEX)
      AMLEP(INDEX)=100.
      CALL FLAVOR(40,I1,I2,I3,J1,INDEX)
      AMLEP(INDEX)=100.
      DO 126 I=1,6
      CALL FLAVOR(20+I,I1,I2,I3,J1,INDEX)
      AMLEP(INDEX)=100.+AMASS(I)
      CALL FLAVOR(30+I,I1,I2,I3,J1,INDEX)
      AMLEP(INDEX)=100.+AMASS(I+10)
126   CONTINUE
C          RESET Q1Q2
      DO 130 I=1,MXGOQ
      DO 130 J=1,3
130   GOQ(I,J)=.TRUE.
      DO 131 I=1,3
131   GOALL(I)=.TRUE.
      GODY(1)=.TRUE.
      GODY(2)=.FALSE.
      GODY(3)=.FALSE.
      GODY(4)=.TRUE.
      DO 132 I=1,2
      ALLWW(I)=.TRUE.
      DO 132 J=1,25
132   GOWW(J,I)=.TRUE.
      DO 133 I=1,3
      DO 133 J=1,25
133   GOWMOD(J,I)=.TRUE.
C          RESET TCPAR
      TCMRHO=1000.
      TCGRHO=100.
C          RESET TYPES
      DO 140 I=1,NTYP
140   LOC(I)=0
      DO 141 I=1,3
      NJTTYP(I)=0
      JETYP(1,I)='ALL     '
      DO 141 K=2,25
141   JETYP(K,I)=BLANK
      JWTYP=4
      DO 142 I=1,2
      NWWTYP(I)=0
      WWTYP(1,I)='ALL     '
      DO 142 K=2,4
142   WWTYP(K,I)=BLANK
      DO 143 I=1,3
      NWMODE(I)=0
      WMODES(1,I)='ALL     '
      DO 143 K=2,25
143   WMODES(K,I)=BLANK
C          RESET WCON
      SIN2W=.232
      WMASS(2)=80.2
      WMASS(3)=WMASS(2)
      WMASS(4)=91.19
      CALL FLAVOR(80,I1,I2,I3,J,INDEX)
      AMLEP(INDEX)=WMASS(2)
      CALL FLAVOR(90,I1,I2,I3,J,INDEX)
      AMLEP(INDEX)=WMASS(4)
      CUTOFF=.200
      CUTPOW=1.0
      WFUDGE=1.75
C          RESET MBGEN
      MNPOM=1
      MXPOM=LIMPOM
C          RESET FORCE
      NFORCE=0
C
C          RESET QCD EVOLUTION CUTS
      USELIM=.FALSE.
      CONCUT=1.0
C
C          RESET SSPAR
      AMGVSS=1.E20
C
C          RESET XMSSM
      GOMSSM=.FALSE.
      GOSUG=.FALSE.
      XM1SS=1.E20
      XM2SS=1.E20
      XMGVTO=1.E20
C
C          RESET HCON
      IHTYPE=0
C
C          RESET EEPAR
      PLEP=0.
      PLEM=0.
C
      RETURN
      END
