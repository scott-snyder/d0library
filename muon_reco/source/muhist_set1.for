      SUBROUTINE MUHIST_SET1(IHOFF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : MUON_RECO histograms, set1.
C-   more detailed histograms. has MUD1 stuff
C-      Histograms for MUOT and PMUO tracks with comparison
C       with ISAJET muon tracks. ID=100-199
C-
C-   Inputs  : IHOFF    I    offset for histogram ID.
C-   Outputs :
C-   Controls:
C-
C-   Original    MAR-1990   D. Hedin
C-   Created  28-MAR-1990   Shuichi Kunori
C    DH 1-91 CHANGE MUOT; DH 11/91 major update; DH 12/91 CD tracks
C    DH 3/92 add histos; some bin changes; add MUOH loop
C    DH 6/92 add MTRG calls
C    DH 8/92 fix plane counting bug
C    PQ 10/92 fix npln index bug
C    DD 11-MAY-1993 Add SAMUS histograms
C    DW  1-DEC-1993 update raw hit loop to accomodate new unpacking
C    DH 1/94 fix indexing problem in EFF code. change selection
C    DD 18-APR-1994  Correct SAMUS quadrants - 13 and 14 only
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IHOFF
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMUCD.LINK'
      INTEGER IDA,IJ,SI,J,LPMUO,GZPMUO,LMUCD,LMUOT,MMOD
      PARAMETER (SI=100)
      INTEGER NPART,ID(SI),ID2(SI),GZMFIT,LMFIT,NMT,IMT(20)
      REAL PMC,DCXMC,DCYMC,DCZMC,DOT,DOTMAX,THETA,RD
      REAL X(SI),Y(SI),Z(SI),PX(SI),PY(SI),PZ(SI),P(SI),
     A     PHI(SI),TH(SI),ETA(SI),UA,VA,WA,PA,UB,VB,WB,PB
      REAL DETA,ET,DR,DET,DTHETA,DPHI,THMC,A,PT,DELP,THETD,PHID
      INTEGER NMURAW,NMUPROC,NMODH,LPMUOF(460),I,IWADD,IEPHA,IEPHB,
     A  IDR1,IDR2,IOPHA,IOPHB,IDELT1,IDELT2,NTRAKS,ITRAK,
     A  NPTRAK,JJ,IFW1,IFW2,NMOD,NPLN,NWIR,IERR,IOFF(10)
      INTEGER IHIT,IHMUOH,ITSIGN,IDELT,IPAD,NON,ION(10),NOFF
      REAL XI,YI,ZI,XMAGC,YMAGC,ZMAGC,XCOSIM,YCOSIM,ZCOSIM,XCOSOM,
     A  YCOSOM,ZCOSOM,CHSQBV,CHSNBV,RMOM,RMOMER,
     A  EPHA,EPHB,OPHA,OPHB,ESUM,OSUM,DR1,
     A  DR2,DELT1,DELT2,PRES,PPMUO
      INTEGER JHIT,IFWH1,IFWH2,INRAW,IORIEN,NHWIR,IM,NP(4),NP2(4)
      REAL CORT1,CORT2,CORP1,CORP2,CORDT1,PN,PN2,PN3,
     XCORDT2,DDIS1,DDIS2,TDIV1,TDIV2,VERD1,VERD2,XCWIR,YCWIR,ZCWIR
      INTEGER IMOD,IFW3,NSAMUS,IFW4,NS,MUNMOD2,NMODU,MODNUM
      REAL ELCAL,ELFE,BDL,CNBT
      REAL    GMT,GP1,GP2
      LOGICAL FIRST
      INTEGER NMODT,JMODT(6),LMUHT,GZMUHT,LMUOF,GZMUOF,IMUOF,
     A  NAMUD1,NAMUOH,NP3(4)
      INTEGER NSAM
      INTEGER NSRAW,NSPRO,NSMOD,NMUHP,NMUOF,NMSCT,NVERS
      INTEGER NRAW,IMUD1,NCEL,LAT,IADC(8),IDUM
      DATA NMODT/6/
      DATA JMODT/11,21,31,12,22,32/
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C  Book histograms for the first call.
C
      IF(FIRST) THEN
        FIRST=.FALSE.
C
        CALL HBOOK1(IHOFF+1,' IFW4,IFW1:TH>20',20,0.,20.,0.)
        CALL HBOOK1(IHOFF+2,' IFW4,IFW1:10<TH<20',20,0.,20.,0.)
        CALL HBOOK1(IHOFF+4,'=MUOT ALL= PHI '  ,18,0.,360.,0.0)
        CALL HBOOK2(IHOFF+5,'=MUOT GOOD= THETA VS PHI '
     +                      ,18,0.,180.,18,0.,360.,0.0)
        CALL HBOOK1(IHOFF+8,'=MUOT GOOD= MOMENTUM ',20,0.,100.,0.0)
        CALL HBOOK1(IHOFF+9,'=MUOT ALL= NO. TRACKS/EVENT ',20,-.5,19.5
     A    ,0.0)
        CALL HBOOK1(IHOFF+10,'=MUOT GOOD= NO. TRACKS/EVENT ',20,-.5
     A    ,19.5,0.0)
        CALL HBOOK1(IHOFF+11,' PMUO / EVENT ',20,-.5
     A    ,19.5,0.0)
        CALL HBOOK1(IHOFF+12,' PMUO passing MTC / EVENT ',
     A    20,-.5,19.5,0.0)
        CALL HBOOK1(IHOFF+16,'MUOT RES BEND ',40,0.,4.,0.0)
        CALL HBOOK1(IHOFF+17,'MUOT RES NONBEND ',40,0.,10.,0.0)
        CALL HBOOK1(IHOFF+18,'MUOT RES NONBEND ',40,0.,400.,0.0)
        CALL HBOOK1(IHOFF+19,'MUOT RES DELTAT ',40,0.,400.,0.0)
        CALL HBOOK1(IHOFF+20,'MUOTGD RES BEND ',40,0.,4.,0.0)
        CALL HBOOK1(IHOFF+21,'MUOTGD RES NONBEND ',40,0.,200.,0.0)
        CALL HBOOK1(IHOFF+24,' NO. WAMUS HITS/TRACK',31,-.5,30.5,0.)
        CALL HBOOK1(IHOFF+25,' PLANES/MOD A-B-C ',15,0.,15.,0.)
        CALL HBOOK1(IHOFF+26,' PLANES/MOD (DT) A-B-C ',15,0.,15.,0.)
        CALL HBOOK1(IHOFF+27,' PLANES/MOD (PADS) A-B-C ',15,0.,15.,0.)
CC   MUOH histograms
        CALL HBOOK1(IHOFF+70,' # top/cen A MUD1 ',40,0.,100.,0.)
        CALL HBOOK1(IHOFF+71,' # top/cen A MUOH ',40,0.,100.,0.)
        CALL HBOOK1(IHOFF+80,' MUOH THETA A-LAYER',36,0.,180.,0.)
        CALL HBOOK1(IHOFF+81,' MUOH THETA B-LAYER',36,0.,180.,0.)
        CALL HBOOK1(IHOFF+82,' MUOH THETA C-LAYER',36,0.,180.,0.)
CC   MUD1 histograms
        CALL HBOOK1(IHOFF+85,' NO. MUD1 HITS/EVENT ',50,0.,1000.,0.)
        CALL HBOOK1(IHOFF+86,' NO. MUOH HITS/EVENT ',50,0.,1000.,0.)
        CALL HBOOK1(IHOFF+87,' NO. MODULES/EVENT ',30,0.,150.,0.)
        CALL HBOOK1(IHOFF+90,' MUD1 HITS VS MOD ',164,.5,164.5,0.)
        CALL HBOOK1(IHOFF+91,' MUOH HITS ',164,.5,164.5,0.)
        CALL HBOOK1(IHOFF+92,' MUOH T2 ',164,.5,164.5,0.)
        CALL HBOOK1(IHOFF+93,'=MUD1= PAD  ',41,0.,4100.,0.)
        CALL HBOOK1(IHOFF+94,'=MUD1= PAD A+B ',41,0.,8200.,0.)
        CALL HBOOK1(IHOFF+95,'=MUD1= TIME 1',41,0.,4100.,0.)
        CALL HBOOK1(IHOFF+96,'=MUD1= TIME 2',41,0.,4100.,0.)
        CALL HBOOK1(IHOFF+97,'=MUD1= DELTA TIME:EVEN$',41,0.,4100.,0.)
        CALL HBOOK1(IHOFF+98,'=MUD1= DELTA TIME:ODD$',41,0.,4100.,0.)
CCCC   ISAJET HISTOGRAMS
        CALL HBOOK1(IHOFF+100,'=MUOT= MOMENTUM RESOLUTION ',40,-1.,1.,
     &    0.)
        CALL HBOOK1(IHOFF+101,'=MUOT= MOM. RES. 15<T<35 ',40,-1.,1.,0.)
        CALL HBOOK1(IHOFF+102,'=MUOT= MOM. RES. 35<T<50 ',40,-1.,1.,0.)
        CALL HBOOK1(IHOFF+103,'=MUOT= MOM. RES. 50<T<90 ',40,-1.,1.,0.)
        CALL HBOOK1(IHOFF+104,' ISAL THETA MUON  ',18,0.,180.,0.)
        CALL HBOOK1(IHOFF+105,' ISAL PHI MUON ',18,0.,360.,0.)
        CALL HBOOK1(IHOFF+106,' MUOT THETA NO ISA MU  ',18,0.,180.,0.)
        CALL HBOOK1(IHOFF+107,' MUOT GOOD THETA NO ISA MU  ',
     A    18,0.,180.,0.)
CC    PMUO histograms
        CALL HBOOK1(IHOFF+120,' PMUO:DPHI ',20,-.1,.1,0.)
        CALL HBOOK1(IHOFF+121,' PMUO:DTHETA ',20,-.1,.1,0.)
        CALL HBOOK1(IHOFF+122,' PMUO:DET ',20,-20.,20.,0.)
        CALL HBOOK1(IHOFF+123,' PMUO:DE  ',20,-20.,20.,0.)
        CALL HBOOK1(IHOFF+124,' PMUO:DE/E  ',20,-1.,1.,0.)
        CALL HBOOK1(IHOFF+125,' PMUO:DE/E 10 ',20,-1.,1.,0.)
        CALL HBOOK1(IHOFF+126,' PMUO:DE/E 20 ',20,-1.,1.,0.)
        CALL HBOOK1(IHOFF+127,' PMUO:DE/E 50 ',20,-1.,1.,0.)
        CALL HBOOK1(IHOFF+129,' PM:MOMENTUM   ',20,0.,100.,0.)
        CALL HBOOK1(IHOFF+130,' PM:ETA   ',30,-3.,3.,0.)
        CALL HBOOK1(IHOFF+131,' PM:PT ',20,0.,100.,0.)
        CALL HBOOK1(IHOFF+132,' PMUO:THETA:NO ISA MATCH ',18,0.,180.,0.)
        CALL HBOOK1(IHOFF+133,' THETA NO CD TRACK ',18,0.,180.,0.)
        CALL HBOOK1(IHOFF+134,' PMUO T0  ',100,-200.,200.,0.)
        CALL HBOOK1(IHOFF+135,' PMUO GLOB FIT CHISQ  ',100,0.,100.,0.)
        CALL HBOOK1(IHOFF+136,' ET:.4 ',20,0.,40.,0.)
        CALL HBOOK1(IHOFF+138,' ISO PARA ',20,-5.,15.,0.)
        CALL HBOOK1(IHOFF+139,' NO. CD TRACKS ',10,-.5,9.5,0.)
        CALL HBOOK1(IHOFF+140,' (PMUO-MUOT)/PMUO E ',50,-1.,1.,0.)
        CALL HBOOK1(IHOFF+150,' SAMUS MUOT MOMENTUM',100,-100.,100.,0.)
        CALL HBOOK1(IHOFF+152,' NUMBER OF SAMUS TRACKS',10,0.,10.,0.)
        CALL HBOOK1(IHOFF+153,' SAMUS HITS ON TRACK NORTH',30,0.,30.,0.)
        CALL HBOOK1(IHOFF+154,' SAMUS HITS ON TRACK SOUTH',30,0.,30.,0.)
        CALL HBOOK1(IHOFF+201,' MTC TRES',50,0.,10.,0.)
        CALL HBOOK1(IHOFF+202,' MTC TRESV',50,0.,10.,0.)
        CALL HBOOK1(IHOFF+203,' MTC FRACT', 100,0.005,1.005,0.)
        CALL HBOOK1(IHOFF+204,' MTC HFRACT', 100,0.005,1.005,0.)
        CALL HBOOK1(IHOFF+205,' MTC GHFRACT', 100,0.005,1.005,0.)
        CALL HBOOK1(IHOFF+206,' MTC ECHI', 50,0.,10.,0.)
        CALL HBOOK1(IHOFF+207,' MTC EN3', 50,0.,10.,0.)
        CALL HBOOK1(IHOFF+208,' MTC EFRACTH(1)', 100,0.005,1.005,0.)
        CALL HBOOK1(IHOFF+209,' MTC LYRMU', 20,0.,20.,0.)
        CALL HBOOK1(IHOFF+210,' MTC ECHI2', 50,0.,10.,0.)
CC EFF HISTOGRAMS
        NMODU=164/12+1
        DO I=1,NMODU
          CALL HBOOK1(IHOFF+399+I,' EFF HISTOGRAMS ',60,0.,60.,0.)
          CALL HBOOK1(IHOFF+419+I,' EFF HISTOGRAMS ',60,0.,60.,0.)
          CALL HBOOK1(IHOFF+439+I,' EFF HISTOGRAMS ',60,0.,60.,0.)
        ENDDO
        CALL HBOOK1(IHOFF+416,' MODULES ON TRACKS ',164,.5,164.5,0.)
        CALL HBOOK1(IHOFF+417,' MODULES OFF TRACKS ',164,.5,164.5,0.)
CCC   TRIGGER HISTOGRAMS
C        CALL BOOK_MTRG_HISTS(IHOFF+600)
      ENDIF
C      CALL FILL_MTRG_HISTS(IHOFF+600)
C
      CALL GISAMU(NPART,ID,ID2,PX,PY,PZ,P,PHI,TH,ETA,X,Y,Z)
C
      DO I=1,NPART
        IF(P(I).GT.4.) THEN
          CALL HFILL(IHOFF+104,TH(I)*180./3.14159,0.,1.)
          CALL HFILL(IHOFF+105,PHI(I)*180./3.14159,0.,1.)
        ENDIF
      ENDDO
C
      CALL GTMTRH(NTRAKS)      ! SEE HOW MANY TRACKS
      GMT=0.
      NSAM=0
      IF(NTRAKS.GT.0) THEN
        DO ITRAK=1,NTRAKS
          CALL GTMUOT(ITRAK,NPTRAK,NSAMUS,JJ,IFW1,IFW2,IFW3,IFW4,XI,
     X      YI,ZI,XMAGC,YMAGC,ZMAGC,XCOSIM,YCOSIM,ZCOSIM,XCOSOM,YCOSOM,
     X      ZCOSOM,CHSQBV,CHSNBV,RMOM,RMOMER,ELCAL,ELFE,BDL,CNBT)
          IF(JJ.GE.13.AND.JJ.LE.14) CALL HFILL(IHOFF+150,RMOM,0.,1.) ! SAMUS
          IF(JJ.GE.13.AND.JJ.LE.14) NSAM=NSAM+1                      ! SAMUS
          IF(JJ.EQ.13) CALL HFILL(IHOFF+153,FLOAT(NSAMUS),0.,1.)
          IF(JJ.EQ.14) CALL HFILL(IHOFF+154,FLOAT(NSAMUS),0.,1.)
CCC   LOOK AT WAMUS TRACKS
          IF(NPTRAK+NSAMUS.GT.0.AND.MOD(JJ,100).LE.12) THEN
            THETD=ACOS(ZCOSIM)*180./3.14159
            PHID=ATAN2(YCOSIM,XCOSIM)*180./3.14159
            IF(PHID.LT.0.) PHID=PHID+360.
            CALL HFILL(IHOFF+16,CHSQBV,0.,1.)
            CALL HFILL(IHOFF+17,CHSNBV,0.,1.)
            CALL HFILL(IHOFF+18,CHSNBV,0.,1.)
            CALL HFILL(IHOFF+19,CNBT,0.,1.)
            CALL HFILL(IHOFF+4,PHID,0.,1.)
            IF(THETD.GT.20..AND.THETD.LE.160.) THEN
              CALL HFILL(IHOFF+1,FLOAT(IFW4),0.,1.)
              CALL HFILL(IHOFF+1,FLOAT(10+IFW1),0.,1.)
            ELSE IF(THETD.LT.20..OR.THETD.GE.160.) THEN
              CALL HFILL(IHOFF+2,FLOAT(IFW4),0.,1.)
              CALL HFILL(IHOFF+2,FLOAT(10+IFW1),0.,1.)
            ENDIF
CCC  LOOK AT MODULES ON/OFF TRACK
            IF(IFW4.LE.2) THEN
              CALL HFILL(IHOFF+24,FLOAT(NPTRAK),0.,1.)
              CALL MUMISS(ITRAK,NON,ION,NOFF,IOFF)
              DO I=1,NON
                IMOD=MUNMOD2(2,ION(I))
                CALL HFILL(IHOFF+416,FLOAT(IMOD),0.,1.)
              ENDDO
              DO I=1,NOFF
                IMOD=MUNMOD2(2,IOFF(I))
                CALL HFILL(IHOFF+417,FLOAT(IMOD),0.,1.)
              ENDDO
            ENDIF
CCC  LOOK AT HITS ON TRACKS
            NMT=0
            DO IHIT=1,NPTRAK
              CALL GTMHTT(ITRAK,IHIT,IWADD,IHMUOH,ITSIGN,IDELT,IPAD)
              CALL MUADD(IWADD,NMOD,NPLN,NWIR,IERR)
              IF(NMT.NE.0) THEN
                DO IM=1,NMT
                  IF(NMOD.EQ.IMT(IM)) GO TO 432
                ENDDO
              ENDIF
              NMT=NMT+1
              IMT(NMT)=NMOD
CCC      NEW MODULE; NOW COUNT NUMBER OF PLANES HIT
              DO I=1,4
                NP(I)=0
                NP2(I)=0
                NP3(I)=0
              ENDDO
              DO IM=1,NPTRAK
                CALL GTMHTT(ITRAK,IM,IWADD,IHMUOH,ITSIGN,IDELT,IPAD)
                CALL MUADD(IWADD,MMOD,NPLN,NWIR,IERR)
                IF(NMOD.EQ.MMOD) THEN    ! IN NEW MODULE
                  NP(NPLN+1)=1     !NPLN GOES FROM 0 TO 3
C                IF(ITSIGN.NE.0) NP(NPLN+1)=1     !NPLN GOES FROM 0 TO 3
                  IF(IDELT.NE.0)  NP2(NPLN+1)=1
                  IF(IPAD.NE.0)   NP3(NPLN+1)=1
                ENDIF
              ENDDO
              PN=NP(1)+NP(2)+NP(3)+NP(4)
              PN2=NP2(1)+NP2(2)+NP2(3)+NP2(4)
              PN3=NP3(1)+NP3(2)+NP3(3)+NP3(4)
CC   REQUIRE SOME HITS
              IF(PN.GT.0.AND.PN2.GT.0.AND.PN3.GT.0) THEN
                IMOD=MUNMOD2(2,NMOD)
                I=(IMOD-1)/12
                RD=(IMOD-12*I-1)*5
                CALL HFILL(IHOFF+400+I,PN+RD,0.,1.)
                CALL HFILL(IHOFF+420+I,PN2+RD,0.,1.)
                CALL HFILL(IHOFF+440+I,PN3+RD,0.,1.)
                IF(NMOD.GE.10.AND.NMOD.LE.99) THEN
                  CALL HFILL(IHOFF+25,PN,0.,1.)
                  CALL HFILL(IHOFF+26,PN2,0.,1.)
                  CALL HFILL(IHOFF+27,PN3,0.,1.)
                ELSE IF(NMOD.GE.100.AND.NMOD.LE.199) THEN
                  CALL HFILL(IHOFF+25,PN+5.,0.,1.)
                  CALL HFILL(IHOFF+26,PN2+5.,0.,1.)
                  CALL HFILL(IHOFF+27,PN3+5.,0.,1.)
                ELSE IF(NMOD.GE.200) THEN
                  CALL HFILL(IHOFF+25,PN+10.,0.,1.)
                  CALL HFILL(IHOFF+26,PN2+10.,0.,1.)
                  CALL HFILL(IHOFF+27,PN3+10.,0.,1.)
                ENDIF
              ENDIF
  432         CONTINUE
            ENDDO
CCCCC   LOOK AT ISAJET INFO
            CALL MUISAL(XCOSIM,YCOSIM,ZCOSIM,PMC,DCXMC,DCYMC,DCZMC)
            IF (IFW4.LE.1) THEN
              CALL HFILL(IHOFF+20,CHSQBV,0.,1.)
              CALL HFILL(IHOFF+21,CHSNBV,0.,1.)
              GMT=GMT+1.
              CALL HFILL(IHOFF+8,ABS(RMOM),0.,1.)
              CALL HFILL(IHOFF+5,THETD,PHID,1.)
            ENDIF
            IF(CHSQBV.LT.100.) THEN
              CALL MUISP1(XCOSIM,YCOSIM,ZCOSIM,PMC,DCXMC,DCYMC,DCZMC,
     &          IDA)
              IF(DCXMC.LE.10.) THEN
                DOT=XCOSIM*DCXMC+YCOSIM*DCYMC+ZCOSIM*DCZMC
              ELSE
                DOT=-99999.           ! NO MUON
              ENDIF
              IF(DOT.GT..95) THEN      ! ISAJET MUON
                IF(IFW4.LE.1) THEN
                  PRES=(1./RMOM-1./PMC)*PMC
                  CALL HFILL(IHOFF+100,PRES,0.,1.)
                  IF(THETD.GT.15..AND.THETD.LT.35.)
     +              CALL HFILL(IHOFF+101,PRES,0.,1.)
                  IF(THETD.GT.145..AND.THETD.LT.165.)
     +              CALL HFILL(IHOFF+101,PRES,0.,1.)
                  IF(THETD.GT.35..AND.THETD.LT.50.)
     +              CALL HFILL(IHOFF+102,PRES,0.,1.)
                  IF(THETD.GT.165..AND.THETD.LT.130.)
     +              CALL HFILL(IHOFF+102,PRES,0.,1.)
                  IF(THETD.GT.50..AND.THETD.LT.130.)
     +              CALL HFILL(IHOFF+103,PRES,0.,1.)
                ENDIF
              ELSE
CCC    NO ISAJET MUON MATCHING MUOT TRACKS
                CALL HFILL(IHOFF+106,THETD,0.,1.)
                IF(IFW4.LE.1) CALL HFILL(IHOFF+107,THETD,0.,1.)
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDIF
      CALL HFILL(IHOFF+9,FLOAT(NTRAKS),0.,1.)
      CALL HFILL(IHOFF+10,GMT,0.,1.)
      CALL HFILL(IHOFF+152,FLOAT(NSAM),0.,1.)
C  ====================
C  HISTOGRAMS FROM MUD1
C  GET THE NUMBER OF RAW HITS FROM ZEBRA BANKS - NMURAW
C  =======================================================
C
C...  WAMUS HITS ANALYSIS ... should work for either 1a or 1b format
C
      CALL GTMUHT(NMURAW,NMUPROC,NMODH,NSRAW,NSPRO,NSMOD,
     &                  NMUHP,NMUOF,NMSCT,NVERS,LPMUOF)
      CALL HFILL(IHOFF+85,FLOAT(NMURAW),0.,1.)
      CALL HFILL(IHOFF+86,FLOAT(NMUPROC),0.,1.)
      CALL HFILL(IHOFF+87,FLOAT(NMODH),0.,1.)
C
C       Initialize utilities
      CALL MUDMOD(0,NRAW,JHIT,IMUD1)
      CALL MUDHIT(0,JHIT,NCEL,LAT,IADC)
C
C loop over WAMUS modules
      DO NMOD=10,307
        IF(LPMUOF(NMOD).GT.0) THEN
          CALL MUDMOD(NMOD,NRAW,JHIT,IMUD1)
C loop over raw hits in the module
          DO IDUM=1,NRAW
            IHIT = JHIT
            IF(IHIT.GT.0) THEN
              IMOD=MUNMOD2(2,NMOD)
              IF(IMOD.LE.164.AND.IMOD.GE.1) THEN
                CALL HFILL(IHOFF+90,FLOAT(IMOD),0.,1.)
              ENDIF
              CALL MUDHIT(IHIT,JHIT,NCEL,LAT,IADC)
              EPHA=FLOAT(IADC(3))
              EPHB=FLOAT(IADC(4))
              OPHA=FLOAT(IADC(7))
              OPHB=FLOAT(IADC(8))
              DR1= FLOAT(IADC(1))
              DR2= FLOAT(IADC(2))
              DELT1=FLOAT(IADC(6))
              DELT2=FLOAT(IADC(5))
              ESUM = EPHA + EPHB
              CALL HFILL(IHOFF+93,EPHA,0.,1.)
              CALL HFILL(IHOFF+93,EPHB,0.,1.)
              CALL HFILL(IHOFF+94,ESUM,0.,1.)
              OSUM = OPHA + OPHB
              CALL HFILL(IHOFF+93,OPHA,0.,1.)
              CALL HFILL(IHOFF+93,OPHB,0.,1.)
              CALL HFILL(IHOFF+94,OSUM,0.,1.)
              CALL HFILL(IHOFF+95,DR1,0.,1.)
              CALL HFILL(IHOFF+96,DR2,0.,1.)
              IF(ESUM.GT.OSUM.AND.ESUM.GT.800.) THEN
                CALL HFILL(IHOFF+97,DELT1,0.,1.)
              ENDIF
              IF(OSUM.GT.ESUM.AND.OSUM.GT.800.) THEN
                CALL HFILL(IHOFF+98,DELT1,0.,1.)
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C
      LMUOF=GZMUOF(0)
      LMUHT=GZMUHT(0)
      NAMUD1=0
      NAMUOH=0
      DO I=1,NMODT      ! LOOP OVER HIT MODULES
        IMUOF=IQ(LMUHT+10+JMODT(I))
        IF(IMUOF.GT.0) THEN
          IMUOF=10*(IMUOF-1)+LMUOF
          NAMUD1=NAMUD1+IQ(IMUOF+2)
          NAMUOH=NAMUOH+IQ(IMUOF+4)
        ENDIF
      ENDDO
      CALL HFILL(IHOFF+70,FLOAT(NAMUD1),0.,1.)
      CALL HFILL(IHOFF+71,FLOAT(NAMUOH),0.,1.)
CCC   loop over MUOH bank
      DO JHIT=1,NMUPROC
        CALL GTMUOH(JHIT,IWADD,IFWH1,IFWH2,INRAW,IORIEN,NHWIR,
     X                  CORT1,CORT2,CORP1,CORP2,CORDT1,CORDT2,DDIS1,
     X                  DDIS2,TDIV1,TDIV2,VERD1,VERD2,XCWIR,YCWIR,ZCWIR)
        CALL MUADD(IWADD,NMOD,NPLN,NWIR,IERR)
        THETA=ZCWIR/SQRT(XCWIR**2+YCWIR**2+ZCWIR**2)
        THETA=ACOS(THETA)*180./3.14159
        IF(NMOD.LE.99) CALL HFILL(IHOFF+80,THETA,0.,1.)
        IF(NMOD.GE.100.AND.NMOD.LE.199) CALL HFILL(IHOFF+81,THETA,0.,1.)
        IF(NMOD.GE.200) CALL HFILL(IHOFF+82,THETA,0.,1.)
        IMOD=MUNMOD2(2,NMOD)
        CALL HFILL(IHOFF+91,FLOAT(IMOD),0.,1.)
        IF(NHWIR.EQ.2) CALL HFILL(IHOFF+92,FLOAT(IMOD),0.,1.)
      ENDDO
CCC  loop over PMUO bank
      GP1=0.
      GP2=0.
      LPMUO=GZPMUO(0)
      IF(LPMUO.NE.0) THEN
        DO WHILE (LPMUO.GT.0)
          GP1=GP1+1.
C tracks passing MTC
          IF (IQ(LPMUO+54).GT.0) THEN
            GP2=GP2+1.
          ENDIF
          A=Q(LPMUO+14)/(Q(LPMUO+13)+.001)        ! SIN(THETA)
          PT=Q(LPMUO+14)+.001
          THETD=Q(LPMUO+15)*180./3.14159
          CALL HFILL(IHOFF+130,Q(LPMUO+16),0.,1.)
          CALL HFILL(IHOFF+134,Q(LPMUO+24),0.,1.)
          CALL HFILL(IHOFF+135,Q(LPMUO+23),0.,1.)
          IF(IQ(LPMUO+6).EQ.0) THEN    ! NO CD TRACKS
            CALL HFILL(IHOFF+133,THETD,0.,1.)
          ENDIF
          J=0
          DOTMAX=.95
C   DO MONTE CARLO
          DO IJ=1,NPART
            IF(P(IJ).NE.0..AND.Q(LPMUO+13).NE.0.) THEN
              DOT=(PX(IJ)*Q(LPMUO+10)+PY(IJ)*Q(LPMUO+11)
     A          +PZ(IJ)*Q(LPMUO+12))/P(IJ)/Q(LPMUO+13)
CCC SEE IF A CLOSE FOUND TRACK
              IF(DOT.GT.DOTMAX) THEN
                DOTMAX=DOT
                J=IJ    ! ISAJET MATCH
              ENDIF
            ENDIF
          ENDDO
          IF(J.EQ.0) CALL HFILL(IHOFF+132,THETD,0.,1.)   !NO ISAJET MATCH
          IF(ABS(Q(LPMUO+16)).LT.2.1) THEN      ! ETA CUT
            CALL HFILL(IHOFF+131,Q(LPMUO+14),0.,1.)
            CALL HFILL(IHOFF+129,Q(LPMUO+13),0.,1.)
            CALL HFILL(IHOFF+136,Q(LPMUO+35)*A,0.,1.)
            CALL HFILL(IHOFF+138,Q(LPMUO+28),0.,1.)
            CALL HFILL(IHOFF+139,FLOAT(IQ(LPMUO+6)),0.,1.)  ! NO. CD TRACKS
CCC   COMPARE TO MUOT
            NS=IQ(LPMUO-2)
            LMUOT=LQ(LPMUO-NS-1)
            PPMUO=-IQ(LPMUO+2)/14*Q(LPMUO+13)
            IF (IQ(LMUOT+4).NE.5) THEN ! skip A stubs
              CALL HFILL(140,(PPMUO-Q(LMUOT+23))/PPMUO,0.,1.)
            END IF
            IF(J.NE.0) THEN            ! ISAJET MATCH
              ET = SQRT(PX(J)**2 + PY(J)**2)
              THMC=TH(J)*180./3.14159
              DETA=ETA(J)-Q(LPMUO+16)
              DPHI=PHI(J)-Q(LPMUO+17)
              DR=SQRT(DETA**2+DPHI**2)
              DET=ET-Q(LPMUO+14)
              DTHETA=TH(J)-Q(LPMUO+15)
              CALL HFILL(IHOFF+120,DPHI,0.,1.)
              CALL HFILL(IHOFF+121,DTHETA,0.,1.)
              CALL HFILL(IHOFF+122,DET,0.,1.)
              CALL HFILL(IHOFF+123,P(J)-Q(LPMUO+13),0.,1.)
              IF(Q(LPMUO+13).NE.0.) THEN
                PRES=(1./Q(LPMUO+13)-1./P(J))*P(J)
              ELSE
                PRES=-99999.
              ENDIF
              CALL HFILL(IHOFF+124,PRES,0.,1.)
              IF(ABS(P(J)).LE.15.) THEN
                CALL HFILL(IHOFF+125,PRES,0.,1.)
              ELSE IF(ABS(P(J)).GT.15..AND.ABS(P(J)).LE.25.) THEN
                CALL HFILL(IHOFF+126,PRES,0.,1.)
              ELSE IF(ABS(P(J)).GT.25..AND.ABS(P(J)).LE.60.) THEN
                CALL HFILL(IHOFF+127,PRES,0.,1.)
              ENDIF
            ENDIF
          ENDIF
          IF (IQ(LPMUO+54).GT.0) THEN
          DO J=1,10
            CALL HFILL(IHOFF+200+J,Q(LPMUO+90+J),0.,1.)
          END DO
          END IF
          LPMUO=LQ(LPMUO)          ! pointer to next muon
        ENDDO
      ENDIF
      CALL HFILL(IHOFF+11,GP1,0.,1.)
      CALL HFILL(IHOFF+12,GP2,0.,1.)
      RETURN
      END
