      SUBROUTINE MUTSTR(IFW1,IQUAD,NTOT,JADD,IPOINT,IDELT,
     A X,Y,Z,XDR,XV)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC    ROUTINE TAKES HITS ON A 3D MUON TRACK, DOES A 'FINAL' FIT
CC    ON THEM (SIMPLE 'TRACKFINDING' FIT WHICH IGNORES M.S. ETC
CC    ) AND THEN STORES ALL THE
CC    OUTPUT PARAMETERS IN ZEBRA BANKS
CC
CC    INPUT: IQUAD  - QUADRANT OF TRACK
C            IFW1   - TRACK FLAG 0=A-B-C,1=B-C,2=A-C,3=A-B,4=C-C
CC           NTOT   - TOTAL NUMBER OF HITS
CC           JADD   - WIRE ADDRESSES OF HITS
CC           IPOINT - LOCATION IN HIT BANK (MUOH) OF HITS
CC           IDELT  - TIME NUMBER OF DELTA TIME SOLUTION
CC           X      - POSITION OF WIRE IN BEND DIRECTION
CC           Y      - POSITION IN DELTA T OF NONBEND DIRECTION
CC           Z      - POSITION IN 'PRIMARY' DIRECTION
CC           XDR    - DRIFT DISTANCE
CC           XV     - PAD POSITIONS
CC
CC    OUTPUT IN ZEBRA BANKS MUOT AND MUTH
CC
CC    HEDIN 4-29-86  11-86 DH NEW ZEBRA FORMAT
CC    dh 2-18-88 change ZBEND careful as this parameter should be elsewhere
CC    DH 7/88 ALLOW FOR STRAIGHT LINE FIT IN BEND VIEW
CC    DH 8/88 ADD IDELT FLAGGING
CC    DH 9/88 store in MUOT through subroutine call
CC    DH 11/88 MAKE USABLE BY ALL QUADRANTS   BASEMENT=QUAD 0
CC    DH 12/88 change slightly MUOT call
CC    DH 12/88 do better fit bend view
CC    DH 3/89 ADD CALL TO MUPQCK quick momentum finder
CC    DH 4/89 if bad fit do not use fit solutions
CC    DH 5/89 FIT IN NONBEND
CC    DH 9/89 STRORE DIFFERENT TRACK INFO
CC    DH 10/89 FIX GTSRCP
CC    DH 1/90  FIT BEND USING ONE POINT/PLANE
CC    DH 3/90 FIX QUAD 5,7
CC    DH 5/90 ALLOW 2 MODULE TRACKS
CC    DH 7/90 CHANGE VERXYZ CALL, FIX NON-BEND BUG
CC    DH 9/90 ADD IFW2
CC    DH 1/91 change MUOT contents
CC    DH 4/91 HANDLE BAD FITS A LITTLE BETTER
CC    DH 4/91 FIX MIXED ORIENTATION; PART 1
CC    DH 6/91 USE EZGET
CC    SA 7/91 NEW ARGUMENT ADDED TO VERXYZ
CC    DH 8/91 rearrange logic somewhat
CC    DH 10/91 ADD IFW3, CORRECT IFW1
C     DH 1/92 CORRECT BEND FEW QUALITY OF FIT DEFINITION
C     DH 2/92 include vertex info in MUFTNB call, remove 'bad'
C             cells from non-bend, comment out Daria's fix for now
C             (MUFTNB will now do the equivalent)
C     DH 5/92 ADD BDL, CT TO MUOT
C     DH 6/92 muthit cleanup
C     dh 7/92 protect against direction cosines=0
C     DH 9/92 add IFW1 and wire length to MUFTNB call
C     DH 9/92 donot use deltaT points if not on bend view
C     DH 10/92 flag IFW1 if vertex used on bend view fit
C     DH 3/93 do central+end fit
C     DH 4/93 add QUAD to MUFTNB call
C     DH 11/93 add central flag to MUTHXX caa
C     DH 4/94 ADD QUAD TO MUFTBD CALL
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER NMAX,IQUAD,NTOT,IVER,I,L,FIRST,IFW1,IFW2,NPTS,J,ICENT
      PARAMETER (NMAX=40)
      INTEGER JADD(NMAX),IPOINT(NMAX),IDSOL(NMAX),IDRFT(20),
     A IPAD(NMAX),IDELT(NMAX),KADD(20),IDT(20),IVERN(20),ORENT,
     A IMUOH(20),IFW1X,IFW3,MOD,PLN,WIR,IERR,MULAYR,ILAYR(4),IVTX
      REAL X(NMAX),Y(NMAX),Z(NMAX),WPL(NMAX),ZBEND,XBEND,SLI1,SLO1,
     A CHIB,DD1,DD2,DCXI,DCYI,DCZI,DCXO,DCYO,DCZO,XX,YY,ZZ,YNB,ZNB,
     A SLNBI,SLNBO,CT,CV,CHINB,P,DP,VER,VXG,VSL,XI,ZI,XXI,YYI,ZZI
      INTRINSIC SQRT
      REAL ELCAL,ELFE,YANB,BDL,EPS,CHIBVBC
      REAL XDR(2,NMAX),X1,Z1,C1,C2,XV(2,NMAX),VERTEX(3),VA,VB,VC,
     AWLEN(NMAX)
      INTEGER FLAG(NMAX),IHIT,IS(NMAX),IPDFIT,IER,NV,LMUOH,GZMUOH
C
C
      DATA EPS/.0000001/   ! SMALL NUMBER
      DATA WPL/NMAX*1./
      DATA FIRST/0/
      DATA VER/60.96/      ! PAD LENGTH-----SHOULD DO BETTER
C
C      FIT IN BEND VIEW
C
      LMUOH=GZMUOH(0)
      IFW3=0
      IF(FIRST.EQ.0) THEN
        CALL EZGET('IPDFIT',IPDFIT,IER)   ! FIT USING PADS??
      ENDIF
      CALL MUZBND(IQUAD,ZBEND)       ! MAGNET BEND POINT
      CALL VERXYZ(IVER,VERTEX,NV)
      IF(IQUAD.LE.4) THEN        ! CENTRAL
        VA=VERTEX(3)
        VB=0.
        VC=0.
      ELSE
        VA=0.
        VB=VERTEX(3)
        VC=0.
      ENDIF
C
      DO IHIT=1,NTOT
        FLAG(IHIT)=0
        IDSOL(IHIT)=0
        IPAD(IHIT)=0
      ENDDO
      CALL MUFTBD(NTOT,IQUAD,FLAG,ZBEND,VA,VB,JADD,Z,X,XDR,1.,X1,Z1,
     A  SLO1,XI,ZI,SLI1,C1,C2,CHIB,IS,IVTX)
      CHIBVBC=C2
      IF(C1.EQ.9999..OR.C2.EQ.9999.) CHIB=999.
      IF(CHIB.LT.500.) THEN    ! 'GOOD' FIT
        DO IHIT=1,NTOT
          IDSOL(IHIT)=IS(IHIT)
CCC   ONLY USE PAD HITS WHICH HAVE A GOOD DRIFT CELL
CCC   will need to evaluate what to do with time division points
          IF(IS(IHIT).EQ.0) THEN
            Z(IHIT)=-99999.
            Y(IHIT)=-99999.
            XV(1,IHIT)=-99999.
            XV(2,IHIT)=-99999.
          ENDIF
        ENDDO
      ELSE
        CHIB=999.
      ENDIF
C
C     FIT IN NONBEND
C
      IF(IPDFIT.EQ.0) THEN     ! USE PADS
        DO IHIT=1,NTOT         ! get wire length
          WLEN(IHIT)=Q(LMUOH+(IPOINT(IHIT)-1)*28+24)/2.
          ORENT=IABS(IQ(LMUOH+(IPOINT(IHIT)-1)*28+5))
CCC    WLEN=maximum fiducial point of cell along pad
          IF(ORENT.EQ.1.OR.ORENT.EQ.3) THEN
            WLEN(IHIT)=WLEN(IHIT)+Q(LMUOH+(IPOINT(IHIT)-1)*28+22)
          ELSE
            WLEN(IHIT)=WLEN(IHIT)+Q(LMUOH+(IPOINT(IHIT)-1)*28+21)
          ENDIF   
        ENDDO
        CALL MUFTNB(IQUAD,IFW1,IVER,VB,VC,NTOT,Z,Y,XV,VER,ZBEND,
     A  WLEN,YNB,YANB,ZNB,SLNBI,SLNBO,CHINB,CT,CV,IS)
C        CHINB=CV                        ! USE PADS ONLY
        YNB=YNB+(ZBEND-ZNB)*SLNBO       ! TRANSFER TO ZBEND(redundant)
        IF(CHINB.LT.500.) THEN    ! 'GOOD' FIT
          DO IHIT=1,NTOT
            IPAD(IHIT)=IS(IHIT)
          ENDDO
        ELSE
          CHINB=999.
        ENDIF
      ELSE
        CALL LINFIT(NTOT,Y(1),Z(1),WPL(1),YNB,ZNB,SLNBO,VXG,VSL,CHINB)
        YNB=YNB+(ZBEND-ZNB)*SLNBO       ! TRANSFER TO ZBEND
        YANB=YNB
        CT=CHINB
        IF(IVER.EQ.0) THEN
          SLNBI=(YANB-VC)/(ZBEND-VB)
        ELSE
          SLNBI=SLNBO
        ENDIF
      ENDIF
C
C SET UP OUTPUT BY TRANSFORMING TO GLOBAL SYSTEM
C THIS SHOULD PROBABLY BE MADE INTO A SUBROUTINE AT SOME POINT
C
      IF(CHINB.LT.500..AND.CHIB.LT.500.) THEN
        DD1=SQRT(SLI1**2+SLNBI**2+1.)
        DD2=SQRT(SLO1**2+SLNBO**2+1.)
        IF(IQUAD.EQ.1) THEN         ! CENTRAL +X
          DCYI=SLNBI/DD1
          DCXI=1./DD1
          DCZI=SLI1/DD1
          DCYO=SLNBO/DD2
          DCXO=1./DD2
          DCZO=SLO1/DD2
          YY=YNB
          XX=ZBEND
          ZZ=X1
          YYI=YANB+SLNBI*(ZI-ZBEND)
          IF(ZI.EQ.9999.) YYI=9999.
          XXI=ZI
          ZZI=XI
        ELSE IF(IQUAD.EQ.2.OR.IQUAD.EQ.0) THEN         ! CENTRAL +Y
          DCXI=SLNBI/DD1
          DCYI=1./DD1
          DCZI=SLI1/DD1
          DCXO=SLNBO/DD2
          DCYO=1./DD2
          DCZO=SLO1/DD2
          XX=YNB
          YY=ZBEND
          ZZ=X1
          XXI=YANB+SLNBI*(ZI-ZBEND)
          IF(ZI.EQ.9999.) XXI=9999.
          YYI=ZI
          ZZI=XI
        ELSE IF(IQUAD.EQ.3) THEN        ! CENTRAL -X
          DCYO=-SLNBO/DD2
          DCXO=-1./DD2
          DCZO=-SLO1/DD2
          DCYI=-SLNBI/DD1
          DCXI=-1./DD1
          DCZI=-SLI1/DD1
          YY=YNB
          XX=ZBEND
          ZZ=X1
          YYI=YANB+SLNBI*(ZI-ZBEND)
          IF(ZI.EQ.9999.) YYI=9999.
          XXI=ZI
          ZZI=XI
        ELSE IF(IQUAD.EQ.4) THEN       ! CENTRAL -Y
          DCXO=-SLNBO/DD2
          DCYO=-1./DD2
          DCZO=-SLO1/DD2
          DCXI=-SLNBI/DD1
          DCYI=-1./DD1
          DCZI=-SLI1/DD1
          XX=YNB
          YY=ZBEND
          ZZ=X1
          XXI=YANB+SLNBI*(ZI-ZBEND)
          IF(ZI.EQ.9999.) XXI=9999.
          YYI=ZI
          ZZI=XI
        ELSE IF(IQUAD.EQ.5.OR.IQUAD.EQ.7) THEN   ! NORTH +-X
          DCYO=-SLNBO/DD2
          DCZO=-1./DD2
          DCXO=-SLO1/DD2
          DCYI=-SLNBI/DD1
          DCZI=-1./DD1
          DCXI=-SLI1/DD1
          YY=YNB
          ZZ=ZBEND
          XX=X1
          YYI=YANB+SLNBI*(ZI-ZBEND)
          IF(ZI.EQ.9999.) YYI=9999.
          ZZI=ZI
          XXI=XI
        ELSE IF(IQUAD.EQ.6.OR.IQUAD.EQ.8) THEN   ! NORTH +-Y
          DCXO=-SLNBO/DD2
          DCZO=-1./DD2
          DCYO=-SLO1/DD2
          DCXI=-SLNBI/DD1
          DCZI=-1./DD1
          DCYI=-SLI1/DD1
          XX=YNB
          ZZ=ZBEND
          YY=X1
          XXI=YANB+SLNBI*(ZI-ZBEND)
          IF(ZI.EQ.9999.) XXI=9999.
          ZZI=ZI
          YYI=XI
        ELSE IF(IQUAD.EQ.9.OR.IQUAD.EQ.11) THEN   ! SOUTH +-X
          DCYI=SLNBI/DD1
          DCZI=1./DD1
          DCXI=SLI1/DD1
          DCYO=SLNBO/DD2
          DCZO=1./DD2
          DCXO=SLO1/DD2
          YY=YNB
          ZZ=ZBEND
          XX=X1
          YYI=YANB+SLNBI*(ZI-ZBEND)
          IF(ZI.EQ.9999.) YYI=9999.
          ZZI=ZI
          XXI=XI
        ELSE IF(IQUAD.EQ.10.OR.IQUAD.EQ.12) THEN   ! SOUTH +-Y
          DCXI=SLNBI/DD1
          DCZI=1./DD1
          DCYI=SLI1/DD1
          DCXO=SLNBO/DD2
          DCZO=1./DD2
          DCYO=SLO1/DD2
          XX=YNB
          ZZ=ZBEND
          YY=X1
          XXI=YANB+SLNBI*(ZI-ZBEND)
          IF(ZI.EQ.9999.) XXI=9999.
          ZZI=ZI
          YYI=XI
        ENDIF
CCC    CHECK FOR DIR. COSINES IDENTICALLY 0
        IF(DCXI.EQ.0.) DCXI=EPS
        IF(DCYI.EQ.0.) DCYI=EPS
        IF(DCZI.EQ.0.) DCZI=EPS
        IF(DCXO.EQ.0.) DCXO=EPS
        IF(DCYO.EQ.0.) DCYO=EPS
        IF(DCZO.EQ.0.) DCZO=EPS
CC
CC    LOOK FOR OTHER HITS IN ADJACENT QUADRANTS--NOTE THAT MUTHXX
CC    IS 'ALLOWED' TO CHANGE THE DIRECTION COSINES, ETC
        CALL MUTHXX(IQUAD,XXI,YYI,ZZI,XX,YY,ZZ,DCXI,DCYI,DCZI,
     ADCXO,DCYO,DCZO,NPTS,KADD,IMUOH,IDRFT,IDT,IVERN,IFW1X,ICENT)
        IF(DCXI.EQ.0.) DCXI=EPS
        IF(DCYI.EQ.0.) DCYI=EPS
        IF(DCZI.EQ.0.) DCZI=EPS
        IF(DCXO.EQ.0.) DCXO=EPS
        IF(DCYO.EQ.0.) DCYO=EPS
        IF(DCZO.EQ.0.) DCZO=EPS
        IF(NPTS.GT.0) THEN
          DO I=1,NPTS
            IF(I+NTOT.LE.NMAX) THEN
              JADD(NTOT+I)=KADD(I)
              IPOINT(NTOT+I)=IMUOH(I)
              IDSOL(NTOT+I)=IDRFT(I)
              IDELT(NTOT+I)=IDT(I)
              IPAD(NTOT+I)=IVERN(I)
            ENDIF
          ENDDO
          NTOT=NTOT+NPTS
          IF(NTOT.GT.NMAX) NTOT=NMAX
          IF(ICENT.GE.1) THEN     ! ADDED CENTRAL
            CALL MUTHX2(IQUAD,XXI,YYI,ZZI,XX,YY,ZZ,DCXI,DCYI,DCZI,
     A  DCXO,DCYO,DCZO,NTOT,JADD,IPOINT,IDSOL,ICENT,CHIB,IVTX)
          ENDIF
          IFW3=IFW1+1+IFW1X+ICENT       ! =0 IF NO MIXED ORIENTATION
CCCC    SEE IF A LAYER HAS BEEN ADDED
          IF(IFW1.NE.0) THEN          ! ALREADY A-B-C
            DO I=1,4
              ILAYR(I)=0
            ENDDO
            DO I=1,NTOT
              CALL MUADD(JADD(I),MOD,PLN,WIR,IERR)
              J=MULAYR(MOD)
              ILAYR(J)=ILAYR(J)+1
            ENDDO
           IFW1=0             ! HAS A-B-C  LAYERS
           IF(ILAYR(1).EQ.0) IFW1=1       ! NO A LAYER
           IF(ILAYR(2).EQ.0) IFW1=2       ! NO B LAYER
           IF(ILAYR(3).EQ.0.AND.ILAYR(4).EQ.0) IFW1=3   ! NO C LAYER
           IF(ILAYR(1).EQ.0.AND.ILAYR(2).EQ.0) IFW1=4   ! NO A-B LAYER
          ENDIF
        ENDIF
C
C  odd QUAD track in top-bottom quadrant
C
C        IF (IQUAD.EQ.5.OR.IQUAD.EQ.7.OR.IQUAD.EQ.9.OR.IQUAD.EQ.11) THEN
C          IF (ABS(YY).GT.ABS(XX)) THEN
C            DD1=XX**2+YY**2+(ZZ-VERTEX(3))**2
C            DD1=SQRT(DD1)
C            DCXI=XX/DD1
C            DCYI=YY/DD1
C            DCZI=(ZZ-VERTEX(3))/DD1
C          END IF
C        END IF
C
CC
CCC DO MOMENTUM FINDER
        CALL MUPQCK(IQUAD,XXI,YYI,ZZI,XX,YY,ZZ,DCXI,DCYI,
     A    DCZI,DCXO,DCYO,DCZO,P,DP,ELCAL,ELFE,BDL)
        IF(ABS(P).GE.999.) THEN
          P=9999.
          DP=999.
        ENDIF
        IFW1=IFW1+10*IVTX    ! FLAG IF VTX USED IN FIRST BEND FIT
CC   SEE IF TRACK IS 'GOOD'
        CALL MTGOOD(IQUAD,P,XXI,YYI,ZZI,XX,YY,ZZ,DCXI,DCYI,
     A   DCZI,DCXO,DCYO,DCZO,CHIB,CHINB,CHIBVBC,CT,IFW1,IFW2)
      ELSE
        XXI=0.
        YYI=0.
        ZZI=0.
        XX=1.
        YY=1.
        ZZ=1.
        DCXI=1.
        DCYI=1.
        DCZI=1.
        DCXO=1.
        DCYO=1.
        DCZO=1.
        P=9999.
        DP=999.
        BDL=1.
        ELCAL=0.
        ELFE=0.
        IFW2=15
      ENDIF
CC
CC   FILL UP OUTPUT ZEBRA BANK
CC
      DP=CHIBVBC
      CALL MUMUOT(NTOT,IQUAD,IFW1,IFW2,IFW3,XXI,YYI,ZZI,XX,YY,ZZ,
     A DCXI,DCYI,DCZI,DCXO,DCYO,DCZO,
     A CHIB,CHINB,P,DP,ELCAL,ELFE,BDL,CT,JADD,IPOINT,IDSOL,IDELT,IPAD)
      RETURN
      END
