      SUBROUTINE MUTHX2(IQUAD,XI,YI,ZI,XO,YO,ZO,UI,VI,WI,UO,
     A  VO,WO,NPTS,JADD,IMUOH,IDRFT,IFW3,C12,IVTX)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC    REFIT CENTRAL+END TRACK; DRIFT VIEW ONLY FOR NOW
CC    INPUT: IQUAD  -  QUADRANT
CC           XI-W0 - point and direction cosines inside and outside
CC           NPTS  - number of new points
CC           JADD  - addresses of new points
CC           IMUOH  - location of new points in MUOH
CC           IDRFT - drift solution: CAN CHANGE
CC           C12   - NEW BEND VIEW QUALITY OF FIT
CC           IFW3   - 110,ABC;120,BC;130,AC;140,AB
CC      DH 3/93 initial version
CC      DH 7/94 ALLOW MISSING DRIFT TIMES
CC      DH 9/94 fix IVTX for cosmics
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER NMAX,NFIT,IVTX,IORENT,MULAYR,MUQUAD,IVER,NV,J
      PARAMETER (NMAX=40)
      REAL XI,YI,ZI,XO,YO,ZO,UI,VI,WI,UO,VO,WO,SLBI,SLBO,DD1,DD2
      INTEGER IQUAD,NPTS,JADD(NMAX),IMUOH(NMAX),IDRFT(NMAX),
     A  FLAG(NMAX),IS(NMAX),KADD(NMAX)
      INTEGER GZMUOH,LMUOH,I,LP,IADD,ILAYER(4),NADJ,NLAYER
      INTEGER NMOD,NPLN,NWIR,IERR,JQUAD,LAYER,IFW3,NSAME,I11
      REAL XYZIN(3),XYZOUT(3),XYZBEND(3)
      REAL SLNI,SLNO,XON,ZON,XIN,ZIN
      REAL XX,YY,ZZ,X,Y,Z,ZS(NMAX),XS(NMAX),XDR(2,NMAX),C12,
     AZBEND,VERTEX(3),VA,VB,XOB,ZOB,SLO1,XIB,ZIB,SLI1,C1,C2,C12A
C
      IFW3=0
      I11=0
      IF(IQUAD.LE.4) RETURN         ! SKIP CENTRAL
      DO I=1,4
        ILAYER(I)=0
      ENDDO
      NSAME=0
      NFIT=0
      LMUOH=GZMUOH(0)
CCC   LOOP OVER HITS ON TRACK; DO FIRST CENTRAL+END
      DO 100 I=1,NPTS
        LP=28*(IMUOH(I)-1)+LMUOH
        IADD=IQ(LP+1)       ! WIRE ADDRESS
        CALL MUADD(IADD,NMOD,NPLN,NWIR,IERR)
CC   SEE IF IN RIGHT QUADRANT
        JQUAD=MUQUAD(NMOD)
        IF(MOD(JQUAD,4).NE.MOD(IQUAD,4)) GO TO 100
        IF(JQUAD.LE.4) NSAME=NSAME+1
        LAYER=MULAYR(NMOD)
        ILAYER(LAYER)=ILAYER(LAYER)+1
        IORENT=IABS(IQ(LP+5))
        XX=Q(LP+21)+Q(LP+26)
        YY=Q(LP+22)+Q(LP+27)
        ZZ=Q(LP+23)+Q(LP+28)
        IF(IORENT.EQ.1) THEN
          X=ZZ     ! DRIFT
          Y=YY     ! WIRE
          Z=XX     ! INTERPLANE
        ELSE IF(IORENT.EQ.2) THEN
          X=ZZ
          Y=XX
          Z=YY
        ELSE IF(IORENT.EQ.3) THEN
          X=XX
          Y=YY
          Z=ZZ
        ELSE IF(IORENT.EQ.4) THEN
          X=YY
          Y=XX
          Z=ZZ
        ENDIF
        NFIT=NFIT+1
        FLAG(NFIT)=0                  ! END
        IF(JQUAD.LE.4) FLAG(NFIT)=1   ! CENTRAL
        KADD(NFIT)=IADD
        XS(NFIT)=X
        ZS(NFIT)=Z
        XDR(1,NFIT)=9999.
        XDR(2,NFIT)=9999.
        IF(IQ(LP+6).GE.1) XDR(1,NFIT)=Q(LP+15)
        IF(IQ(LP+6).EQ.2) XDR(2,NFIT)=Q(LP+16)
  100 CONTINUE
      NLAYER=0
      IF(ILAYER(1).GT.0) NLAYER=NLAYER+1
      IF(ILAYER(2).GT.0) NLAYER=NLAYER+1
      IF(ILAYER(3).GT.0) NLAYER=NLAYER+1
      IF(ILAYER(4).GT.0) NLAYER=NLAYER+1
      IF(NSAME.GE.3.AND.NLAYER.GT.1) THEN   ! 3 POINTS IN CENTRAL; SAME ORIENTATION
        CALL MUZBN2(IQUAD,XI,YI,ZI,XO,YO,ZO,UI,VI,WI,
     A  UO,VO,WO,XYZIN,XYZOUT,XYZBEND)       ! MAGNET BEND POINT
        ZBEND=XYZBEND(3)     ! ALWAYS IN END
        CALL VERXYZ(IVER,VERTEX,NV)
        VA=0.
        VB=VERTEX(3)
        CALL MUFTBX(NFIT,FLAG,ZBEND,VA,VB,KADD,ZS,XS,XDR,1.,XOB,ZOB,
     A    SLBO,XIB,ZIB,SLBI,C1,C2,C12A,IS,IVTX)
        IF(C1.LT.300..AND.C2.LT.300.) THEN    ! 'GOOD' FIT
          C12=C12A
          I11=100
          IF(ILAYER(1).GE.1) THEN
            IF(ILAYER(2).GE.1) THEN
              IF(ILAYER(3).GE.1.OR.ILAYER(4).GE.1) THEN
                IFW3=110                                       !ABC
              ELSE
                IFW3=120                                       !AB
              ENDIF
            ELSE
              IF(ILAYER(3).GE.1.OR.ILAYER(4).GE.1) IFW3=130    !AC
            ENDIF
          ELSE
            IF(ILAYER(2).GE.1.AND.ILAYER(3).GE.1) IFW3=120     !BC
            IF(ILAYER(2).GE.1.AND.ILAYER(4).GE.1) IFW3=120
          ENDIF
          DO J=1,NPTS
            DO I=1,NFIT
              IF(KADD(I).EQ.JADD(J)) IDRFT(J)=IS(I)
            ENDDO
          ENDDO
C
C SET UP OUTPUT BY TRANSFORMING TO GLOBAL SYSTEM
C first convert input to slopes
          IF(IQUAD.EQ.5.OR.IQUAD.EQ.7.OR.IQUAD.EQ.9.OR.IQUAD.
     A      EQ.11) THEN
            SLI1=VI/WI
            SLO1=VO/WO
            YO=YO+SLO1*(ZBEND-ZO)
            XO=XOB+SLBO*(ZBEND-ZOB)
            XI=XIB+SLBI*(ZI-ZIB)
          ELSE IF(IQUAD.EQ.6.OR.IQUAD.EQ.8.OR.IQUAD.EQ.10.OR.IQUAD.
     A      EQ.12) THEN
            SLI1=UI/WI
            SLO1=UO/WO
            XO=XO+SLO1*(ZBEND-ZO)
            YO=XOB+SLBO*(ZO-ZOB)
            YI=XIB+SLBI*(ZI-ZIB)
          ENDIF
          ZO=ZBEND
          DD1=SQRT(SLI1**2+SLBI**2+1.)
          DD2=SQRT(SLO1**2+SLBO**2+1.)
          IF(IQUAD.EQ.5.OR.IQUAD.EQ.7) THEN   ! NORTH +-X
            UO=-SLBO/DD2
            WO=-1./DD2
            VO=-SLO1/DD2
            UI=-SLBI/DD1
            WI=-1./DD1
            VI=-SLI1/DD1
          ELSE IF(IQUAD.EQ.6.OR.IQUAD.EQ.8) THEN   ! NORTH +-Y
            VO=-SLBO/DD2
            WO=-1./DD2
            UO=-SLO1/DD2
            VI=-SLBI/DD1
            WI=-1./DD1
            UI=-SLI1/DD1
          ELSE IF(IQUAD.EQ.9.OR.IQUAD.EQ.11) THEN   ! SOUTH +-X
            UI=SLBI/DD1
            WI=1./DD1
            VI=SLI1/DD1
            UO=SLBO/DD2
            WO=1./DD2
            VO=SLO1/DD2
          ELSE IF(IQUAD.EQ.10.OR.IQUAD.EQ.12) THEN   ! SOUTH +-Y
            VI=SLBI/DD1
            WI=1./DD1
            UI=SLI1/DD1
            VO=SLBO/DD2
            WO=1./DD2
            UO=SLO1/DD2
          ENDIF
        ENDIF
      ENDIF
CCCC   DO ADJACENT
      DO I=1,4
        ILAYER(I)=0
      ENDDO
      NADJ=0
      NFIT=0
      DO 200 I=1,NPTS
        LP=28*(IMUOH(I)-1)+LMUOH
        IADD=IQ(LP+1)       ! WIRE ADDRESS
        CALL MUADD(IADD,NMOD,NPLN,NWIR,IERR)
CC   SEE IF IN RIGHT QUADRANT
        JQUAD=MUQUAD(NMOD)
        IF(MOD(JQUAD,4).EQ.MOD(IQUAD,4)) GO TO 200
        IF(JQUAD.LE.4) NADJ=NADJ+1
        LAYER=MULAYR(NMOD)
        ILAYER(LAYER)=ILAYER(LAYER)+1
        IORENT=IABS(IQ(LP+5))
        XX=Q(LP+21)+Q(LP+26)
        YY=Q(LP+22)+Q(LP+27)
        ZZ=Q(LP+23)+Q(LP+28)
        IF(IORENT.EQ.1) THEN
          X=ZZ     ! DRIFT
          Y=YY     ! WIRE
          Z=XX     ! INTERPLANE
        ELSE IF(IORENT.EQ.2) THEN
          X=ZZ
          Y=XX
          Z=YY
        ELSE IF(IORENT.EQ.3) THEN
          X=XX
          Y=YY
          Z=ZZ
        ELSE IF(IORENT.EQ.4) THEN
          X=YY
          Y=XX
          Z=ZZ
        ENDIF
        NFIT=NFIT+1
        FLAG(NFIT)=0                  ! END
        IF(JQUAD.LE.4) FLAG(NFIT)=1   ! CENTRAL
        KADD(NFIT)=IADD
        XS(NFIT)=X
        ZS(NFIT)=Z
        XDR(1,NFIT)=Q(LP+15)
        XDR(2,NFIT)=9999.
        IF(IQ(LP+6).EQ.2) XDR(2,NFIT)=Q(LP+16)
  200 CONTINUE
      IF(NADJ.GE.3) THEN   ! 3 POINTS IN ADJACENT; SAME ORIENTATION
        CALL MUZBN2(IQUAD,XI,YI,ZI,XO,YO,ZO,UI,VI,WI,
     A  UO,VO,WO,XYZIN,XYZOUT,XYZBEND)       ! MAGNET BEND POINT
        ZBEND=XYZBEND(3)     ! ALWAYS IN END
        CALL VERXYZ(IVER,VERTEX,NV)
        VA=0.
        VB=VERTEX(3)
        CALL MUFTBX(NFIT,FLAG,ZBEND,VA,VB,KADD,ZS,XS,XDR,1.,XON,ZON,
     A    SLNO,XIN,ZIN,SLNI,C1,C2,C12A,IS,IVTX)
        IF(C1.LT.300..AND.C2.LT.300.) THEN    ! 'GOOD' FIT
          IF(ILAYER(1).GE.1) THEN
            IF(ILAYER(2).GE.1) THEN
              IF(ILAYER(3).GE.1.OR.ILAYER(4).GE.1) THEN
                IFW3=210+I11                                   !ABC
              ELSE
                IFW3=240+I11                                   !AB
              ENDIF
            ELSE
              IF(ILAYER(3).GE.1.OR.ILAYER(4).GE.1) IFW3=230+I11    !AC
            ENDIF
          ELSE
            IF(ILAYER(2).GE.1.AND.ILAYER(3).GE.1) IFW3=220+I11     !BC
            IF(ILAYER(2).GE.1.AND.ILAYER(4).GE.1) IFW3=220+I11
          ENDIF
          DO J=1,NPTS
            DO I=1,NFIT
              IF(KADD(I).EQ.JADD(J)) IDRFT(J)=IS(I)
            ENDDO
          ENDDO
C
C SET UP OUTPUT BY TRANSFORMING TO GLOBAL SYSTEM
C first convert input to slopes
          IF(IQUAD.EQ.5.OR.IQUAD.EQ.7.OR.IQUAD.EQ.9.OR.IQUAD.
     A      EQ.11) THEN
            SLI1=UI/WI
            SLO1=UO/WO
            XO=XO+SLO1*(ZBEND-ZO)
            YO=XON+SLNO*(ZBEND-ZON)
            YI=XIN+SLNI*(ZI-ZIN)
          ELSE IF(IQUAD.EQ.6.OR.IQUAD.EQ.8.OR.IQUAD.EQ.10.OR.IQUAD.
     A      EQ.12) THEN
            SLI1=VI/WI
            SLO1=VO/WO
            YO=YO+SLO1*(ZBEND-ZO)
            XO=XON+SLNO*(ZBEND-ZON)
            XI=XIN+SLNI*(ZI-ZIN)
          ENDIF
          ZO=ZBEND
          DD1=SQRT(SLI1**2+SLNI**2+1.)
          DD2=SQRT(SLO1**2+SLNO**2+1.)
          IF(IQUAD.EQ.5.OR.IQUAD.EQ.7) THEN   ! NORTH +-X
            VO=-SLNO/DD2
            WO=-1./DD2
            UO=-SLO1/DD2
            VI=-SLNI/DD1
            WI=-1./DD1
            UI=-SLI1/DD1
          ELSE IF(IQUAD.EQ.6.OR.IQUAD.EQ.8) THEN   ! NORTH +-Y
            UO=-SLNO/DD2
            WO=-1./DD2
            VO=-SLO1/DD2
            UI=-SLNI/DD1
            WI=-1./DD1
            VI=-SLI1/DD1
          ELSE IF(IQUAD.EQ.9.OR.IQUAD.EQ.11) THEN   ! SOUTH +-X
            VI=SLNI/DD1
            WI=1./DD1
            UI=SLI1/DD1
            VO=SLNO/DD2
            WO=1./DD2
            UO=SLO1/DD2
          ELSE IF(IQUAD.EQ.10.OR.IQUAD.EQ.12) THEN   ! SOUTH +-Y
            UI=SLNI/DD1
            WI=1./DD1
            VI=SLI1/DD1
            UO=SLNO/DD2
            WO=1./DD2
            VO=SLO1/DD2
          ENDIF
        ENDIF
      ENDIF
      RETURN
      END
