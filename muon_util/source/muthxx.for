      SUBROUTINE MUTHXX(IQUAD,XI,YI,ZI,XO,YO,ZO,UI,VI,WI,UO,
     A VO,WO,NPTS,JADD,IMUOH,IDRFT,IDELT,IVERN,IFW3,ICENT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC    PUTS HITS ON A GIVEN TRACK IN ADJACENT QUADRANT
CC    REDO FIT FOR SOME CASES
CC    INPUT: IQUAD  -  QUADRANT
CC           XI-W0 - point and direction cosines inside and outside
CC    OUTPUT NPTS  - number of new points
CC           JADD  - addresses of new points
CC           IMUOH  - location of new points in MUOH
CC           IDRFT - drift solution
CC           IDELT  - delta time solution
CC           IVERN  - vernier pad solution
CC           IFW3   - if refit end: 10,ABC;20,BC;30,AC;40,AB
CC           ICENT  - =0 if no central hits; if non-zero then number of hits
CC      DH 4/91 initial version
CC      DH 5/91 MINOR CHANGE; NESTED DO LOOP
CC      DH 6/91 HARDWIRE ROAD FOR L2
CC      DH 8/91 FIT IN DRIFT DIRECTION IF 2 OR MORE LAYERS; add IFW3
CC      DH 10/91 LOOP OVER ALL QUADRANTS; ONLY REFIT ONCE
CC      DH 1/92 CHANGE MUFTBD CALL
CC      DH 2/92 28 words/MUOH hit add MUROTC call
CC      DH 6/92 cut on fiducial of cell
CC      DH 10/92 stick some 2D points on track; call to MUFTBD modified
CC      DH 2/93 JADD/KADD bug if 2D. tighter on bend road (assume pad fit)
CC      DH 3/93 flag central added
CC      DH 5/93 fix XDD bug when no deltaT point on track
CC      DH 11/93 more careful dealing with multiple quadrants; add ICENT
CC      DH 4/94 ADD QUAD to MUFTBD call
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER NMAX,NFIT,REFIT,IVTX
      PARAMETER (NMAX=20)
      PARAMETER (NFIT=40)
      REAL XI,YI,ZI,XO,YO,ZO,UI,VI,WI,UO,VO,WO,ROADNB,SLNBI,SLNBO
      INTEGER IQUAD,NPTS,JADD(NMAX),IMUOH(NMAX),IDRFT(NMAX),IDELT(NMAX),
     A IVERN(NMAX),FLAG(NFIT),IS(NFIT),KADD(NFIT)
      INTEGER GZMUHT,GZMUOH,LMUHT,LMUOH,NMUSRT,I,LP,IADD,ILAYER(4)
      INTEGER NMOD,NPLN,NWIR,IERR,JQUAD,LAYER,K,NLAYER,IFW3
      INTEGER IORENT,MULAYR,MUQUAD,IVER,NV,NQUAD,ICENT,
     A ID,KK,ISIGN(2),NTIME,IT,JT,NCENT,
     A NM,MM(10),IM(10),NTOT2,IPOINT2(NMAX),JADD2(NMAX)
       REAL X2(NMAX),Y2(NMAX),Z2(NMAX),XDR2(2,NMAX),
     A DXX2(NMAX),DYY2(NMAX),DZZ2(NMAX)
      REAL XX,YY,ZZ,DMIN,DD,DEL,S,XP,YP,ANGLE,COST,VECT(3),WLEN,XTD,
     A TCOR,TIME,MUDRFT,DDD(2),X,Y,Z,ZS(NFIT),XS(NFIT),XDR(2,NFIT),XDD,
     A ZBEND,VERTEX(3),VA,VB,XONB,ZONB,SLO1,XINB,ZINB,SLI1,C1,C2,ZDD,
     A DD1,DD2,C12,XXX,YYY,ZZZ,DX,DY,DZ,DXX,DYY,DZZ,ROADB
      INTEGER IOR,IODD,IER
      DATA ISIGN/-1,1/
      DATA ROADNB/50./      ! ROAD IN DELTAT VIEWS
      DATA ROADB/30./      ! ROAD IN BEND; ASSUMES PADS~OK
CC
      ICENT=0
      IFW3=0
      REFIT=0
      NPTS=0
      IF(IQUAD.LE.4) RETURN         ! SKIP CENTRAL
      NTOT2=0
      NM=0
      DO I=1,4
        ILAYER(I)=0
      ENDDO
CC
      LMUHT=GZMUHT(0)
      LMUOH=GZMUOH(0)
      NMUSRT=IQ(LMUHT+2)
      DO NQUAD=1,12        ! LOOP OVER OTHER QUADRANTS
      NCENT=0
      DO 100 I=1,NMUSRT
        LP=28*(I-1)+LMUOH
        IF(IQ(LP+3).NE.0) GO TO 100        ! ALREADY ON A TRACK
        IADD=IQ(LP+1)       ! WIRE ADDRESS
        CALL MUADD(IADD,NMOD,NPLN,NWIR,IERR)
CC   SEE IF IN RIGHT QUADRANT
        JQUAD=MUQUAD(NMOD)
        IF(JQUAD.NE.NQUAD) GO TO 100
        IF(IQUAD.EQ.5) THEN
          IF(JQUAD.NE.1.AND.JQUAD.NE.6.AND.JQUAD.NE.8.AND.
     A    JQUAD.NE.2.AND.JQUAD.NE.4) GO TO 100
        ELSE IF(IQUAD.EQ.6) THEN
          IF(JQUAD.NE.2.AND.JQUAD.NE.5.AND.JQUAD.NE.7.AND.
     A     JQUAD.NE.1.AND.JQUAD.NE.3) GO TO 100
        ELSE IF(IQUAD.EQ.7) THEN
          IF(JQUAD.NE.3.AND.JQUAD.NE.6.AND.JQUAD.NE.8.AND.
     A     JQUAD.NE.2.AND.JQUAD.NE.4) GO TO 100
        ELSE IF(IQUAD.EQ.8) THEN
          IF(JQUAD.NE.4.AND.JQUAD.NE.5.AND.JQUAD.NE.7.AND.
     A     JQUAD.NE.1.AND.JQUAD.NE.3) GO TO 100
        ELSE IF(IQUAD.EQ.9) THEN
          IF(JQUAD.NE.1.AND.JQUAD.NE.10.AND.JQUAD.NE.12.AND.
     A     JQUAD.NE.2.AND.JQUAD.NE.4) GO TO 100
        ELSE IF(IQUAD.EQ.10) THEN
          IF(JQUAD.NE.2.AND.JQUAD.NE.9.AND.JQUAD.NE.11.AND.
     A     JQUAD.NE.1.AND.JQUAD.NE.3) GO TO 100
        ELSE IF(IQUAD.EQ.11) THEN
          IF(JQUAD.NE.3.AND.JQUAD.NE.10.AND.JQUAD.NE.12.AND.
     A     JQUAD.NE.2.AND.JQUAD.NE.4) GO TO 100
        ELSE IF(IQUAD.EQ.12) THEN
          IF(JQUAD.NE.4.AND.JQUAD.NE.9.AND.JQUAD.NE.11.AND.
     A     JQUAD.NE.1.AND.JQUAD.NE.3) GO TO 100
        ENDIF
        LAYER=MULAYR(NMOD)
CC
        IORENT=IABS(IQ(LP+5))
        XX=Q(LP+21)
        YY=Q(LP+22)
        ZZ=Q(LP+23)
        IF(IORENT.EQ.1) THEN
          X=ZZ     ! DRIFT
          Y=YY     ! WIRE
          Z=XX     ! INTERPLANE
          IF(LAYER.EQ.1) THEN      ! A LAYER
            COST=1./SQRT(1.+WI**2/UI**2)
            XP=ZI+WI/UI*(XX-XI)                        ! BEND
            YP=YI+VI/UI*(XX-XI)                        ! NONBEND
          ELSE    ! BC LAYER
            COST=1./SQRT(1.+WO**2/UO**2)
            XP=ZO+WO/UO*(XX-XO)
            YP=YO+VO/WO*(XX-XO)
          ENDIF
        ELSE IF(IORENT.EQ.2) THEN
          X=ZZ
          Y=XX
          Z=YY
          IF(LAYER.EQ.1) THEN      ! A LAYER
            COST=1./SQRT(1.+WI**2/VI**2)
            XP=ZI+WI/VI*(YY-YI)                        ! BEND
            YP=XI+UI/VI*(YY-YI)                        ! NONBEND
          ELSE    ! BC LAYER
            COST=1./SQRT(1.+WO**2/VO**2)
            XP=ZO+WO/VO*(YY-YO)                        ! BEND
            YP=XO+UO/VO*(YY-YO)                        ! NONBEND
          ENDIF
        ELSE IF(IORENT.EQ.3) THEN
          X=XX
          Y=YY
          Z=ZZ
          IF(LAYER.EQ.1) THEN      ! A LAYER
            COST=1./SQRT(1.+UI**2/WI**2)
            XP=XI+UI/WI*(ZZ-ZI)                        ! BEND
            YP=YI+VI/WI*(ZZ-ZI)                        ! NONBEND
          ELSE    ! BC LAYER
            COST=1./SQRT(1.+UO**2/WO**2)
            XP=XO+UO/WO*(ZZ-ZO)                        ! BEND
            YP=YO+VO/WO*(ZZ-ZO)                        ! NONBEND
          ENDIF
        ELSE IF(IORENT.EQ.4) THEN
          X=YY
          Y=XX
          Z=ZZ
          IF(LAYER.EQ.1) THEN      ! A LAYER
            COST=1./SQRT(1.+VI**2/WI**2)
            XP=YI+VI/WI*(ZZ-ZI)                        ! BEND
            YP=XI+UI/WI*(ZZ-ZI)                        ! NONBEND
          ELSE    ! BC LAYER
            COST=1./SQRT(1.+VO**2/WO**2)
            XP=YO+VO/WO*(ZZ-ZO)                        ! BEND
            YP=XO+UO/WO*(ZZ-ZO)                        ! NONBEND
          ENDIF
        ENDIF
        WLEN=Q(LP+24)
        IF(ABS(X-XP).GT.45.) GO TO 100
        IF(ABS(Y-YP).GT.WLEN+15.) GO TO 100
CCC  SETUP FOR ROTATION CORRECTIONS
        IF(IORENT.EQ.1) THEN
          XXX=Z
          YYY=YP
          ZZZ=XP
        ELSE IF(IORENT.EQ.2) THEN
          XXX=YP
          YYY=Z
          ZZZ=XP
        ELSE IF(IORENT.EQ.3) THEN
          XXX=XP
          YYY=YP
          ZZZ=Z
        ELSE IF(IORENT.EQ.4) THEN
          XXX=YP
          YYY=XP
          ZZZ=Z
        ENDIF
        DXX=0.
        DYY=0.
        DZZ=0.
        CALL MUROTC(IADD,XXX,YYY,ZZZ,DX,DY,DZ,IER)
        IF(IER.EQ.0) THEN
          DXX=DX
          DYY=DY
          DZZ=DZ
        IF(IORENT.EQ.1) THEN
          X=X+DZ
          Y=Y+DY
          Z=Z+DX
        ELSE IF(IORENT.EQ.2) THEN
          X=X+DZ
          Y=Y+DX
          Z=Z+DY
        ELSE IF(IORENT.EQ.3) THEN
          X=X+DX
          Y=Y+DY
          Z=Z+DZ
        ELSE IF(IORENT.EQ.4) THEN
          X=X+DY
          Y=Y+DX
          Z=Z+DZ
        ENDIF
        ENDIF
        ANGLE=ACOS(COST)
CC   FING CLOSEST HIT TO TRACK BEND AND NONBEND
        NTIME=IQ(LP+6)
        VECT(1)=Q(LP+21)
        VECT(2)=Q(LP+22)
        VECT(3)=Q(LP+23)
        IF(IORENT.EQ.1) XTD=YP-YY              ! TIME DIVISION
        IF(IORENT.EQ.2) XTD=YP-XX              ! TIME DIVISION
        IF(IORENT.EQ.3) XTD=YP-YY              ! TIME DIVISION
        IF(IORENT.EQ.4) XTD=YP-XX              ! TIME DIVISION
        IOR=IQ(LP+5)           ! ORIENTATION
        IODD=1                   ! ODD/EVEN CELL
        IF(MOD(NWIR,2).EQ.0) IODD=0
        CALL MUTCOR(IOR,IODD,VECT,XTD,WLEN,TCOR)   ! TIME OF FLIGHT
CC   SEE IF HIT ON TRACK IN BEND VIEW
        IT=0
        ID=0
        XDD=X
        ZDD=Z
        DMIN=ROADB           ! USE BEND ROAD
CC        IF(NQUAD.LE.4) DMIN=ROADB/2.   ! DRIFT AND DRIFT:SMALLER ROAD
        IF(NTIME.GE.1) THEN
        DO 10 K=1,NTIME
CCC    REDO DRIFT DISTANCE CALCULATION
          TIME=Q(LP+8+K)-TCOR                ! DRIFT TIME PLUS TOF
          DD=MUDRFT(TIME,ANGLE,NMOD)          ! DRIFT DISTANCE
          DDD(K)=DD
          DO KK=1,2
            S=ISIGN(KK)
            DEL=ABS(X+S*DD-XP)
            IF(DEL.LT.DMIN) THEN
              IT=K
              ID=KK
              DMIN=DEL
            ENDIF
          ENDDO
   10   CONTINUE
        ELSE
            DEL=ABS(X-XP)
            IF(DEL.LT.DMIN) THEN
              IT=3
              ID=0
              DMIN=DEL
            ENDIF
        ENDIF
CC   SEE IF HIT ON TRACK IN NONBEND VIEW USING TIME DIVISION
        JT=0
        DMIN=ROADNB
        IF(NTIME.GE.1) THEN
        DO 15 K=1,NTIME
          DD=Q(LP+16+K)                  ! TIME DIVISION
          IF(ABS(DD).LE.80000.) THEN
            DEL=ABS(Y+DD-YP)
            IF(DEL.LT.DMIN) THEN
              JT=K
              DMIN=DEL
            ENDIF
          ENDIF
   15   CONTINUE
        ENDIF
CC     REQUIRE HIT TO BE ON TRACK IN both VIEWS
        IF(IT.EQ.0.OR.JT.EQ.0) GO TO 200
        IF(NPTS.GE.NMAX) GO TO 100
CC   HIT ON TRACK
        IF(NM.EQ.0) THEN
          NM=1
          MM(1)=NMOD
          IM(1)=1
        ELSE
          DO K=1,NM
            IF(NMOD.EQ.MM(K)) THEN
              IM(K)=IM(K)+1
              GO TO 765
            ENDIF
          ENDDO
          NM=NM+1
          MM(NM)=NMOD
          IM(NM)=1
765       CONTINUE
        ENDIF
        NCENT=NCENT+1
        NPTS=NPTS+1
        DO K=1,NTIME
          Q(LP+14+K)=DDD(K)                 ! SAVE NEW POSITION
          XDR(K,NPTS)=DDD(K)
        ENDDO
        IQ(LP+7)=1
        Q(LP+26)=DXX
        Q(LP+27)=DYY
        Q(LP+28)=DZZ
        IF(NTIME.LE.1) XDR(2,NPTS)=99999.
        JADD(NPTS)=IADD
        KADD(NPTS)=IADD
        IMUOH(NPTS)=I
        IDRFT(NPTS)=0
        IDELT(NPTS)=JT
        IVERN(NPTS)=0
        ZS(NPTS)=ZDD
        XS(NPTS)=XDD
        IF(NQUAD.LE.4) THEN   ! DON'T FIT WITH A-LAYER
          ZS(NPTS)=9999999.
          XS(NPTS)=9999999.
        ENDIF
        FLAG(NPTS)=0                   ! ALL POINTS ARE GOOD
        IF(NQUAD.GE.5) ILAYER(LAYER)=ILAYER(LAYER)+1
        GO TO 100
200     CONTINUE
CCC   HIT ON TRACK ONLY IN BEND VIEW; IN END BC ONLY IF UNPHYSICAL
        IF(IT.EQ.0) GO TO 100
C        IF(IQUAD.GE.5.AND.JT.EQ.-1.AND.LAYER.GE.2) GO TO 100      ! END REGIONS
        IF(NTOT2.GE.NMAX) GO TO 100
        NTOT2=NTOT2+1
        JADD2(NTOT2)=IADD
        IPOINT2(NTOT2)=I
CCC   SAVE ROTATION CORRECTIONS
        DXX2(NTOT2)=DXX
        DYY2(NTOT2)=DYY
        DZZ2(NTOT2)=DZZ
        X2(NTOT2)=XDD
        XDR2(1,NTOT2)=DDD(1)
        XDR2(2,NTOT2)=DDD(2)
        IF(NTIME.LE.0) XDR2(1,NTOT2)=999999.
        IF(NTIME.LE.1) XDR2(2,NTOT2)=999999.
        Z2(NTOT2)=ZDD
        IF(NQUAD.LE.4) THEN   ! DON'T FIT WITH A-LAYER
          Z2(NPTS)=9999999.
          X2(NPTS)=9999999.
        ENDIF
  100 CONTINUE
CC   SEE IF HITS ONLY ON IN BEND SHOULD BE INCLUDED
      IF(NTOT2.GT.0) THEN
        DO 109 I=1,NTOT2
          DO K=1,NPTS
            IF(IMUOH(K).EQ.IPOINT2(I)) GO TO 109
          ENDDO
          CALL MUADD(JADD2(I),NMOD,NPLN,NWIR,IERR)
CCCC   SEE IF A 3D POINT IN THAT MODULE
          DO K=1,NM
            IF(MM(K).EQ.NMOD.AND.IM(K).GE.1) GO TO 110
          ENDDO
          GO TO 109
 110    CONTINUE
CC  3D POINT IN THE MODULE; ADD ON 2D POINTS
        IF(NPTS.EQ.NMAX) GO TO 108
          NPTS=NPTS+1
          NCENT=NCENT+1
          JADD(NPTS)=JADD2(I)
          KADD(NPTS)=JADD2(I)
          IMUOH(NPTS)=IPOINT2(I)
          IDRFT(NPTS)=0
          IDELT(NPTS)=0
          IVERN(NPTS)=0
          FLAG(NPTS)=0
          XS(NPTS)=X2(I)
          ZS(NPTS)=Z2(I)
          XDR(1,NPTS)=XDR2(1,I)
          XDR(2,NPTS)=XDR2(2,I)
          LP=28*(IPOINT2(I)-1)+LMUOH
          IQ(LP+7)=1
          IF(XDR2(1,I).LT.6.) Q(LP+15)=XDR2(1,I)
          IF(XDR2(2,I).LT.6.) Q(LP+16)=XDR2(2,I)
          Q(LP+26)=DXX2(I)
          Q(LP+27)=DYY2(I)
          Q(LP+28)=DZZ2(I)
          LAYER=MULAYR(NMOD)
          IF(NQUAD.GE.5) ILAYER(LAYER)=ILAYER(LAYER)+1       ! COUNT HITS IN EACH LAYER
 109    CONTINUE
 108    CONTINUE
      ENDIF
CCCC     SEE HOW MANY LAYERS ARE HIT
      NLAYER=0
      IF(ILAYER(1).GE.2) NLAYER=NLAYER+1
      IF(ILAYER(2).GE.2) NLAYER=NLAYER+1
      IF(ILAYER(3).GE.2.OR.ILAYER(4).GE.2) NLAYER=NLAYER+1
      IF(NQUAD.LE.4.AND.NCENT.GE.3) ICENT=NCENT
CCCC   ONLY REFIT ONCE; ONLY REFIT IF BOTH QUADS IN SAME END
      IF(NLAYER.GE.2.AND.REFIT.EQ.0.AND.NQUAD.GE.5) THEN
        CALL MUZBND(IQUAD,ZBEND)       ! MAGNET BEND POINT
        CALL VERXYZ(IVER,VERTEX,NV)
        IF(IQUAD.LE.4) THEN        ! CENTRAL
          VA=VERTEX(3)
          VB=0.
        ELSE
          VA=0.
          VB=VERTEX(3)
        ENDIF
        CALL MUFTBD(NPTS,IQUAD,FLAG,ZBEND,VA,VB,KADD,ZS,XS,XDR,1.,
     A  XONB,ZONB,SLNBO,XINB,ZINB,SLNBI,C1,C2,C12,IS,IVTX)
        IF(C1.LT.3..AND.C2.LT.3.) THEN    ! 'GOOD' FIT
          REFIT=REFIT+1                     ! NUMBER OF GOOD REFITS
          IF(ILAYER(1).GE.1) THEN
            IF(ILAYER(2).GE.1) THEN
              IF(ILAYER(3).GE.1.OR.ILAYER(4).GE.1) THEN
                IFW3=10                                       !ABC
              ELSE
                IFW3=40                                       !AB
              ENDIF
            ELSE
              IF(ILAYER(3).GE.1.OR.ILAYER(4).GE.1) IFW3=30    !AC
            ENDIF
          ELSE
            IF(ILAYER(2).GE.1.AND.ILAYER(3).GE.1) IFW3=20     !BC
            IF(ILAYER(2).GE.1.AND.ILAYER(4).GE.1) IFW3=20
          ENDIF
          DO I=1,NPTS
            IDRFT(I)=IS(I)
          ENDDO
C
C SET UP OUTPUT BY TRANSFORMING TO GLOBAL SYSTEM
C first convert input to slopes
          IF(IQUAD.EQ.1.OR.IQUAD.EQ.3) THEN
            SLI1=WI/UI
            SLO1=WO/UO
            YO=XONB+SLNBO*(XO-ZONB)
            YI=XINB+SLNBI*(XI-ZINB)
          ELSE IF(IQUAD.EQ.2.OR.IQUAD.EQ.4) THEN
            SLI1=WI/VI
            SLO1=WO/VO
            XO=XONB+SLNBO*(YO-ZONB)
            XI=XINB+SLNBI*(YI-ZINB)
          ELSE IF(IQUAD.EQ.5.OR.IQUAD.EQ.7.OR.IQUAD.EQ.9.OR.IQUAD.
     A      EQ.11) THEN
            SLI1=UI/WI
            SLO1=UO/WO
            YO=XONB+SLNBO*(ZO-ZONB)
            YI=XINB+SLNBI*(ZI-ZINB)
          ELSE IF(IQUAD.EQ.6.OR.IQUAD.EQ.8.OR.IQUAD.EQ.10.OR.IQUAD.
     A      EQ.12) THEN
            SLI1=VI/WI
            SLO1=VO/WO
            XO=XONB+SLNBO*(ZO-ZONB)
            XI=XINB+SLNBI*(ZI-ZINB)
          ENDIF
          DD1=SQRT(SLI1**2+SLNBI**2+1.)
          DD2=SQRT(SLO1**2+SLNBO**2+1.)
          IF(IQUAD.EQ.1) THEN         ! CENTRAL +X
            VI=SLNBI/DD1
            UI=1./DD1
            WI=SLI1/DD1
            VO=SLNBO/DD2
            UO=1./DD2
            WO=SLO1/DD2
          ELSE IF(IQUAD.EQ.2) THEN         ! CENTRAL +Y
            UI=SLNBI/DD1
            VI=1./DD1
            WI=SLI1/DD1
            UO=SLNBO/DD2
            VO=1./DD2
            WO=SLO1/DD2
          ELSE IF(IQUAD.EQ.3) THEN        ! CENTRAL -X
            VO=-SLNBO/DD2
            UO=-1./DD2
            WO=-SLO1/DD2
            VI=-SLNBI/DD1
            UI=-1./DD1
            WI=-SLI1/DD1
          ELSE IF(IQUAD.EQ.4) THEN       ! CENTRAL -Y
            UO=-SLNBO/DD2
            VO=-1./DD2
            WO=-SLO1/DD2
            UI=-SLNBI/DD1
            VI=-1./DD1
            WI=-SLI1/DD1
          ELSE IF(IQUAD.EQ.5.OR.IQUAD.EQ.7) THEN   ! NORTH +-X
            VO=-SLNBO/DD2
            WO=-1./DD2
            UO=-SLO1/DD2
            VI=-SLNBI/DD1
            WI=-1./DD1
            UI=-SLI1/DD1
          ELSE IF(IQUAD.EQ.6.OR.IQUAD.EQ.8) THEN   ! NORTH +-Y
            UO=-SLNBO/DD2
            WO=-1./DD2
            VO=-SLO1/DD2
            UI=-SLNBI/DD1
            WI=-1./DD1
            VI=-SLI1/DD1
          ELSE IF(IQUAD.EQ.9.OR.IQUAD.EQ.11) THEN   ! SOUTH +-X
            VI=SLNBI/DD1
            WI=1./DD1
            UI=SLI1/DD1
            VO=SLNBO/DD2
            WO=1./DD2
            UO=SLO1/DD2
          ELSE IF(IQUAD.EQ.10.OR.IQUAD.EQ.12) THEN   ! SOUTH +-Y
            UI=SLNBI/DD1
            WI=1./DD1
            VI=SLI1/DD1
            UO=SLNBO/DD2
            WO=1./DD2
            VO=SLO1/DD2
          ENDIF
        ENDIF
CCCC    SINCE WE HAVE LOOPED OVER CENTRAL QUADS AND HAVE REFIT AN END; SKIP OUT
        GO TO 999
      ENDIF
      ENDDO
 999  CONTINUE
      RETURN
      END
