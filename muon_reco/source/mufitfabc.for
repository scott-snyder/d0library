      SUBROUTINE MUFITFABC(LMUON,CLIST,Z0,ZMIDCAL,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fit a muon track through central detector and muon
C-   system. Allow for MCS in the Calorimeter. This version applies to
C-   small angle tracks and uses information from FDC and WAMUS.
C-
C-   The fitting parameters are:
C    PAR(1-4)  a,b,c,d
C-   PAR(5)    thmcx   scattering angle in Calorimeter in xz plane
C-   PAR(6)    thmcy   scattering angle in Calorimeter in yz plane
C-   PAR(7)    1/P
C-
C-   Muon track trajectory is:
C-
C-   x=a+b*z                                     |z|<ZCAL
C-   y=c+d*z
C-
C-   x=a+b*z+thmcx*(z-ZCAL)                  ZFE<|z|<ZCAL
C-   y=c+d*z+thmcy*(z-ZCAL)
C-
C-   x=a+b*z+thmcx*(z-ZCAL)+CX/p*(z-ZFE)     |z|>ZFE
C-   y=c+d*z+thmcy*(z-ZCAL)+CY/p*(z-ZFE)
C-
C-   Inputs  :  LMUON  bank address
C-   Outputs :  CLIST: NDF, CHISQ, fitted parameters and error matrix
C-              IERR (=0 OK, =1 no ZTRK, =2 no VTXT, =3 no FDCT,
C-                    =4 < no WAMUS A layer)
C-                    =5 error inverting ERMAT
C-                    =6 error inverting BMAT
C-                    =7 bad chisq
C-   Controls:
C-
C-   Created  11-SEP-1992   Daria Zieminska
C-   adapted from J. Martin's muon fitting program for E672
C-   Updated   7-JUL-1994   Daria Zieminska add vertex,
C-                          protect against 0 errors
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER LMUON,IERR,LMUOT,LVERT,LZTRK,LVTXT,LFDCT,KSTAT,IFDCT
      INTEGER ITRAK,NP,IH,PRIMAX,RUN,ID,STATUS,NV,NVERT,ICONT(10)
      INTEGER NLOOP,NLOOPMX,K,NPASS,IER,NM,NT,QUAD
      PARAMETER (NM=16)  ! number of measured quantities
      PARAMETER (NP=7)   ! number of parameters
      PARAMETER (NT=3)   ! number of tracks (FDC, MUOT_A, MUOT_BC)
C       NM=   4  FDC track parameters
C            +4  WAMUS  A LAYER PARAMETERS
C            +2  scattering angle "measumements"
C            +4  WAMUS  BC PARAMETERS
C            +2  vertex x(or y), z
      REAL VERX,VERY
      REAL MOM,MOMI,MOMF,RADLENC,PFACT(2),X,Y,Z,DX,DY,DZ,CXZ,CYZ
      REAL Z0,ZC,ZCAL,ZMIDCAL,ZA,SIDE,ZFE
      REAL CLIST(75),ERVEC(4,4,NT)
      REAL AMAT(NP,NM),YVEC(NP),RVEC(NM),PAR(NP),CHSQ,CHSQ1,CHQOLD
      REAL ERSAV(NM,NM),ERP,SMALL,ERMSC1,ERMSC
      REAL ERMAT(NM,NM),ERALL(NM,NM)
      REAL XVEC(NM),XVEC1(NM),XVEC2(NM),BMAT(NP,NP),CMAT(NP,NP)
      REAL RATIO(NM),PHIF,PHIMU,THEF,THEMU,DPHI,DTHE
      INTEGER MXLST
      LOGICAL FIRST,DEBUG
      PARAMETER (MXLST=75)
      REAL RELERR,EPS(NP),MXINT,CLSTSV(MXLST),VER(3),VERT(14),EVER(3)
      INTEGER NSEGA,NSEGBC,ISEG,IABC,NMOD,NHITSEG,ORIEN,IERSEG,IMUON
      REAL BUF(5),SUBS,ASEG,BSEG,CSEG,DSEG,COV1(2,2),COV2(2,2),CHI1,CHI2
C
      DATA RELERR/0.001/
      DATA EPS/0.001,0.0001,0.001,0.0001,0.0001,0.0001,0.0001/
      DATA MXINT,NLOOPMX/3,3/
      DATA SMALL/1.0E-8/
      DATA ZCAL/250./
      DATA RADLENC/200./
      DATA VERX,VERY/-0.23,0.18/
      DATA FIRST,DEBUG/.TRUE.,.FALSE./
      IF (FIRST) THEN
C
C  MCS errors for p=1 GeV
C
        ERMSC1=0.0136*SQRT(RADLENC)*(1.+0.038*LOG(RADLENC))
        CALL DHDIR('MURECO_RCP','HBOOK_DIRECTORY',IERR,' ')
C
        IF(IERR.NE.0) THEN
          CALL ERRMSG('MURECO','MURECO_HST',
     +      'ERROR SETTING HBOOK DIRECTORY ','W')
        ENDIF
        IF (DEBUG) THEN
          CALL HBOOK1(1151,' a FDC',100,-5.,5.,0.)
          CALL HBOOK1(1152,' b FDC',100,-5.,5.,0.)
          CALL HBOOK1(1153,' c FDC',100,-5.,5.,0.)
          CALL HBOOK1(1154,' d FDC',100,-5.,5.,0.)
          CALL HBOOK1(1155,' a  A ',100,-5.,5.,0.)
          CALL HBOOK1(1156,' b  A ',100,-5.,5.,0.)
          CALL HBOOK1(1157,' c  A ',100,-5.,5.,0.)
          CALL HBOOK1(1158,' d  A ',100,-5.,5.,0.)
          CALL HBOOK1(1159,' TH XY',100,-5.,5.,0.)
          CALL HBOOK1(1160,' TH XZ',100,-5.,5.,0.)
          CALL HBOOK1(1161,' a  BC',100,-5.,5.,0.)
          CALL HBOOK1(1162,' b  BC',100,-5.,5.,0.)
          CALL HBOOK1(1163,' c  BC',100,-5.,5.,0.)
          CALL HBOOK1(1164,' d  BC',100,-5.,5.,0.)
          CALL HBOOK1(1190,' CHISQ/DF FDC',100,0.,200.,0.)
          CALL HBOOK1(1191,' phi(FDC)-phi(MUOT)',100,-0.1,0.1,0.)
          CALL HBOOK1(1192,' theta(FDC)-theta(MUOT)',100,-0.1,0.1,0.)
        END IF
        FIRST=.FALSE.
      END IF
      IERR=0
      CALL VZERO(CLIST(1),MXLST)
      CALL VZERO(CLSTSV(1),MXLST)
      LMUOT=LQ(LMUON-11)
      ITRAK=IQ(LMUOT-5)
      QUAD=IQ(LMUOT+3)
      IF (QUAD.LT.5) GO TO 999
      PRIMAX=3
      SIDE=Q(LMUOT+13)
      MOM=MAX(ABS(Q(LMUOT+23)),3.001)
      MOMI=MOM
      MOMF=MOM-3.0
      MOMF=SIGN(MOMF,Q(LMUOT+23))
      LZTRK=LQ(LMUON-13)
      IF (LZTRK.EQ.0) THEN
        IERR=1
        GO TO 999
      END IF
      LVTXT=LQ(LZTRK-6)
      IF (LVTXT.EQ.0) THEN
        IERR=2
      END IF
      LFDCT=LQ(LZTRK-8)
      IF (LFDCT.EQ.0) THEN
        IERR=3
        GO TO 999
      END IF
      LVERT=LQ(LMUON-12)
      NVERT=IQ(LVERT-5)
C
C  Get vertex
C
      CALL GTVERH(ICONT)
      NV=ICONT(2)
      IF (NV.GT.0) THEN
        CALL GTVERT(NVERT,VERT)
        VER(1)=VERT(3)
        VER(2)=VERT(4)
        VER(3)=VERT(5)
        EVER(1)=MAX(VERT(6),0.001)  ! make sure it's not 0
        EVER(2)=MAX(VERT(7),0.001)
        EVER(3)=VERT(8)
        IF (LVERT.GT.0) THEN
          VER(3)=Q(LVERT+5)
          EVER(3)=Q(LVERT+8)
        END IF
        EVER(3)=MAX(EVER(3),0.1)    ! make sure it's not 0
      ELSE
        VER(1)=VERX
        VER(2)=VERY
        VER(3)=0.
        EVER(1)=0.5
        EVER(2)=0.5
        EVER(3)=50.
      END IF
C
C     CLIST(1)    :  number of words.
C     CLIST(2)    :  degrees of freedom.
C     CLIST(3)    :  chisquare.
C     CLIST(4)    :
C       TO            PAR(1:7)
C     CLIST(10)   :
C     CLIST(11)   :
C        TO       :  error matrix for the fit.
C     CLIST(59)   :
C
   20 KSTAT=-1
C
C  Fill vector of measured quantities and make initial estimate
C  of the parameters and their errors.
C
      CALL VZERO(XVEC,NM)
C
C  Run-dependent beam position
C
      CALL EVNTID(RUN,ID)
      CALL VXY_BEAM1(VER(3),VER(1),EVER(1),VER(2),EVER(2),STATUS)
      XVEC(15)=VER(1)
      XVEC(16)=VER(2)
C
C  FDC track parameters
C
      IFDCT=IQ(LFDCT-5)
      PHIF=Q(LFDCT+6)
      THEF=Q(LFDCT+22)
      CALL FGETZ0(IFDCT,Z0)
      XVEC(1)=Q(LFDCT+4)
      XVEC(2)=Q(LFDCT+7)   ! Bf
      XVEC(3)=Q(LFDCT+5)
      XVEC(4)=Q(LFDCT+8)   ! Df
      ERVEC(1,1,1)=Q(LFDCT+9)
      ERVEC(2,2,1)=Q(LFDCT+16)
      ERVEC(3,3,1)=Q(LFDCT+13)
      ERVEC(4,4,1)=Q(LFDCT+18)
      ERVEC(1,2,1)=Q(LFDCT+11)
      ERVEC(3,4,1)=Q(LFDCT+15)
      ERVEC(1,3,1)=Q(LFDCT+10)
      ERVEC(1,4,1)=Q(LFDCT+12)
      ERVEC(2,3,1)=Q(LFDCT+14)
      ERVEC(2,4,1)=Q(LFDCT+17)
      ERVEC(2,1,1)=ERVEC(1,2,1)
      ERVEC(3,1,1)=ERVEC(1,3,1)
      ERVEC(4,1,1)=ERVEC(1,4,1)
      ERVEC(3,2,1)=ERVEC(2,3,1)
      ERVEC(4,2,1)=ERVEC(2,4,1)
      ERVEC(4,3,1)=ERVEC(3,4,1)
  777 CONTINUE
      ZC=ZCAL-ABS(Z0)
      ZC=SIGN(ZC,SIDE)
C
C  MUOT track parameters
C
      ZA=Q(LMUOT+10)
      X=Q(LMUOT+8)
      Y=Q(LMUOT+9)
      Z=Q(LMUOT+10)
      DX=Q(LMUOT+14)
      DY=Q(LMUOT+15)
      DZ=Q(LMUOT+16)
      PHIMU=ATAN2(DY,DX)
      IF (DY.LT.0.) PHIMU=PHIMU+TWOPI
      THEMU=ACOS(DZ)
      CXZ=SQRT(DX**2+DZ**2)
      CYZ=SQRT(DY**2+DZ**2)
      XVEC(5)=X
      XVEC(6)=DX/DZ
      XVEC(7)=Y
      XVEC(8)=DY/DZ
      ERVEC(1,2,2)=0.
      ERVEC(1,3,2)=0.
      ERVEC(1,4,2)=0.
      ERVEC(2,3,2)=0.
      ERVEC(2,4,2)=0.
      ERVEC(3,4,2)=0.
      ERVEC(1,1,2)=99.
      ERVEC(2,2,2)=99.
      ERVEC(3,3,2)=99.
      ERVEC(4,4,2)=99.
C
C  get parameters and cov matrix for BC segment
C
      X=Q(LMUOT+11)
      Y=Q(LMUOT+12)
      Z=Q(LMUOT+13)
      DX=Q(LMUOT+17)
      DY=Q(LMUOT+18)
      DZ=Q(LMUOT+19)
      XVEC(11)=X
      XVEC(12)=DX/DZ
      XVEC(13)=Y
      XVEC(14)=DY/DZ
      ERVEC(1,1,3)=0.38  ! sigma=1.5cm
      ERVEC(2,2,3)=0.00025/3.
      ERVEC(3,3,3)=0.015  ! sigma=0.3cm
      ERVEC(4,4,3)=0.00001/3.
      ERVEC(1,2,3)=0.
      ERVEC(1,3,3)=0.
      ERVEC(1,4,3)=0.
      ERVEC(2,3,3)=0.
      ERVEC(2,4,3)=0.
      ERVEC(3,4,3)=0.

C
C  Get A and BC segments
C
      NSEGA=0
      NSEGBC=0
      IMUON=IQ(LMUON-5)
      DO ISEG=1,20
        CALL GTMSEG(IMUON,ISEG,IABC,NMOD,NHITSEG,ORIEN,BUF,SUBS,
     1        BSEG,ASEG,COV1,CHI1,DSEG,CSEG,COV2,CHI2,IERSEG)
        IF (IERSEG.NE.0) GO TO 888
        IF (IABC.EQ.0) THEN
          XVEC(5)=ASEG
          XVEC(6)=BSEG
          XVEC(7)=CSEG
          XVEC(8)=DSEG
          ERVEC(1,1,2)=COV1(2,2)
c          ERVEC(1,1,2)=ERVEC(1,1,2)+0.09  ! add 3mm alignment error
          ERVEC(1,2,2)=COV1(1,2)
          ERVEC(2,2,2)=COV1(1,1)
          ERVEC(3,3,2)=COV2(2,2)
c          ERVEC(3,3,2)=ERVEC(3,3,2)+0.09  ! add 3mm alignment error
          ERVEC(3,4,2)=COV2(1,2)
          ERVEC(4,4,2)=COV2(1,1)
          NSEGA=NSEGA+1
        END IF
        IF (IABC.EQ.1) THEN
          XVEC(11)=ASEG
          XVEC(12)=BSEG
          XVEC(13)=CSEG
          XVEC(14)=DSEG
          ERVEC(1,1,3)=COV1(2,2)
c          ERVEC(1,1,3)=ERVEC(1,1,3)+0.09  ! add 3mm alignment error
          ERVEC(1,2,3)=COV1(1,2)
          ERVEC(2,2,3)=COV1(1,1)
          ERVEC(3,3,3)=COV2(2,2)
c          ERVEC(3,3,3)=ERVEC(3,3,3)+0.09  ! add 3mm alignment error
          ERVEC(3,4,3)=COV2(1,2)
          ERVEC(4,4,3)=COV2(1,1)
          NSEGBC=NSEGBC+1
        END IF
      END DO
  888 CONTINUE
      IF (NSEGA.EQ.0) IERR=4
      ERVEC(2,1,2)=ERVEC(1,2,2)
      ERVEC(3,1,2)=ERVEC(1,3,2)
      ERVEC(4,1,2)=ERVEC(1,4,2)
      ERVEC(3,2,2)=ERVEC(2,3,2)
      ERVEC(4,2,2)=ERVEC(2,4,2)
      ERVEC(4,3,2)=ERVEC(3,4,2)
      ERVEC(2,1,3)=ERVEC(1,2,3)
      ERVEC(3,1,3)=ERVEC(1,3,3)
      ERVEC(4,1,3)=ERVEC(1,4,3)
      ERVEC(3,2,3)=ERVEC(2,3,3)
      ERVEC(4,2,3)=ERVEC(2,4,3)
      ERVEC(4,3,3)=ERVEC(3,4,3)
C
C  Initial values of parameters
C
      ZFE=Q(LMUOT+13)
      ZMIDCAL=SIGN(ZCAL,SIDE)
      CALL VZERO(PAR(1),NP)
      CALL UCOPY2(XVEC(1),PAR(1),4)
      PAR(7)=1./MOMF
      NLOOP=0
    8 NLOOP=NLOOP+1
      CALL VZERO(ERALL(1,1),NM*NM)
      DO 1 K=1,4
        CALL UCOPY2(ERVEC(1,K,1),ERALL(1,K),4)
        CALL UCOPY2(ERVEC(1,K,2),ERALL(5,K+4),4)
        CALL UCOPY2(ERVEC(1,K,3),ERALL(11,K+10),4)
    1 CONTINUE
      ERMSC=Q(LMUON+21)**2
      RADLENC=Q(LMUON+19)
      ERMSC1=0.0136*SQRT(RADLENC)*(1.+0.038*LOG(RADLENC))
      ERMSC=(ERMSC1/(3.+1./ABS(PAR(7))))**2
      ERALL(9,9)=MAX(ERMSC,1.0E-6)
      ERALL(10,10)=MAX(ERMSC,1.0E-6)
      ERALL(15,15)=(CXZ*EVER(3))**2
      ERALL(16,16)=(CYZ*EVER(3))**2
      NPASS=0
C
C protection: 0 error means no information; assign big error
C
      DO K=1,NM
        IF (ERALL(K,K).LT.SMALL) THEN
          ERALL(K,K)=999999.
        END IF
      END DO
      CHQOLD=1000000.
   11 CALL VZERO(CLIST(1),MXLST)
      CALL UCOPY2(XVEC(1),RVEC(1),NM)
      CALL VZERO(AMAT(1,1),NM*NP)
      IF (NPASS.GT.0) THEN
        DZ=1./SQRT(1.+XVEC1(12)**2+XVEC1(14)**2)
        DZ=SIGN(DZ,Q(LMUOT+19))
        DX=DZ*XVEC1(12)
        DY=DZ*XVEC1(14)
        X=XVEC1(11)
        Y=XVEC1(13)
      END IF
      MOMF=-MOMF
      IF (ABS(DZ).GT.0.99) THEN ! protection againts unphysical input
        IERR=8
        GO TO 999
      END IF
      CALL MUBDL(MOMF,QUAD,PRIMAX,X,Y,Z,DX,DY,DZ,PFACT)
      IF (ABS(PFACT(1)).LT.0.01.AND.ABS(PFACT(2)).LT.0.01) THEN
        IERR=8
        GO TO 998
      END IF
C
      AMAT(1,1)=1.
      AMAT(2,2)=1.
      AMAT(3,3)=1.
      AMAT(4,4)=1.
      AMAT(1,5)=1.
      AMAT(2,5)=ZA-Z0
      AMAT(2,6)=1.
      AMAT(3,7)=1.
      AMAT(4,7)=ZA-Z0
      AMAT(4,8)=1.
      AMAT(5,5)=ZA-ZMIDCAL
      AMAT(6,7)=ZA-ZMIDCAL
      AMAT(5,6)=1.
      AMAT(6,8)=1.
      AMAT(5,9)=1.
      AMAT(6,10)=1.
      AMAT(1,11)=1.
      AMAT(2,11)=ZFE-Z0
      AMAT(2,12)=1.
      AMAT(3,13)=1.
      AMAT(4,13)=ZFE-Z0
      AMAT(4,14)=1.
      AMAT(5,11)=ZFE-ZMIDCAL
      AMAT(6,13)=ZFE-ZMIDCAL
      AMAT(5,12)=1.
      AMAT(6,14)=1.
      AMAT(7,12)=PFACT(1)
      AMAT(7,14)=PFACT(2)
      AMAT(1,15)=1.
      AMAT(3,16)=1.
      AMAT(2,15)=VER(3)-Z0
      AMAT(4,16)=VER(3)-Z0
      CALL UCOPY2(ERALL(1,1),ERMAT(1,1),NM*NM)
      CALL UCOPY2(ERMAT(1,1),ERSAV(1,1),NM*NM)
      CALL SMXINV3(ERMAT(1,1),NM,IER)
      IF(IER.NE.0)THEN
        IERR=5
        GO TO 998
      END IF
      CALL MXMLTR(AMAT,ERMAT,BMAT(1,1),NP,NM)
      CALL SMXINV3(BMAT,NP,IER)
      IF(IER.NE.0)THEN
        IERR=6
        GO TO 998
      END IF
      CALL UCOPY2(BMAT(1,1),CMAT(1,1),NP*NP)
      CALL MXMPY(ERMAT(1,1),RVEC(1),XVEC1(1),NM,NM,1)
      CALL MXMPY2(AMAT(1,1),XVEC1(1),XVEC2(1),NP,NM,1)
      CALL MXMPY(BMAT(1,1),XVEC2(1),YVEC(1),NP,NP,1)
C
C  Calculate residuals and chisquare.
C
      CALL MXMPY(AMAT(1,1),YVEC(1),XVEC1(1),NM,NP,1)
      CALL VSUB(RVEC(1),XVEC1(1),XVEC2(1),NM)
      CALL MXMLTR(XVEC2(1),ERMAT(1,1),CHSQ,1,NM)
      CALL UCOPY2(YVEC(1),CLIST(4),NP)
      CALL UCOPY2(CMAT(1,1),CLIST(11),NP*NP)
      CLIST(1)=MXLST
      CLIST(2)=9.
      IF (IERR.EQ.4) THEN
        CLIST(2)=5.
      END IF
      CLIST(3)=CHSQ
      NPASS=NPASS+1
      MOMF=ABS(CLIST(10))
      MOMF=1./MAX(MOMF,0.001)
      MOMF=SIGN(MOMF,CLIST(10))
      IF(CHSQ.GT.CHQOLD)THEN
        IF(NLOOP.LT.NLOOPMX) THEN
          GO TO 8
        ELSE
          IF (CHSQ.GT.1000000.) THEN
            IERR=7
            GO TO 998
          END IF
        END IF
        CALL UCOPY2(CLSTSV(1),CLIST(1),MXLST)
        KSTAT=MXLST
        GO TO 998
      END IF
      DO 6 K=1,NP
        ERP=YVEC(K)-PAR(K)
        IF(ABS(YVEC(K)).GT.1.0E-6)THEN
          IF(ABS(ERP/YVEC(K)).LT.RELERR) GO TO 6
        END IF
        IF (ABS(ERP).GT.EPS(K)) GO TO 7
    6 CONTINUE
C      IF(NLOOP.LT.2) GO TO 8
      KSTAT=MXLST
      GO TO 998
    7 CALL UCOPY2(YVEC(1),PAR(1),NP)
      IF(CHSQ.LE.CHQOLD)THEN
        CALL UCOPY2(CLIST(1),CLSTSV(1),MXLST)
        CHQOLD=CHSQ
      END IF
      IF(NPASS.LT.MXINT) GO TO 11
      KSTAT=MXLST
  998 CONTINUE
      IF (.NOT.DEBUG) GO TO 999
      CALL EVNTID(RUN,ID)
      DPHI=PHIF-PHIMU
      IF (DPHI.GT.PI) DPHI=DPHI-TWOPI
      IF (DPHI.LT.-PI) DPHI=TWOPI+DPHI
      DTHE=THEF-THEMU
      WRITE(0,100) RUN,ID,IERR,Q(LMUOT+23),MOMF,CHSQ,DPHI,DTHE,
     1             PHIMU,THEMU
  100 FORMAT(1x,2I7,I5,2F7.1,2X,F9.1,2X,2F10.3,2X,2F10.3)
      IF (CLIST(2).LT.1.) GO TO 999
      CHSQ1=CHSQ/CLIST(2)
C      IF (IQ(LMUOT+7).NE.0) GO TO 999
      DO IH=1,NM
        RATIO(IH)=XVEC2(IH)/SQRT(ERALL(IH,IH))
        CALL HFILL(1150+IH,RATIO(IH),0.,1.)
      END DO
      CALL HFILL(1190,CHSQ1,0.,1.)
      CALL HFILL(1191,DPHI,0.,1.)
      CALL HFILL(1192,DTHE,0.,1.)
  999 RETURN
      END
