      SUBROUTINE MUFITASTUB(LMUON,CLIST,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fit an A-layer muon track using information
C-   from vertex, CDC and WAMUS A layer.
C-   Allow for MCS in the Calorimeter.
C-
C-   The fitting parameters are (QUAD=1,3):
C-   (for QUAD=2,4 x<->y))
C    PAR(1-4)  a,b,c,d
C-   PAR(5)    thmcy   scattering angle in Calorimeter in yx plane
C-   PAR(6)    thmcz   scattering angle in Calorimeter in zx plane
C-
C-   Muon track trajectory is:
C-
C-   y=a+b*x                                     |x|<XCAL
C-   z=c+d*x
C-
C-   y=a+b*x+thmcx*(x-XCAL)                      |x|>XCAL
C-   z=c+d*x+thmcy*(x-XCAL)
C-
C-   Inputs  :  LMUON  bank address
C-   Outputs :  CLIST: NDF, CHISQ, fitted parameters and error matrix
C-              IERR (=0 OK, =1 no ZTRK, =2 no VTXT, =3 no CDCT,
C-                    =4 < no WAMUS A layer)
C-                    =5 error inverting ERMAT
C-                    =6 error inverting BMAT
C-                    =7 bad chisq
C-                    =8 bad bdl
C-                    =9 no MTC 
C-   Controls:
C-
C-   Created 16-JUN-1994   Daria Zieminska
C-   Updated  19-MAY-1995   Daria Zieminska  call VXY_BEAM1 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER LMUON,IERR,LMUOT,NVERT,LVERT,LZTRK,LVTXT,LCDCT,KSTAT 
      INTEGER NSEGA,ISEG,IABC,NMOD,NHITSEG,ORIEN,IERSEG,IMUON
      REAL BUF(5),SUBS,ASEG,BSEG,CSEG,DSEG,COV1(2,2),COV2(2,2),CHI1,CHI2
      REAL XMTC,YMTC,ZMTC,DELTAZ1,DELTAZ2,CORR
      INTEGER ITRAK,NP,PRIMAX,RUN,ID,STATUS
      INTEGER NLOOP,NLOOPMX,K,IER,NM,NT,QUAD
      INTEGER LMTCA,LDTRH,GZDTRH
      PARAMETER (NM=14)  ! number of measured quantities
      PARAMETER (NP=6)   ! number of parameters
      PARAMETER (NT=2)   ! number of tracks (CDC, A-layer segment)
C       NM=   4  CDC track parameters
C            +2  MTC point
C            +2  scattering angle "measumements"
C            +4  WAMUS  A layer segment
C            +2  vertex x(or y), z
      REAL MOM,RADLENC,DX,DY,DZ,DXYZ
      REAL COOR0,COORMIDCAL,COORA,SIDE
      REAL CLIST(75),ERVEC(4,4,NT)
      REAL AMAT(NP,NM),YVEC(NP),RVEC(NM),PAR(NP),CHSQ,CHSQ1,CHQOLD
      REAL ERSAV(NM,NM),ERP,SMALL,ERMSC1,ERMSC
      REAL ERMAT(NM,NM),ERALL(NM,NM) 
      REAL XVEC(NM),XVEC1(NM),XVEC2(NM),BMAT(NP,NP),CMAT(NP,NP)
      REAL PHI,THE,SP2,CP2,EPHI,EXY,ETHE,EZ,EZTHE 
      REAL X0,Y0,Z0,R0,CYX,CZX,P1,P2,RATIO(NM)
      REAL VERX,VERY
      INTEGER MXLST,NV,IH,ICONT(10)
      LOGICAL FIRST,DEBUG,FIXCDC
      PARAMETER (MXLST=75)
      REAL RELERR,EPS(NP),CLSTSV(MXLST),VER(3),VERT(14),EVER(3)
      REAL DPHI,DTHE
C
      DATA RELERR/0.001/
      DATA EPS/0.001,0.0001,0.001,0.0001,0.0001,0.0001/
      DATA NLOOPMX/10/
      DATA SMALL/1.0E-8/ 
      DATA EVER/0.05,0.05,2./
      DATA RADLENC/100./
      DATA VERX,VERY/-0.23,0.18/
      DATA FIRST,DEBUG/.TRUE.,.FALSE./
      IF (FIRST) THEN
C
        ERMSC1=0.0136*SQRT(RADLENC)*(1.+0.038*LOG(RADLENC))
        FIRST=.FALSE.
C
        CALL DHDIR('MURECO_RCP','HBOOK_DIRECTORY',IERR,' ')
C
        IF(IERR.NE.0) THEN
          CALL ERRMSG('MURECO','MURECO_HST',
     +      'ERROR SETTING HBOOK DIRECTORY ','W')
        ENDIF
        IF (DEBUG) THEN
          CALL HBOOK1(171,' a CDC',100,-5.,5.,0.)
          CALL HBOOK1(172,' b CDC',100,-5.,5.,0.)
          CALL HBOOK1(173,' c CDC',100,-5.,5.,0.)
          CALL HBOOK1(174,' d CDC',100,-5.,5.,0.)
          CALL HBOOK1(175,' a  MTC',100,-5.,5.,0.)
          CALL HBOOK1(176,' c  MTC',100,-5.,5.,0.)
          CALL HBOOK1(177,' TH XY',100,-5.,5.,0.)
          CALL HBOOK1(178,' TH XZ',100,-5.,5.,0.)
          CALL HBOOK1(179,' a  A ',100,-5.,5.,0.)
          CALL HBOOK1(180,' b  A ',100,-5.,5.,0.)
          CALL HBOOK1(181,' c  A ',100,-5.,5.,0.)
          CALL HBOOK1(182,' d  A ',100,-5.,5.,0.)
          CALL HBOOK1(183,' ver 1',100,-5.,5.,0.)
          CALL HBOOK1(184,' ver 2',100,-5.,5.,0.)
          CALL HBOOK1(185,' DELTA-PHI',100,0.,0.01,0.)
          CALL HBOOK1(186,' DELTA-THETA',100,0.,1.,0.)
          CALL HBOOK1(189,' CHISQ/DF',100,0.,20.,0.)
          CALL HBOOK1(190,' CHISQ/DF ABC',100,0.,20.,0.)
        END IF
      END IF
      IERR=0
      CALL VZERO(CLIST(1),MXLST)
      CALL VZERO(CLSTSV(1),MXLST)
      LMUOT=LQ(LMUON-11)
      ITRAK=IQ(LMUOT-5)
      QUAD=IQ(LMUOT+3)
      MOM=Q(LMUON+14) 
      IF (QUAD.GT.4) GO TO 999
      IF (QUAD.LE.2.OR.QUAD.EQ.5.OR.QUAD.EQ.6.OR.QUAD.EQ.9.OR.QUAD.EQ.
     &  10) THEN
        SIDE=1.
      ELSE
        SIDE=-1.
      END IF
      LVERT=LQ(LMUON-12)
      NVERT=IQ(LVERT-5)
      LZTRK=LQ(LMUON-13)
      IF (LZTRK.EQ.0) THEN
        IERR=1
        GO TO 999
      END IF
      LVTXT=LQ(LZTRK-6)
      LCDCT=LQ(LZTRK-7)
      IF (LCDCT.EQ.0) THEN
        IERR=3
        GO TO 999
      END IF
C
C     CLIST(1)    :  number of words.
C     CLIST(2)    :  degrees of freedom.
C     CLIST(3)    :  chisquare.
C     CLIST(4)    :
C       TO            PAR(1:6)
C     CLIST(9)   :
C     CLIST(10)   :
C        TO       :  error matrix for the fit.
C     CLIST(31)   :
C
   20 KSTAT=-1
C
C  Fill vector of measured quantities and make initial estimate
C  of the parameters and their errors.
C
      CALL VZERO(XVEC,NM)
C
C  CDC track parameters
C
      PHI=Q(LCDCT+6)
      X0 =Q(LCDCT+7)
      Y0 =Q(LCDCT+8)
      R0 =Q(LCDCT+10)
      Z0 =Q(LCDCT+11)
      THE=Q(LCDCT+9)
      SP2=(SIN(PHI))**2
      CP2=(COS(PHI))**2
      EPHI=Q(LCDCT+16) 
      EXY=Q(LCDCT+17) 
      ETHE=Q(LCDCT+18) 
      EZ=Q(LCDCT+19) 
      EZTHE=Q(LCDCT+22) 
      CALL DTRK_FIX(THE,EZ,ETHE,EZTHE)
      ETHE=MAX(ETHE,0.0001)
C
C  Taka's correction to z0
C
      DELTAZ1=-0.149          ! CDC-MU alignment
      DELTAZ2=0.0129*Z0       ! z reconstruction in CDC
      FIXCDC=.TRUE.
      LDTRH=GZDTRH()
      IF (LDTRH .GT. 0 .AND. IBITS(IQ(LDTRH),0,1) .NE. 0) FIXCDC=.FALSE.
      IF (LDTRH .GT. 0 .AND. IBITS(IQ(LDTRH),1,1) .NE. 0) FIXCDC=.FALSE.
      IF (LDTRH .GT. 0 .AND. IBITS(IQ(LDTRH),2,1) .NE. 0) FIXCDC=.FALSE.
      IF (FIXCDC) THEN
        Z0=Z0-DELTAZ2   ! the CDC track is not corrected
      END IF
      IF (MOD(QUAD,2).EQ.1) THEN
        PRIMAX=1
        COOR0=X0
        XVEC(1)=Y0
        XVEC(2)=TAN(PHI)
        XVEC(3)=Z0
        XVEC(4)=COS(THE)/SIN(THE)/COS(PHI)
        ERVEC(1,1,1)=EXY**2*CP2
        ERVEC(2,2,1)=1./CP2**2*EPHI**2
        ERVEC(3,3,1)=EZ**2
        ERVEC(4,4,1)=1./SIN(THE)**4/COS(PHI)**2*ETHE**2+
     1               (XVEC(4)*SIN(PHI)/COS(PHI))**2*EPHI**2
        ERVEC(1,2,1)=0.
        ERVEC(3,4,1)=0.
        ERVEC(1,3,1)=0.
        ERVEC(1,4,1)=0.
        ERVEC(2,3,1)=0.
        ERVEC(2,4,1)=SIN(PHI)/CP2**2*COS(THE)/SIN(THE)*EPHI**2
      ELSE
        PRIMAX=2
        COOR0=Y0
        XVEC(1)=X0
        XVEC(2)=1./TAN(PHI)
        XVEC(3)=Z0
        XVEC(4)=1./TAN(THE)/SIN(PHI)
        ERVEC(1,1,1)=EXY**2*SP2
        ERVEC(2,2,1)=1./SP2**2*EPHI**2
        ERVEC(3,3,1)=EZ**2
        ERVEC(4,4,1)=1./SIN(THE)**4/SIN(PHI)**2*ETHE**2+
     1               (XVEC(4)*COS(PHI)/SIN(PHI))**2*EPHI**2
        ERVEC(1,2,1)=0.
        ERVEC(3,4,1)=0.
        ERVEC(1,3,1)=0.
        ERVEC(1,4,1)=0.
        ERVEC(2,3,1)=0.
        ERVEC(2,4,1)=COS(PHI)/SP2**2*COS(THE)/SIN(THE)*Q(LCDCT+16)**2
      END IF
      ERVEC(2,1,1)=ERVEC(1,2,1)
      ERVEC(3,1,1)=ERVEC(1,3,1)
      ERVEC(4,1,1)=ERVEC(1,4,1)
      ERVEC(3,2,1)=ERVEC(2,3,1)
      ERVEC(4,2,1)=ERVEC(2,4,1)
      ERVEC(4,3,1)=ERVEC(3,4,1)
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
C  Run-dependent beam position
C
      CALL EVNTID(RUN,ID)
C      CALL VXY_BEAM(RUN,VER(1),EVER(1),VER(2),EVER(2),STATUS)
      CALL VXY_BEAM1(VER(3),VER(1),EVER(1),VER(2),EVER(2),STATUS)
      IF (MOD(QUAD,2).EQ.1) THEN
        XVEC(13)=VER(2)
        XVEC(14)=VER(3)
      ELSE
        XVEC(13)=VER(1)
        XVEC(14)=VER(3)
      END IF
C
C  MTC parameters
C
      LMTCA=LQ(LMUON-9)
      IF (LMTCA.LE.0) THEN
        IERR=9
        GO TO 999
      END IF
      IF (Q(LMTCA+15).LT.0.75) THEN  ! require at least 3 hadronic layers
        IERR=9
        GO TO 999
      END IF
      IF (Q(LMTCA+27).LE.0.0) THEN  ! require >0 energy in the last layer 
        IERR=9
        GO TO 999
      END IF
      XMTC =Q(LMTCA+10)
      YMTC =Q(LMTCA+11)
      ZMTC =Q(LMTCA+12)
      DX=XMTC-VER(1)
      DY=YMTC-VER(2)
      DZ=ZMTC-VER(3)
      DXYZ=SQRT(DX**2+DY**2+DZ**2)
      DX=DX/DXYZ
      DY=DY/DXYZ
      DZ=DZ/DXYZ
      IF (MOD(QUAD,2).EQ.1) THEN
        CYX=SQRT(DY**2+DX**2)
        CZX=SQRT(DZ**2+DX**2)
        XVEC(5)=YMTC
        XVEC(6)=ZMTC
        COORMIDCAL=XMTC 
      ELSE
        CYX=SQRT(DY**2+DX**2)
        CZX=SQRT(DZ**2+DY**2)
        XVEC(5)=XMTC
        XVEC(6)=ZMTC
        COORMIDCAL=YMTC 
      END IF
      CALL VZERO(ERALL(1,1),NM*NM)
      DO 1 K=1,4
        CALL UCOPY2(ERVEC(1,K,1),ERALL(1,K),4)
    1 CONTINUE
      ERALL(5,5)=25. 
      ERALL(6,6)=25. 
      CORR=Q(LMTCA+26)/3.
      CORR=MAX(CORR,1.)
      CORR=MIN(CORR,400.)
      ERALL(1,1)=ERALL(1,1)*CORR**2
      ERALL(2,2)=ERALL(2,2)*CORR**2
      RADLENC=Q(LMUON+19)
      ERMSC1=0.0136*SQRT(RADLENC)*(1.+0.038*LOG(RADLENC))
      P1=CYX*MOM 
      P2=CZX*MOM 
      ERMSC=(ERMSC1/P1)**2
      ERALL(7,7)=MAX(ERMSC,1.0E-8)
      ERMSC=(ERMSC1/P2)**2
      ERALL(8,8)=MAX(ERMSC,1.0E-8)
C
C  For VTX-based vertex assume 100 um alignment error wrt CDC
C
      IF (STATUS.EQ.2) THEN   ! default beam position
        EVER(1)=0.1    
      END IF
      ERALL(13,13)=EVER(1)**2+0.0001
      ERALL(14,14)=EVER(3)**2 
C
C  Get A layer segment
C
      NSEGA=0
      IMUON=IQ(LMUON-5)
      NLOOP=0
    8 NLOOP=NLOOP+1
      ISEG=NLOOP
      CALL GTMSEG(IMUON,ISEG,IABC,NMOD,NHITSEG,ORIEN,BUF,SUBS,
     1        BSEG,ASEG,COV1,CHI1,DSEG,CSEG,COV2,CHI2,IERSEG)
      IF (IERSEG.NE.0) GO TO 998
      IF (IABC.EQ.0.AND.ORIEN.LT.3) THEN
        XVEC(9)=ASEG
        XVEC(10)=BSEG
        XVEC(11)=CSEG+DELTAZ1
        XVEC(12)=DSEG
        ERVEC(1,1,2)=COV1(2,2)
        ERVEC(1,1,2)=ERVEC(1,1,2)
        ERVEC(1,2,2)=COV1(1,2)
        ERVEC(2,2,2)=COV1(1,1)
        ERVEC(3,3,2)=COV2(2,2)
        ERVEC(3,3,2)=ERVEC(3,3,2)
        ERVEC(3,4,2)=COV2(1,2)
        ERVEC(4,4,2)=COV2(1,1)
        NSEGA=NSEGA+1
      END IF
      IF (MOD(QUAD,2).EQ.1) THEN
        COORA=Q(LMUOT+8)
      ELSE
        COORA=Q(LMUOT+9)
      END IF
      ERVEC(2,1,2)=ERVEC(1,2,2)
      ERVEC(3,1,2)=ERVEC(1,3,2)
      ERVEC(4,1,2)=ERVEC(1,4,2)
      ERVEC(3,2,2)=ERVEC(2,3,2)
      ERVEC(4,2,2)=ERVEC(2,4,2)
      ERVEC(4,3,2)=ERVEC(3,4,2)
      DO 2 K=1,4
        CALL UCOPY2(ERVEC(1,K,2),ERALL(9,K+8),4)
    2 CONTINUE
C
C  Initial values of parameters
C
      CALL VZERO(PAR(1),NP)
      CALL UCOPY2(XVEC(1),PAR(1),4)     ! a,b,c,d
C
C protection: 0 error means no information; assign big error
C
      DO K=1,NM
        IF (ERALL(K,K).LT.SMALL) THEN
          ERALL(K,K)=999999.
        END IF
      END DO
      CHQOLD=100000.
      CALL VZERO(CLIST(1),MXLST)
      CALL UCOPY2(XVEC(1),RVEC(1),NM)
      CALL VZERO(AMAT(1,1),NM*NP)
C
      AMAT(1,1)=1.
      AMAT(2,2)=1.
      AMAT(3,3)=1.
      AMAT(4,4)=1.
      AMAT(1,5)=1.
      AMAT(2,5)=COORMIDCAL-COOR0
      AMAT(2,6)=1.
      AMAT(3,6)=1.
      AMAT(4,6)=COORMIDCAL-COOR0
      AMAT(5,7)=1.
      AMAT(6,8)=1.
      AMAT(1,9)=1.
      AMAT(2,9)=COORA-COOR0 
      AMAT(5,9)=COORA-COORMIDCAL
      AMAT(2,10)=1.
      AMAT(5,10)=1.
      AMAT(3,11)=1.
      AMAT(4,11)=COORA-COOR0 
      AMAT(6,11)=COORA-COORMIDCAL  
      AMAT(4,12)=1.
      AMAT(6,12)=1.
      AMAT(1,13)=1.
      AMAT(3,14)=1.
      IF (MOD(QUAD,2).EQ.1) THEN
        AMAT(2,13)=VER(1)-X0
        AMAT(4,14)=VER(1)-X0
      ELSE
        AMAT(2,13)=VER(2)-Y0
        AMAT(4,14)=VER(2)-Y0
      END IF
      CALL UCOPY2(ERALL(1,1),ERMAT(1,1),NM*NM)
      CALL UCOPY2(ERMAT(1,1),ERSAV(1,1),NM*NM)
      CALL SMXINV3(ERMAT(1,1),NM,IER)
      IF(IER.NE.0)THEN
        IERR=5
        GO TO 999
      END IF
      CALL MXMLTR(AMAT,ERMAT,BMAT,NP,NM)
      CALL SMXINV3(BMAT,NP,IER)
      IF(IER.NE.0)THEN
        IERR=6
        GO TO 999
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
      CLIST(1)=FLOAT(MXLST)
      CLIST(2)=8.
      CLIST(3)=CHSQ
      IF(CHSQ.GT.CHQOLD)THEN
        IF(NLOOP.LT.NLOOPMX) THEN
          GO TO 8
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
      IF(NLOOP.LT.NLOOPMX) GO TO 8
      KSTAT=MXLST
      GO TO 998
    7 CALL UCOPY2(YVEC(1),PAR(1),NP)
      IF(CHSQ.LE.CHQOLD)THEN
        CALL UCOPY2(CLIST(1),CLSTSV(1),MXLST)
        CHQOLD=CHSQ
      END IF
      KSTAT=MXLST
  998 CONTINUE
      IF (.NOT.DEBUG) GO TO 999
      IF (CLIST(2).LT.1.) GO TO 999
      DO IH=1,14
        RATIO(IH)=XVEC2(IH)/SQRT(ERALL(IH,IH))
        CALL HFILL(170+IH,RATIO(IH),0.,1.)
  666   CONTINUE
      END DO
      CALL EVNTID(RUN,ID)
      IF (LVTXT.GT.0.AND.LCDCT.GT.0) THEN
        DPHI=ABS(Q(LCDCT+6)-Q(LVTXT+6))
        DTHE=ABS(Q(LCDCT+9)-Q(LVTXT+9))
        IF (DPHI.GT.PI) DPHI=TWOPI-DPHI
        CALL HFILL(187,DPHI,0.,1.)
        CALL HFILL(188,Dthe,0.,1.)
      END IF
      CHSQ1=CHSQ/CLIST(2)
      CALL HFILL(189,CHSQ1,0.,1.)
      CALL HFILL(190,q(lmuon+31),0.,1.)
  999 RETURN
      END
