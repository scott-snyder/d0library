      SUBROUTINE MUFITDABC(LMUON,CLIST,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fit a muon track through central detector and muon 
C-   system. Allow for MCS in the Calorimeter. This version applies to 
C-   central tracks and uses information from vertex, CDC and WAMUS.
C-   
C-   The fitting parameters are (QUAD=1,3): 
C-   (for QUAD=2,4 x<->y))
C    PAR(1-4)  a,b,c,d 
C-   PAR(5)    thmcy   scattering angle in Calorimeter in yx plane 
C-   PAR(6)    thmcz   scattering angle in Calorimeter in zx plane 
C-   PAR(7)    1/P
C-
C-   Muon track trajectory is:
C-   
C-   y=a+b*x                                     |x|<XCAL
C-   z=c+d*x
C-
C-   y=a+b*x+thmcx*(x-XCAL)                  XFE<|x|<XCAL
C-   z=c+d*x+thmcy*(x-XCAL)
C-
C-   y=a+b*x+thmcx*(x-XCAL)+CY/p*(x-XFE)     |x|>XFE 
C-   z=c+d*x+thmcy*(x-XCAL)+CZ/p*(x-XFE)
C-
C-   Inputs  :  LMUON  bank address 
C-   Outputs :  CLIST: NDF, CHISQ, fitted parameters and error matrix
C-              IERR (=0 OK, =1 no ZTRK, =2 no VTXT, =3 no CDCT, 
C-                    =4 < no WAMUS A layer)
C-                    =5 error inverting ERMAT
C-                    =6 error inverting BMAT 
C-                    =7 bad chisq
C-                    =8 bad bdl
C-   Controls: 
C-
C-   Created  11-SEP-1992   Daria Zieminska
C-   adapted from J. Martin's muon fitting program for E672
C-   Updated  13-FEB-1993   Daria Zieminska  fix NBC 
C-   Updated  24-JUN-1994   Daria Zieminska  correction to CDC z 
C-   Updated   6-JUL-1994   Daria Zieminska  protection against unphysical
C-                          input 
C-   Updated  31-JAN-1995   Daria Zieminska  call VXY_BEAM1 instead of
C-   VXY_BEAM; remove the CDC z0 correction (it is now in the CDC code) 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER LMUON,IERR,LMUOT,NVERT,LVERT,LZTRK,LVTXT,LCDCT,KSTAT 
      INTEGER LDTRH,GZDTRH
      INTEGER NSEGA,NSEGBC,ISEG,IABC,NMOD,NHITSEG,ORIEN,IERSEG,IMUON
      REAL BUF(5),SUBS,ASEG,BSEG,CSEG,DSEG,COV1(2,2),COV2(2,2),CHI1,CHI2
      INTEGER ITRAK,NP,PRIMAX,RUN,ID,STATUS
      INTEGER NLOOP,NLOOPMX,K,NPASS,IER,NM,NT,QUAD
      PARAMETER (NM=16)  ! number of measured quantities
      PARAMETER (NP=7)   ! number of parameters
      PARAMETER (NT=3)   ! number of tracks (CDC, MUOT_A, MUOT_BC)
C       NM=   4  CDC track parameters
C            +4  WAMUS  A LAYER PARAMETERS 
C            +2  scattering angle "measumements" 
C            +4  WAMUS  BC PARAMETERS 
C            +2  vertex x(or y), z
      REAL MOM,MOMI,MOMF,RADLENC,PFACT(2),X,Y,Z,DX,DY,DZ,PSIGN 
      REAL COOR0,COORC,COORCAL,COORMIDCAL,COORA,SIDE,COORFE 
      REAL CLIST(75),ERVEC(4,4,NT) 
      REAL AMAT(NP,NM),YVEC(NP),RVEC(NM),PAR(NP),CHSQ,CHSQ1,CHQOLD 
      REAL ERSAV(NM,NM),ERP,SMALL,ERMSC1,ERMSC
      REAL ERMAT(NM,NM),ERALL(NM,NM) 
      REAL XVEC(NM),XVEC1(NM),XVEC2(NM),BMAT(NP,NP),CMAT(NP,NP)
      REAL PHI,THE,SP2,CP2,EPHI,EXY,ETHE,EZ,EZTHE 
      REAL X0,Y0,Z0,R0,CYX,CZX,P1,P2,RATIO(16)
      REAL VERX,VERY,DELTAZ1,DELTAZ2
      INTEGER PLANES_ALL,PLANES_FIT,IAPLN,IFPLN
      INTEGER NA,NB,NC,ND,NA_ADJ,NBC_ADJ,SA,SB,SC,NBC
      INTEGER MXLST,NV,IH,ICONT(10)
      LOGICAL FIRST,DEBUG,FIXCDC
      PARAMETER (MXLST=75)
      REAL RELERR,EPS(NP),MXINT,CLSTSV(MXLST),VER(3),VERT(14),EVER(3)
      REAL DPHI,DTHE
C
      DATA RELERR/0.001/
      DATA EPS/0.001,0.0001,0.001,0.0001,0.0001,0.0001,0.0001/
      DATA MXINT,NLOOPMX/3,3/
      DATA SMALL/1.0E-8/ 
      DATA COORCAL/150./  
      DATA EVER/0.05,0.05,2./
      DATA RADLENC/100./   
      DATA VERX,VERY/-0.23,0.18/   
      DATA FIRST,DEBUG/.TRUE.,.false./
      IF (FIRST) THEN
C
        ERMSC1=0.0136*SQRT(RADLENC)*(1.+0.038*LOG(RADLENC))
        FIRST=.FALSE.
C
        CALL DHDIR('MURECO_RCP','HBOOK_DIRECTORY',IERR,' ')
C
      IF(IERR.NE.0) THEN
         CALL ERRMSG('MURECO','MURECO_HST',
     +     'ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
      IF (DEBUG) THEN
        CALL HBOOK1(171,' a CDC',100,-5.,5.,0.)
        CALL HBOOK1(172,' b CDC',100,-5.,5.,0.)
        CALL HBOOK1(173,' c CDC',100,-5.,5.,0.)
        CALL HBOOK1(174,' d CDC',100,-5.,5.,0.)
        CALL HBOOK1(175,' a  A ',100,-5.,5.,0.)
        CALL HBOOK1(176,' b  A ',100,-5.,5.,0.)
        CALL HBOOK1(177,' c  A ',100,-5.,5.,0.)
        CALL HBOOK1(178,' d  A ',100,-5.,5.,0.)
        CALL HBOOK1(179,' TH XY',100,-5.,5.,0.)
        CALL HBOOK1(180,' TH XZ',100,-5.,5.,0.)
        CALL HBOOK1(181,' a  BC',100,-5.,5.,0.)
        CALL HBOOK1(182,' b  BC',100,-5.,5.,0.)
        CALL HBOOK1(183,' c  BC',100,-5.,5.,0.)
        CALL HBOOK1(184,' d  BC',100,-5.,5.,0.)
        CALL HBOOK1(185,' ver 1',100,-5.,5.,0.)
        CALL HBOOK1(186,' ver 2',100,-5.,5.,0.)
        CALL HBOOK1(187,' DELTA-PHI',100,0.,0.01,0.)
        CALL HBOOK1(188,' DELTA-THETA',100,0.,1.,0.)
        CALL HBOOK1(189,' CHISQ/DF',100,0.,20.,0.)
      END IF
      END IF
      IERR=0
      CALL VZERO(CLIST(1),MXLST)
      CALL VZERO(CLSTSV(1),MXLST)
      LMUOT=LQ(LMUON-11)
      ITRAK=IQ(LMUOT-5)
      QUAD=IQ(LMUOT+3)
      IF (QUAD.GT.12) GO TO 999
      CALL MUCPLN(LMUOT,PLANES_ALL,PLANES_FIT,IAPLN,IFPLN)
      CALL MUUPLN(PLANES_FIT,NA,NB,NC,ND,SA,SB,SC,NA_ADJ,NBC_ADJ)
      IF (IQ(LMUOT+4).EQ.1.OR.IQ(LMUOT+4).EQ.11) THEN  
        IERR=4   ! mising A layer
      END IF
      IF (QUAD.LE.2.OR.QUAD.EQ.5.OR.QUAD.EQ.6.OR.QUAD.EQ.9.OR.QUAD.EQ.
     &  10) THEN
        SIDE=1.
      ELSE
        SIDE=-1.
      END IF
      COORMIDCAL=SIGN(COORCAL,SIDE)
      MOM=MAX(ABS(Q(LMUOT+23)),3.001)    
      PSIGN=Q(LMUOT+23)
      MOMI=MOM
      MOMF=MOM-3.0
      MOMF=SIGN(MOMF,PSIGN)
      LVERT=LQ(LMUON-12)
      NVERT=IQ(LVERT-5)
      LZTRK=LQ(LMUON-13)
      IF (LZTRK.EQ.0) THEN
        IERR=1
        GO TO 999
      END IF
      LVTXT=LQ(LZTRK-6)
      IF (LVTXT.EQ.0) THEN
        IERR=2
      END IF
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
C       TO            PAR(1:7)
C     CLIST(10)   :  
C     CLIST(11)   : 
C        TO       :  error matrix for the fit.
C     CLIST(59)   :
C
 20    KSTAT=-1
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
      ETHE=MAX(ETHE,0.0001)
      EZ=SQRT(EZ**2+(0.004*Z0)**2+0.013**2) ! add error from Taka's correction
      IF (MOD(QUAD,2).EQ.1) THEN
        PRIMAX=1
        COORFE=Q(LMUOT+11)
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
 777  CONTINUE
C
C  MUOT track parameters
C
C  Get parameters and cov matrix for A layer segment;
C  (temporary). In future will use segment banks.
C
CCC
ccc      Q(LMUOT+8)=-77.5
ccc      Q(LMUOT+9)=306.1
ccc      Q(LMUOT+10)=104.6
CCC
      X=Q(LMUOT+8)
      Y=Q(LMUOT+9)
      Z=Q(LMUOT+10)+DELTAZ1
      DX=Q(LMUOT+14)
      DY=Q(LMUOT+15)
      DZ=Q(LMUOT+16)
      IF (MOD(QUAD,2).EQ.1) THEN
        CYX=SQRT(DY**2+DX**2)
        CZX=SQRT(DZ**2+DX**2)
        XVEC(5)=Y 
        XVEC(6)=DY/DX 
        XVEC(7)=Z
        XVEC(8)=DZ/DX
        COORFE=Q(LMUOT+11)
        COORA=Q(LMUOT+8)
        COORC=COORCAL-ABS(X0)
        COORMIDCAL=COORMIDCAL*ABS(COS(PHI))
      ELSE
        CYX=SQRT(DY**2+DX**2)
        CZX=SQRT(DZ**2+DY**2)
        XVEC(5)=X 
        XVEC(6)=DX/DY 
        XVEC(7)=Z
        XVEC(8)=DZ/DY
        COORFE=Q(LMUOT+12)
        COORA=Q(LMUOT+9)
        COORC=COORCAL-ABS(Y0)
        COORMIDCAL=COORMIDCAL*ABS(COS(PHI))
      END IF
      COORC=SIGN(COORC,SIDE)
      ERVEC(1,1,2)=99. 
      ERVEC(2,2,2)=99. 
      ERVEC(3,3,2)=99. 
      ERVEC(4,4,2)=99. 
      ERVEC(1,2,2)=0. 
      ERVEC(3,4,2)=0. 
      ERVEC(1,3,2)=0. 
      ERVEC(1,4,2)=0. 
      ERVEC(2,3,2)=0. 
      ERVEC(2,4,2)=0. 
C
C  get parameters and cov matrix for BC segment; for now cov hardwired
C
      X=Q(LMUOT+11)
      Y=Q(LMUOT+12)
      Z=Q(LMUOT+13)+DELTAZ1
      DX=Q(LMUOT+17)
      DY=Q(LMUOT+18)
      DZ=Q(LMUOT+19)
      IF (MOD(QUAD,2).EQ.1) THEN
        XVEC(11)=Y 
        XVEC(12)=DY/DX 
        XVEC(13)=Z
        XVEC(14)=DZ/DX
      ELSE
        XVEC(11)=X 
        XVEC(12)=DX/DY 
        XVEC(13)=Z
        XVEC(14)=DZ/DY
      END IF
      ERVEC(1,1,3)=0.38  ! sigma=1.5cm
      ERVEC(2,2,3)=0.00025/3. 
      ERVEC(3,3,3)=0.015  ! sigma=0.3cm
      ERVEC(4,4,3)=0.00001/3. 
      NBC=NB+NC+ND
      ERVEC(1,1,3)=ERVEC(1,1,3)*6./NBC 
      ERVEC(2,2,3)=ERVEC(2,2,3)*6./NBC 
      ERVEC(3,3,3)=ERVEC(3,3,3)*6./NBC 
      ERVEC(4,4,3)=ERVEC(4,4,3)*6./NBC 
      ERVEC(1,2,3)=0. 
      ERVEC(3,4,3)=0. 
      ERVEC(1,3,3)=0. 
      ERVEC(1,4,3)=0. 
      ERVEC(2,3,3)=0. 
      ERVEC(2,4,3)=0. 
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
        IF (IABC.EQ.0.AND.ORIEN.LT.3) THEN
          XVEC(5)=ASEG
          XVEC(6)=BSEG
          XVEC(7)=CSEG+DELTAZ1
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
        IF (IABC.EQ.1.AND.ORIEN.LT.3) THEN
          XVEC(11)=ASEG
          XVEC(12)=BSEG
          XVEC(13)=CSEG+DELTAZ1
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
 888  CONTINUE
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
        XVEC(15)=VER(2) 
        XVEC(16)=VER(3) 
      ELSE
        XVEC(15)=VER(1) 
        XVEC(16)=VER(3) 
      END IF
C
C  Initial values of parameters
C
      CALL VZERO(PAR(1),NP)
      CALL UCOPY2(XVEC(1),PAR(1),4)     ! a,b,c,d
      PAR(7)=1./MOMF                    ! 1/p 
      NLOOP=0
  8   NLOOP=NLOOP+1
      CALL VZERO(ERALL(1,1),NM*NM)
      DO 1 K=1,4
        CALL UCOPY2(ERVEC(1,K,1),ERALL(1,K),4)
        CALL UCOPY2(ERVEC(1,K,2),ERALL(5,K+4),4)
        CALL UCOPY2(ERVEC(1,K,3),ERALL(11,K+10),4)
  1   CONTINUE
C      ERMSC=Q(LMUON+21)**2   
      RADLENC=Q(LMUON+19)
      ERMSC1=0.0136*SQRT(RADLENC)*(1.+0.038*LOG(RADLENC))
      P1=CYX*(3.+1./ABS(PAR(7)))
      P2=CZX*(3.+1./ABS(PAR(7)))
      ERMSC=(ERMSC1/P1)**2
      ERALL(9,9)=MAX(ERMSC,1.0E-8)
      ERMSC=(ERMSC1/P2)**2
      ERALL(10,10)=MAX(ERMSC,1.0E-8)
C
C  Assume 100 um alignment error wrt CDC
C
      ERALL(15,15)=EVER(1)**2+0.0001
      ERALL(16,16)=EVER(3)**2
      NPASS=0
C
C protection: 0 error means no information; assign big error
C
      DO K=1,NM
        IF (ERALL(K,K).LT.SMALL) THEN
          ERALL(K,K)=999999.
        END IF
      END DO
      CHQOLD=10000.
 11   CALL VZERO(CLIST(1),MXLST)
      CALL UCOPY2(XVEC(1),RVEC(1),NM)
      CALL VZERO(AMAT(1,1),NM*NP)
      IF (NLOOP.GT.1.OR.NPASS.GT.0) THEN
        IF (MOD(QUAD,2).EQ.1) THEN
          DX=1./SQRT(1.+XVEC1(12)**2+XVEC1(14)**2)
          DX=SIGN(DX,Q(LMUOT+14))
          DY=DX*XVEC1(12)
          DZ=DX*XVEC1(14)
          Y=XVEC1(11)
          Z=XVEC1(13)
        ELSE
          DY=1./SQRT(1.+XVEC1(12)**2+XVEC1(14)**2)
          DY=SIGN(DY,Q(LMUOT+15))
          DX=DY*XVEC1(12)
          DZ=DY*XVEC1(14)
          X=XVEC1(11)
          Z=XVEC1(13)
        END IF
      END IF
      MOMF=-MOMF
      IF (ABS(DX).LT.0.01.AND.ABS(DY).LT.0.01) THEN 
C  protection againts unphysical input
        IERR=8
        GO TO 999
      END IF
      CALL MUBDL(MOMF,QUAD,PRIMAX,X,Y,Z,DX,DY,DZ,PFACT)
      IF (ABS(PFACT(1)).LT.0.01.AND.ABS(PFACT(2)).LT.0.01) THEN
        IERR=8
        GO TO 999
      END IF
C
      AMAT(1,1)=1.
      AMAT(2,2)=1.
      AMAT(3,3)=1.
      AMAT(4,4)=1.
      AMAT(1,5)=1.
      AMAT(2,5)=COORA-COOR0
      AMAT(2,6)=1.
      AMAT(3,7)=1.
      AMAT(4,7)=COORA-COOR0
      AMAT(4,8)=1.
      AMAT(5,5)=COORA-COORMIDCAL
      AMAT(6,7)=COORA-COORMIDCAL
      AMAT(5,6)=1.
      AMAT(6,8)=1.
      AMAT(5,9)=1.
      AMAT(6,10)=1.
      AMAT(1,11)=1.
      AMAT(2,11)=COORFE-COOR0
      AMAT(2,12)=1.
      AMAT(3,13)=1.
      AMAT(4,13)=COORFE-COOR0
      AMAT(4,14)=1.
      AMAT(5,11)=COORFE-COORMIDCAL
      AMAT(6,13)=COORFE-COORMIDCAL
      AMAT(5,12)=1.
      AMAT(6,14)=1.
      AMAT(7,12)=PFACT(1) 
      AMAT(7,14)=PFACT(2) 
      AMAT(1,15)=1.
      AMAT(3,16)=1.
      IF (MOD(QUAD,2).EQ.1) THEN
        AMAT(2,15)=VER(1)-X0 
        AMAT(4,16)=VER(1)-X0 
      ELSE
        AMAT(2,15)=VER(2)-Y0
        AMAT(4,16)=VER(2)-Y0 
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
      CLIST(2)=9.
      IF (IERR.EQ.4) CLIST(2)=5.
      CLIST(3)=CHSQ
      NPASS=NPASS+1
      MOMF=ABS(CLIST(10))
      MOMF=1./MAX(MOMF,0.001)
      MOMF=SIGN(MOMF,CLIST(10))
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
  6   CONTINUE
      IF(NLOOP.LT.NLOOPMX) GO TO 8
      KSTAT=MXLST
      GO TO 998
  7   CALL UCOPY2(YVEC(1),PAR(1),NP)
      IF(CHSQ.LE.CHQOLD)THEN
        CALL UCOPY2(CLIST(1),CLSTSV(1),MXLST)
        CHQOLD=CHSQ
      END IF
      IF(NPASS.LT.MXINT) GO TO 11
      KSTAT=MXLST
  998 CONTINUE
      IF (.NOT.DEBUG) GO TO 999
      IF (IQ(LMUOT+7).NE.0) GO TO 999
      IF (CLIST(2).LT.1.) GO TO 999
      DO IH=1,16
        RATIO(IH)=XVEC2(IH)/SQRT(ERALL(IH,IH))
        IF (IH.GE.5.AND.IH.LE.8) THEN
          IF (NSEGA.GT.0) CALL HFILL(170+IH,RATIO(IH),0.,1.)
          GO TO 666
        END IF
        IF (IH.GE.11.AND.IH.LE.14) THEN
          IF (NSEGBC.GT.0) CALL HFILL(170+IH,RATIO(IH),0.,1.)
          GO TO 666
        END IF
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
      IF (NSEGA.GT.0.AND.NSEGBC.GT.0) CALL HFILL(189,CHSQ1,0.,1.)
  999 RETURN
      END
