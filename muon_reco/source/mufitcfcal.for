      SUBROUTINE MUFITCFCAL(LMUON,CLIST,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fit a central muon track that has no CD match.
C-   Use information from vertex, MTCA, MSEG; allow for MCS in the Calorimeter. 
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
C-   y=a+b*x+thmcx*(x-XCAL)                      |x|>XCAL
C-   z=c+d*x+thmcy*(x-XCAL)
C-
C-   y=a+b*x+thmcx*(x-XCAL)+CY/p*(x-XFE)         |x|>XFE 
C-   z=c+d*x+thmcy*(x-XCAL)+CZ/p*(x-XFE)
C-
C-   Inputs  :  LMUON  bank address 
C-   Outputs :  CLIST: NDF, CHISQ, fitted parameters and error matrix
C-              IERR (=0 OK, 
C-                    =4 < no WAMUS A layer)
C-                    =5 error inverting ERMAT
C-                    =6 error inverting BMAT 
C-                    =7 bad chisq
C-                    =8 bad bdl
C-   Controls: 
C-
C-   Created  29-APR 1994 Daria Zieminska
C-   Updated   7-JUL-1994   Daria Zieminska  protect against 0 errors 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER LMUON,IERR,LMUOT,NVERT,LVERT,LZTRK,LVTXT,LCDCT
      INTEGER LMTCA,KSTAT 
      INTEGER NSEGA,NSEGBC,ISEG,IABC,NMOD,NHITSEG,ORIEN,IERSEG,IMUON
      REAL BUF(5),SUBS,ASEG,BSEG,CSEG,DSEG,COV1(2,2),COV2(2,2),CHI1,CHI2
      INTEGER ITRAK,NP,PRIMAX,RUN,ID,STATUS
      INTEGER NLOOP,NLOOPMX,K,NPASS,IER,NM,NT,QUAD
      PARAMETER (NM=14)  ! number of measured quantities
      PARAMETER (NP=7)   ! number of parameters
      PARAMETER (NT=2)   ! number of tracks ( MUOT_A, MUOT_BC)
C       NM=   2  MTC point 
C            +2  scattering angle "measumements" 
C            +4  WAMUS  A LAYER PARAMETERS 
C            +4  WAMUS  BC PARAMETERS 
C            +2  vertex x(or y), z
      REAL MOM,MOMI,MOMF,RADLENC,PFACT(2),X,Y,Z,DX,DY,DZ,PSIGN 
      REAL COOR0,COORA,SIDE,COORFE 
      REAL CLIST(75),ERVEC(4,4,NT) 
      REAL AMAT(NP,NM),YVEC(NP),RVEC(NM),PAR(NP),CHSQ,CHSQ1,CHQOLD 
      REAL ERSAV(NM,NM),ERP,SMALL,ERMSC1,ERMSC
      REAL ERMAT(NM,NM),ERALL(NM,NM) 
      REAL XVEC(NM),XVEC1(NM),XVEC2(NM),BMAT(NP,NP),CMAT(NP,NP)
      REAL X0,Y0,Z0,CYX,CZX,P1,P2,RATIO(14)
      REAL VERX,VERY,CORR
      INTEGER PLANES_ALL,PLANES_FIT,IAPLN,IFPLN
      INTEGER NA,NB,NC,ND,NA_ADJ,NBC_ADJ,SA,SB,SC,NBC
      INTEGER MXLST,NV,IH,ICONT(10)
      LOGICAL FIRST,DEBUG
      PARAMETER (MXLST=75)
      REAL RELERR,EPS(NP),MXINT,CLSTSV(MXLST),VER(3),VERT(14),EVER(3)
      REAL DPHI,DTHE
C
      DATA RELERR/0.001/
      DATA EPS/0.001,0.0001,0.001,0.0001,0.0001,0.0001,0.0001/
      DATA MXINT,NLOOPMX/3,3/
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
     +     'ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
      CORR=1.
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
      IF (QUAD.GT.4) GO TO 999
      CALL MUCPLN(LMUOT,PLANES_ALL,PLANES_FIT,IAPLN,IFPLN)
      CALL MUUPLN(PLANES_FIT,NA,NB,NC,ND,SA,SB,SC,NA_ADJ,NBC_ADJ)
      IF (IQ(LMUOT+4).EQ.1.OR.IQ(LMUOT+4).EQ.11) THEN  
        IERR=4   ! mising A layer
        GO TO 999
      END IF
      IF (QUAD.LE.2.OR.QUAD.EQ.5.OR.QUAD.EQ.6.OR.QUAD.EQ.9.OR.QUAD.EQ.
     &  10) THEN
        SIDE=1.
      ELSE
        SIDE=-1.
      END IF
      MOM=MAX(ABS(Q(LMUOT+23)),3.001)    
      PSIGN=Q(LMUOT+23)
      MOMI=MOM
      MOMF=MOM-3.0
      MOMF=SIGN(MOMF,PSIGN)
      LVERT=LQ(LMUON-12)
      NVERT=IQ(LVERT-5)
      LZTRK=LQ(LMUON-13)
      IF (LZTRK.GT.0) THEN
        LCDCT=LQ(LZTRK-7)
        LVTXT=LQ(LZTRK-6)
        IF (LCDCT.GT.0) THEN
C          GO TO 999           
        END IF
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
      CALL VXY_BEAM1(VER(3),VER(1),EVER(1),VER(2),EVER(2),STATUS)
      IF (MOD(QUAD,2).EQ.1) THEN
        XVEC(13)=VER(2) 
        XVEC(14)=VER(3) 
      ELSE
        XVEC(13)=VER(1) 
        XVEC(14)=VER(3) 
      END IF
      IF (STATUS.EQ.2) THEN   ! default beam position
        EVER(1)=0.1
      END IF
C
C  MTC parameters
C
      LMTCA=LQ(LMUON-9)
      IF (LMTCA.LE.0) THEN
        IERR=9
        GO TO 999
      END IF
      X0 =Q(LMTCA+10)
      Y0 =Q(LMTCA+11)
      Z0 =Q(LMTCA+12)
      IF (MOD(QUAD,2).EQ.1) THEN
        PRIMAX=1
        COORFE=Q(LMUOT+11)
        COOR0=X0
        XVEC(1)=Y0
        XVEC(2)=Z0 
      ELSE
        PRIMAX=2
        COOR0=Y0
        XVEC(1)=X0
        XVEC(2)=Z0 
      END IF
C
C muon segments
C
      X=Q(LMUOT+8)
      Y=Q(LMUOT+9)
      Z=Q(LMUOT+10)
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
      ELSE
        CYX=SQRT(DY**2+DX**2)
        CZX=SQRT(DZ**2+DY**2)
        XVEC(5)=X 
        XVEC(6)=DX/DY 
        XVEC(7)=Z
        XVEC(8)=DZ/DY
        COORFE=Q(LMUOT+12)
        COORA=Q(LMUOT+9)
      END IF
      ERVEC(1,1,1)=99. 
      ERVEC(2,2,1)=99. 
      ERVEC(3,3,1)=99. 
      ERVEC(4,4,1)=99. 
      ERVEC(1,2,1)=0. 
      ERVEC(3,4,1)=0. 
      ERVEC(1,3,1)=0. 
      ERVEC(1,4,1)=0. 
      ERVEC(2,3,1)=0. 
      ERVEC(2,4,1)=0. 
C
C  get parameters and cov matrix for BC segment; for now cov hardwired
C
      X=Q(LMUOT+11)
      Y=Q(LMUOT+12)
      Z=Q(LMUOT+13)
      DX=Q(LMUOT+17)
      DY=Q(LMUOT+18)
      DZ=Q(LMUOT+19)
      IF (MOD(QUAD,2).EQ.1) THEN
        XVEC(9)=Y 
        XVEC(10)=DY/DX 
        XVEC(11)=Z
        XVEC(12)=DZ/DX
      ELSE
        XVEC(9)=X 
        XVEC(10)=DX/DY 
        XVEC(11)=Z
        XVEC(12)=DZ/DY
      END IF
      ERVEC(1,1,2)=0.38  ! sigma=1.5cm
      ERVEC(2,2,2)=0.00025/3. 
      ERVEC(3,3,2)=0.015  ! sigma=0.3cm
      ERVEC(4,4,2)=0.00001/3. 
      NBC=NB+NC+ND
      ERVEC(1,1,2)=ERVEC(1,1,2)*6./NBC 
      ERVEC(2,2,2)=ERVEC(2,2,2)*6./NBC 
      ERVEC(3,3,2)=ERVEC(3,3,2)*6./NBC 
      ERVEC(4,4,2)=ERVEC(4,4,2)*6./NBC 
      ERVEC(1,2,2)=0. 
      ERVEC(3,4,2)=0. 
      ERVEC(1,3,2)=0. 
      ERVEC(1,4,2)=0. 
      ERVEC(2,3,2)=0. 
      ERVEC(2,4,2)=0. 
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
          XVEC(7)=CSEG
          XVEC(8)=DSEG
          ERVEC(1,1,1)=COV1(2,2)
          ERVEC(1,2,1)=COV1(1,2)
          ERVEC(2,2,1)=COV1(1,1)
          ERVEC(3,3,1)=COV2(2,2)
          ERVEC(3,4,1)=COV2(1,2)
          ERVEC(4,4,1)=COV2(1,1)
          NSEGA=NSEGA+1
        END IF
        IF (IABC.EQ.1.AND.ORIEN.LT.3) THEN
          XVEC(9)=ASEG
          XVEC(10)=BSEG
          XVEC(11)=CSEG
          XVEC(12)=DSEG
          ERVEC(1,1,2)=COV1(2,2)
          ERVEC(1,2,2)=COV1(1,2)
          ERVEC(2,2,2)=COV1(1,1)
          ERVEC(3,3,2)=COV2(2,2)
          ERVEC(3,4,2)=COV2(1,2)
          ERVEC(4,4,2)=COV2(1,1)
          NSEGBC=NSEGBC+1
        END IF
      END DO
 888  CONTINUE
      IF (NSEGA.EQ.0) IERR=4
      ERVEC(2,1,1)=ERVEC(1,2,1)
      ERVEC(3,1,1)=ERVEC(1,3,1)
      ERVEC(4,1,1)=ERVEC(1,4,1)
      ERVEC(3,2,1)=ERVEC(2,3,1)
      ERVEC(4,2,1)=ERVEC(2,4,1)
      ERVEC(4,3,1)=ERVEC(3,4,1)
      ERVEC(2,1,2)=ERVEC(1,2,2)
      ERVEC(3,1,2)=ERVEC(1,3,2)
      ERVEC(4,1,2)=ERVEC(1,4,2)
      ERVEC(3,2,2)=ERVEC(2,3,2)
      ERVEC(4,2,2)=ERVEC(2,4,2)
      ERVEC(4,3,2)=ERVEC(3,4,2)
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
        CALL UCOPY2(ERVEC(1,K,1),ERALL(5,K+4),4)
        CALL UCOPY2(ERVEC(1,K,2),ERALL(9,K+8),4)
  1   CONTINUE
      ERALL(1,1)=25.
      ERALL(2,2)=25.
      CORR=Q(LMTCA+26)/3.
      CORR=MAX(CORR,1.)
      CORR=MIN(CORR,400.)
      ERALL(1,1)=ERALL(1,1)*CORR**2
      ERALL(2,2)=ERALL(2,2)*CORR**2
      RADLENC=Q(LMUON+19)
      ERMSC1=0.0136*SQRT(RADLENC)*(1.+0.038*LOG(RADLENC))
      P1=CYX*(3.+1./ABS(PAR(7)))
      P2=CZX*(3.+1./ABS(PAR(7)))
      ERMSC=(ERMSC1/P1)**2
      ERALL(3,3)=MAX(ERMSC,1.0E-8)
      ERMSC=(ERMSC1/P2)**2
      ERALL(4,4)=MAX(ERMSC,1.0E-8)
C
C  Assume 100 um alignment error wrt CDC
C
      ERALL(13,13)=EVER(1)**2+0.0001
      ERALL(14,14)=EVER(3)**2
C
C protection: 0 error means no information; assign big error
C
      DO K=1,NM
        IF (ERALL(K,K).LT.SMALL) THEN
          ERALL(K,K)=999999.
        END IF
      END DO
      NPASS=0
      CHQOLD=10000.
 11   CALL VZERO(CLIST(1),MXLST)
      CALL UCOPY2(XVEC(1),RVEC(1),NM)
      CALL VZERO(AMAT(1,1),NM*NP)
      IF (NLOOP.GT.1.OR.NPASS.GT.0) THEN
        IF (MOD(QUAD,2).EQ.1) THEN
          DX=1./SQRT(1.+XVEC1(10)**2+XVEC1(12)**2)
          DX=SIGN(DX,Q(LMUOT+14))
          DY=DX*XVEC1(10)
          DZ=DX*XVEC1(12)
          Y=XVEC1(9)
          Z=XVEC1(11)
        ELSE
          DY=1./SQRT(1.+XVEC1(10)**2+XVEC1(12)**2)
          DY=SIGN(DY,Q(LMUOT+15))
          DX=DY*XVEC1(10)
          DZ=DY*XVEC1(12)
          X=XVEC1(9)
          Z=XVEC1(11)
        END IF
      END IF
      MOMF=-MOMF
      IF (ABS(DX).LT.0.01.AND.ABS(DY).LT.0.01) THEN 
C protection againts unphysical input
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
      AMAT(3,2)=1.
      AMAT(5,3)=1.
      AMAT(6,4)=1.
      AMAT(1,5)=1.
      AMAT(2,5)=COORA-COOR0
      AMAT(5,5)=COORA-COOR0
      AMAT(2,6)=1.
      AMAT(5,6)=1.
      AMAT(3,7)=1.
      AMAT(4,7)=COORA-COOR0
      AMAT(6,7)=COORA-COOR0
      AMAT(4,8)=1.
      AMAT(6,8)=1.
      AMAT(1,9)=1.
      AMAT(2,9)=COORFE-COOR0
      AMAT(5,9)=COORFE-COOR0
      AMAT(2,10)=1.
      AMAT(5,10)=1.
      AMAT(7,10)=PFACT(1) 
      AMAT(3,11)=1.
      AMAT(4,11)=COORFE-COOR0
      AMAT(6,11)=COORFE-COOR0
      AMAT(4,12)=1.
      AMAT(6,12)=1.
      AMAT(7,12)=PFACT(2) 
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
      CALL MXMLTR(AMAT,ERMAT,BMAT(1,1),NP,NM)
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
      CLIST(2)=7.
      IF (IERR.EQ.4) CLIST(2)=3.
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
      DO IH=1,NM
        RATIO(IH)=XVEC2(IH)/SQRT(ERALL(IH,IH))
        IF (IH.GE.5.AND.IH.LE.8) THEN
          IF (NSEGA.GT.0) CALL HFILL(170+IH,RATIO(IH),0.,1.)
          GO TO 666
        END IF
        IF (IH.GE.9.AND.IH.LE.12) THEN
          IF (NSEGBC.GT.0) CALL HFILL(170+IH,RATIO(IH),0.,1.)
          GO TO 666
        END IF
        CALL HFILL(170+IH,RATIO(IH),0.,1.)
  666   CONTINUE
      END DO
      CALL EVNTID(RUN,ID)
      IF (LVTXT.GT.0.AND.LMTCA.GT.0) THEN
        DPHI=ABS(Q(LMTCA+6)-Q(LVTXT+6))
        DTHE=ABS(Q(LMTCA+9)-Q(LVTXT+9))
        IF (DPHI.GT.PI) DPHI=TWOPI-DPHI
        CALL HFILL(187,DPHI,0.,1.)
        CALL HFILL(188,Dthe,0.,1.)
      END IF
      CHSQ1=CHSQ/CLIST(2)
      IF (NSEGA.GT.0.AND.NSEGBC.GT.0) CALL HFILL(189,CHSQ1,0.,1.)
  999 RETURN
      END
