      SUBROUTINE MUFITEFCAL(LMUON,CLIST,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fit a forward muon track that has no CD match.
C-   Use information from vertex, MTCA, MSEG; allow for MCS in the Calorimeter. 
C-   
C-   The fitting parameters are: 
C    PAR(1-4)  a,b,c,d 
C-   PAR(5)    thmcx   scattering angle in Calorimeter in yx plane 
C-   PAR(6)    thmcy   scattering angle in Calorimeter in zx plane 
C-   PAR(7)    1/P
C-
C-   Muon track trajectory is:
C-   
C-   x=a+b*z                                         |z|<ZCAL
C-   y=c+d*z
C-
C-   x=a+b*z+thmcx*(z-ZCAL)                     ZCAL<|z|<ZFE
C-   y=c+d*z+thmcy*(z-ZCAL)
C-
C-   x=a+b*z+thmcx*(z-ZCAL)+CX/p*(z-ZFE)            |z|>ZFE 
C-   y=c+d*z+thmcy*(z-ZCAL)+CY/p*(z-ZFE)
C-
C-   Inputs  :  LMUON  bank address 
C-   Outputs :  CLIST: NDF, CHISQ, fitted parameters and error matrix
C-              IERR (=0 OK, 
C-                    =4 < no WAMUS A layer)
C-                    =5 error inverting ERMAT
C-                    =6 error inverting BMAT 
C-                    =7 bad chisq
C-                    =8 bad BDL or MSEG
C-                    =9 bad MTCA 
C-   Controls: 
C-
C-   Created  23-JUN 1994 Daria Zieminska
C-   Updated   7-JUL-1994   Daria Zieminska  protect againts 0 errors 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER LMUON,IERR,LMUOT,NVERT,LVERT 
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
      REAL ZA,ZFE 
      REAL CLIST(75),ERVEC(4,4,NT) 
      REAL AMAT(NP,NM),YVEC(NP),RVEC(NM),PAR(NP),CHSQ,CHSQ1,CHQOLD 
      REAL ERSAV(NM,NM),ERP,SMALL,ERMSC1,ERMSC
      REAL ERMAT(NM,NM),ERALL(NM,NM) 
      REAL XVEC(NM),XVEC1(NM),XVEC2(NM),BMAT(NP,NP),CMAT(NP,NP)
      REAL X0,Y0,Z0,CXZ,CYZ,P1,P2,RATIO(14),PHIMU,THEMU
      REAL VERX,VERY,CORR
      INTEGER PLANES_ALL,PLANES_FIT,IAPLN,IFPLN
      INTEGER NA,NB,NC,ND,NA_ADJ,NBC_ADJ,SA,SB,SC 
      INTEGER MXLST,NV,IH,ICONT(10)
      LOGICAL FIRST,DEBUG
      PARAMETER (MXLST=75)
      REAL RELERR,EPS(NP),MXINT,CLSTSV(MXLST),VER(3),VERT(14),EVER(3)
C
      DATA RELERR/0.001/
      DATA EPS/0.001,0.0001,0.001,0.0001,0.0001,0.0001,0.0001/
      DATA MXINT,NLOOPMX/3,3/
      DATA SMALL/1.0E-8/ 
      DATA VERX,VERY/-0.23,0.18/
      DATA EVER/0.05,0.05,2./
      DATA FIRST,DEBUG/.TRUE.,.FALSE./
      IF (FIRST) THEN
C
        FIRST=.FALSE.
C
        CALL DHDIR('MURECO_RCP','HBOOK_DIRECTORY',IERR,' ')
C
      IF(IERR.NE.0) THEN
         CALL ERRMSG('MURECO','MURECO_HST',
     +     'ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
      IF (DEBUG) THEN
        CALL HBOOK1(171,' a MTC',100,-5.,5.,0.)
        CALL HBOOK1(172,' c MTC',100,-5.,5.,0.)
        CALL HBOOK1(173,' TH xz ',100,-5.,5.,0.)
        CALL HBOOK1(174,' TH yz ',100,-5.,5.,0.)
        CALL HBOOK1(175,' a  A ',100,-5.,5.,0.)
        CALL HBOOK1(176,' b  A ',100,-5.,5.,0.)
        CALL HBOOK1(177,' c  A ',100,-5.,5.,0.)
        CALL HBOOK1(178,' d  A ',100,-5.,5.,0.)
        CALL HBOOK1(179,' a  BC',100,-5.,5.,0.)
        CALL HBOOK1(180,' b  BC',100,-5.,5.,0.)
        CALL HBOOK1(181,' c  BC',100,-5.,5.,0.)
        CALL HBOOK1(182,' d  BC',100,-5.,5.,0.)
        CALL HBOOK1(183,' ver 1',100,-5.,5.,0.)
        CALL HBOOK1(184,' ver 2',100,-5.,5.,0.)
        CALL HBOOK1(189,' CHISQ/DF',100,0.,20.,0.)
      END IF
      END IF
      IERR=0
      CALL VZERO(CLIST(1),MXLST)
      CALL VZERO(CLSTSV(1),MXLST)
      LMUOT=LQ(LMUON-11)
      ITRAK=IQ(LMUOT-5)
      QUAD=IQ(LMUOT+3)
      IF (QUAD.LT.5.OR.QUAD.GT.12) GO TO 999
      CALL MUCPLN(LMUOT,PLANES_ALL,PLANES_FIT,IAPLN,IFPLN)
      CALL MUUPLN(PLANES_FIT,NA,NB,NC,ND,SA,SB,SC,NA_ADJ,NBC_ADJ)
      IF (IQ(LMUOT+4).EQ.1.OR.IQ(LMUOT+4).EQ.11) THEN  
        IERR=4   ! missing A layer
        GO TO 999
      END IF
      MOM=MAX(ABS(Q(LMUOT+23)),3.001)    
      PSIGN=Q(LMUOT+23)
      MOMI=MOM
      MOMF=MOM-3.0
      MOMF=SIGN(MOMF,PSIGN)
      LVERT=LQ(LMUON-12)
      NVERT=IQ(LVERT-5)
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
      XVEC(13)=VER(1) 
      XVEC(14)=VER(2) 
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
      PRIMAX=3
      ZFE=Q(LMUOT+13)
      XVEC(1)=X0
      XVEC(2)=Y0 
C
C muon segments
C
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
      ZA=Q(LMUOT+10)
      X=Q(LMUOT+11)
      Y=Q(LMUOT+12)
      Z=Q(LMUOT+13)
      DX=Q(LMUOT+17)
      DY=Q(LMUOT+18)
      DZ=Q(LMUOT+19)
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
          ERVEC(1,1,1)=COV1(2,2)
          ERVEC(1,2,1)=COV1(1,2)
          ERVEC(2,2,1)=COV1(1,1)
          ERVEC(3,3,1)=COV2(2,2)
          ERVEC(3,4,1)=COV2(1,2)
          ERVEC(4,4,1)=COV2(1,1)
          NSEGA=NSEGA+1
        END IF
        IF (IABC.EQ.1) THEN
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
      IF (NSEGA.EQ.0.OR.NSEGBC.EQ.0) THEN 
        IERR=8
        GO TO 999
      END IF
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
      P1=CXZ*(3.+1./ABS(PAR(7)))
      P2=CYZ*(3.+1./ABS(PAR(7)))
      ERMSC=(ERMSC1/P1)**2
      ERALL(3,3)=MAX(ERMSC,1.0E-8)
      ERMSC=(ERMSC1/P2)**2
      ERALL(4,4)=MAX(ERMSC,1.0E-8)
      ERALL(13,13)=(CXZ*EVER(3))**2 
      ERALL(14,14)=(CYZ*EVER(3))**2
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
        DZ=1./SQRT(1.+XVEC1(10)**2+XVEC1(12)**2)
        DZ=SIGN(DZ,Q(LMUOT+19))
        DX=DZ*XVEC1(10)
        DY=DZ*XVEC1(12)
        X=XVEC1(9)
        Y=XVEC1(11)
      END IF
      MOMF=-MOMF
      IF (ABS(DZ).GT.0.99) THEN ! protection againts unphysical input
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
      AMAT(2,5)=ZA-Z0
      AMAT(5,5)=ZA-Z0
      AMAT(2,6)=1.
      AMAT(5,6)=1.
      AMAT(3,7)=1.
      AMAT(4,7)=ZA-Z0
      AMAT(6,7)=ZA-Z0
      AMAT(4,8)=1.
      AMAT(6,8)=1.
      AMAT(1,9)=1.
      AMAT(2,9)=ZFE-Z0
      AMAT(5,9)=ZFE-Z0
      AMAT(2,10)=1.
      AMAT(5,10)=1.
      AMAT(7,10)=PFACT(1) 
      AMAT(3,11)=1.
      AMAT(4,11)=ZFE-Z0
      AMAT(6,11)=ZFE-Z0
      AMAT(4,12)=1.
      AMAT(6,12)=1.
      AMAT(7,12)=PFACT(2) 
      AMAT(1,13)=1.
      AMAT(3,14)=1.
      AMAT(2,13)=VER(3)-Z0 
      AMAT(4,14)=VER(3)-Z0 
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
      CHSQ1=CHSQ/CLIST(2)
      IF (NSEGA.GT.0.AND.NSEGBC.GT.0) CALL HFILL(189,CHSQ1,0.,1.)
  999 RETURN
      END
