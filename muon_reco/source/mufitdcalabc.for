      SUBROUTINE MUFITDCALABC(LMUON,POINT,COVCAL,CLIST,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fit a muon track through central detector,
C-   calorimeter and muon system. Allow for MCS in the Calorimeter. 
C-   This version applies to central tracks and uses information from 
C-   vertex, CDC, exit point from Calorimeter and WAMUS.
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
C-   Updated  27-APR-1993   Daria Zieminska  removed CDC error corrections 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LMUON,IERR,LMUOT,LVERT,LZTRK,LVTXT,LCDCT,KSTAT,ICDCT
      INTEGER ITRAK,NP,I,J,PRIMAX 
      INTEGER NLOOP,NLOOPMX,K,NPASS,IER,NENTRY,NPRT,NM,NT,NHIT,QUAD
      PARAMETER (NM=18)  ! number of measured quantities
      PARAMETER (NP=7)   ! number of parameters
      PARAMETER (NT=3)   ! number of tracks (CDC, MUOT_A, MUOT_BC)
C       NM=   4  CDC track parameters
C            +4  WAMUS  A LAYER PARAMETERS 
C            +2  scattering angle "measumements" 
C            +4  WAMUS  BC PARAMETERS 
C            +2  vertex x(or y), z
C            +2  exit point from CAL x(or y), z
      REAL MOM,MOMI,MOMF,RADLENC,PFACT(2),X,Y,Z,DX,DY,DZ,PSIGN 
      REAL COOR0,COORC,COORCAL,COORMIDCAL,COORA,SIDE,COORFE 
      REAL CLIST(75),TVEC(4,NT),ERVEC(4,4,NT) 
      REAL AMAT(NP,NM),YVEC(NP),RVEC(NM),PAR(NP),CHSQ,CHSQ1,CHQOLD 
      REAL WRK(200),ERSAV(NM,NM),ERP,SMALL,ERMSC1,ERMSC
      REAL ERMAT(NM,NM),ERALL(NM,NM),ERPAR(NM,NM),UCOS(2)
      REAL XVEC(NM),XVEC1(NM),XVEC2(NM),BMAT(NP,NP),CMAT(NP,NP)
      REAL PHI,THE,SP2,CP2,EPHI,EXY,ETHE,EZ,ZV
      REAL X0,Y0,Z0,R0,CYX,CZX,P1,P2,RATIO(NM)
      REAL PARA(4),PARBC(4),COVA(4,4),COVBC(4,4),PREFA,PREFBC
      REAL VERX,VERY,POINT(3),COVCAL(2,2)
      INTEGER PLANES_ALL,PLANES_FIT,IAPLN,IFPLN
      INTEGER NA,NB,NC,ND,NA_ADJ,NBC_ADJ,SA,SB,SC,NBC
      INTEGER NATOT,NBTOT,NCTOT,NSAMUS,NTOT
      INTEGER NAADJTOT,NBADJTOT,NCADJTOT
      INTEGER MXLST,IVER,NV,IH,IERA,IERBC,ICONT(10)
      LOGICAL FIRST,DEBUG
      PARAMETER (MXLST=75)
      REAL RELERR,EPS(NP),MXINT,CLSTSV(MXLST),VER(3),VERT(14),EVER(3)
C
      DATA RELERR/0.001/
      DATA EPS/0.001,0.0001,0.001,0.0001,0.0001,0.0001,0.0001/
      DATA MXINT,NLOOPMX/3,3/
      DATA SMALL/1.0E-8/,NENTRY/0/,NPRT/0/
      DATA COORCAL/150./  
      DATA EVER/0.05,0.05,2./
      DATA RADLENC/100./   
      DATA VERX,VERY/-0.23,0.18/   ! should use a utility  (not available
                                   ! at present)
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
      END IF
      END IF
      IERR=0
      CALL VZERO(CLIST(1),MXLST)
      LMUOT=LQ(LMUON-11)
      ITRAK=IQ(LMUOT-5)
      QUAD=IQ(LMUOT+3)
      IF (QUAD.GT.12) GO TO 999
      CALL MUCPLN(LMUOT,PLANES_ALL,PLANES_FIT,IAPLN,IFPLN)
      CALL MUUPLN(PLANES_FIT,NA,NB,NC,ND,SA,SB,SC,NA_ADJ,NBC_ADJ)
      IF (IQ(LMUOT+4).EQ.1.OR.IQ(LMUOT+4).EQ.11) THEN  
        IERR=4   ! mising A layer
C        GO TO 999
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
      LZTRK=LQ(LMUON-13)
      IF (LZTRK.EQ.0) THEN
        IERR=1
C        GO TO 999
        GO TO 20
      END IF
      LVTXT=LQ(LZTRK-6)
      IF (LVTXT.EQ.0) THEN
        IERR=2
      END IF
      LCDCT=LQ(LZTRK-7)
      IF (LCDCT.EQ.0) THEN
        IERR=3
C        GO TO 999
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
      IF (IERR.EQ.1) THEN
        GO TO 777
      END IF
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
      ETHE=MAX(ETHE,0.001)
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
        COORC=COORCAL-ABS(X0)
        COORMIDCAL=COORMIDCAL*ABS(DX)/SQRT(DX**2+DY**2) 
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
        COORMIDCAL=COORMIDCAL*ABS(DY)/SQRT(DX**2+DY**2) 
      END IF
      COORC=SIGN(COORC,SIDE)
      ERVEC(1,1,2)=0.55  ! sigma=1.5cm
      ERVEC(2,2,2)=0.0025 
      ERVEC(3,3,2)=0.0225  ! sigma=0.3cm
      ERVEC(4,4,2)=0.0001 
      IF (NA.GT.0) THEN
        ERVEC(1,1,2)=ERVEC(1,1,2)*4./NA 
        ERVEC(2,2,2)=ERVEC(2,2,2)*4./NA 
        ERVEC(3,3,2)=ERVEC(3,3,2)*4./NA 
        ERVEC(4,4,2)=ERVEC(4,4,2)*4./NA 
      ELSE
        ERVEC(1,1,2)=99. 
        ERVEC(2,2,2)=99. 
        ERVEC(3,3,2)=99. 
        ERVEC(4,4,2)=99. 
      END IF
      ERVEC(1,2,2)=0. 
      ERVEC(3,4,2)=0. 
      ERVEC(1,3,2)=0. 
      ERVEC(1,4,2)=0. 
      ERVEC(2,3,2)=0. 
      ERVEC(2,4,2)=0. 
      ERVEC(2,1,2)=ERVEC(1,2,2)
      ERVEC(3,1,2)=ERVEC(1,3,2)
      ERVEC(4,1,2)=ERVEC(1,4,2)
      ERVEC(3,2,2)=ERVEC(2,3,2)
      ERVEC(4,2,2)=ERVEC(2,4,2)
      ERVEC(4,3,2)=ERVEC(3,4,2)
C
      IF (IERR.EQ.1.AND.NA.LT.3) THEN
        IERR=9    ! no CD, bad A
        GO TO 999
      END IF
C
C  Get away without CD if good A layer
C
      IF (IERR.EQ.1.AND.NA.GE.3) THEN
        X0=0.
        Y0=0.
        COOR0=0.
        XVEC(2)=XVEC(6)
        XVEC(4)=XVEC(8)
        ERVEC(1,1,1)=99. 
        ERVEC(2,2,1)=99. 
        ERVEC(3,3,1)=99. 
        ERVEC(4,4,1)=99. 
      END IF
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
      ERVEC(2,1,3)=ERVEC(1,2,3)
      ERVEC(3,1,3)=ERVEC(1,3,3)
      ERVEC(4,1,3)=ERVEC(1,4,3)
      ERVEC(3,2,3)=ERVEC(2,3,3)
      ERVEC(4,2,3)=ERVEC(2,4,3)
      ERVEC(4,3,3)=ERVEC(3,4,3)
C
C  Get A and BC segments (don't use; MUSEG_SER not debugged)
C
C      CALL MUSEG_SER(ITRAK,1,1,QUAD,PARA,COVA,PREFA,IERA)
C      CALL MUSEG_SER(ITRAK,2,1,QUAD,PARBC,COVBC,PREFBC,IERBC)
C  
C  Get vertex
C
C      CALL VERXYZ(IVER,VER,NV)  
      CALL GTVERH(ICONT)
      NV=ICONT(2)
      IF (NV.GT.0) THEN
        CALL GTVERT(1,VERT)
        VER(1)=VERT(3)
        VER(2)=VERT(4)
        VER(3)=VERT(5)
        EVER(1)=VERT(6)
        EVER(2)=VERT(7)
        EVER(3)=VERT(8)
      ELSE
        VER(1)=VERX
        VER(2)=VERY
        VER(3)=0.
        EVER(1)=0.5
        EVER(2)=0.5
        EVER(3)=50.
      END IF
      IF (MOD(QUAD,2).EQ.1) THEN
        PRIMAX=1
        XVEC(15)=VER(2) 
        XVEC(16)=VER(3) 
      ELSE
        PRIMAX=2
        XVEC(15)=VER(1) 
        XVEC(16)=VER(3) 
      END IF
C
C  Exit point from CAL
C
      IF (MOD(QUAD,2).EQ.1) THEN
        XVEC(17)=POINT(2) 
        XVEC(18)=POINT(3) 
      ELSE
        XVEC(17)=POINT(1) 
        XVEC(18)=POINT(3) 
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
      ERALL(15,15)=EVER(1)**2
      ERALL(16,16)=EVER(3)**2
      ERALL(17,17)=COVCAL(1,1) 
      ERALL(18,18)=COVCAL(2,2) 
      NPASS=0
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
      AMAT(1,17)=1.
      AMAT(3,18)=1.
      IF (MOD(QUAD,2).EQ.1) THEN
        AMAT(2,15)=VER(1)-X0 
        AMAT(4,16)=VER(1)-X0 
        AMAT(2,17)=POINT(1)-X0 
        AMAT(4,18)=POINT(1)-X0 
        AMAT(5,17)=POINT(1)-COORMIDCAL 
        AMAT(6,18)=POINT(1)-COORMIDCAL
      ELSE
        AMAT(2,15)=VER(2)-Y0
        AMAT(4,16)=VER(2)-Y0 
        AMAT(2,17)=POINT(2)-Y0
        AMAT(4,18)=POINT(2)-Y0 
        AMAT(5,17)=POINT(2)-COORMIDCAL
        AMAT(6,18)=POINT(2)-COORMIDCAL 
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
      CLIST(2)=11.
      IF (NA.EQ.0) CLIST(2)=CLIST(2)-4
      IF (IERR.EQ.1) CLIST(2)=CLIST(2)-4
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
      DO IH=1,NM
        RATIO(IH)=XVEC2(IH)/SQRT(ERALL(IH,IH))
        CALL HFILL(170+IH,RATIO(IH),0.,1.)
      END DO
      WRITE(1,100) NLOOP,NPASS,Q(LMUOT+23),MOMF,chsq,
     1             PFACT(1),PFACT(2),x,y,z 
  100 FORMAT(' NL NP P PF CHSQ PFACT x y z', 2I5,
     1         2F7.1,2X,E10.2,2F9.3,3F8.1)
        WRITE(1,101) (ratio(K),K=1,16)
  101   FORMAT(8f9.2)
        IF (QUAD.GT.4) THEN
          WRITE(2,100) NLOOP,NPASS,Q(LMUOT+23),MOMF,chsq,
     1                 PFACT(1),PFACT(2),x,y,z 
          WRITE(2,101) (ratio(K),K=1,16)
        END IF
  999 RETURN
      END
