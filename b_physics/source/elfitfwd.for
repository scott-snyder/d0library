      SUBROUTINE ELFITFWD(LPELC,CVAR,EVAR,CLIST,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fit the trajectory of a FORWARD electron
C-   
C-   The fitting parameters are 
C    PAR(1-4)  a,b,c,d 
C-   PAR(5)    thmcx   scattering angle in the VTX xz end plane 
C-   PAR(6)    thmcy   scattering angle in the VTX yz end plane 
C-
C-   electron track trajectory is:
C-   
C-   y=a+b*z                                     |x|<VTX
C-   z=c+d*z
C-
C-   y=a+b*z+thmcx*(z-XTRD)                  ZVTX<|x|<ZCAL
C-   z=c+d*z+thmcz*(z-XTRD)  
C-
C-   Inputs  :  LPELC  bank address 
C-   Outputs :  CLIST: NDF, CHISQ, fitted parameters and error matrix
C-              IERR (=0 OK, =1 no ZTRK, =2 no VTXT, =3 no FDCT, 
C-                    =4 < no calorimeter position
C-                    =5 error inverting ERMAT
C-   Controls: 
C-
C-   Created  07-July-1993   Daria Zieminska
C-                           Andrzej Zieminski
C-   adapted from J. Martin's muon fitting program for E672
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      LOGICAL LVTX, OK
C
C-    fit parameters
      INTEGER MXLST,NM,NP
      PARAMETER (NM=14)  ! number of measured quantities
      PARAMETER (NP=6)   ! number of parameters
C       NM=   2  X(Y),Z interaction vertex 
C            +4  VTX track parameters
C            +2  multiple scattering angles "measumements" in XY,XZ planes
C            +4  FDC track parameters
C            +2  electron cluster position in the calorimeter
C
C-    Pointers, indicators
      INTEGER LPELC,LVERT,LZTRK,LVTXT,LFDCT,LCACL
      INTEGER IERR,IER,L1
C
C-    Track, vertex parameters
      REAL ERVTX(4,4),ERFDC(4,4)
      REAL SCERR,P1,P2,RATIO(NM)
      REAL RADLENC,COXYZ,COVTX,COEND,COFDC,COCAL 
      REAL VER(3),EVER(3),VERT(14),ZEND
      REAL VERX,VERY,THETA,PHI,DXDZ,DYDZ
      INTEGER ICONT(10)
      REAL COSIN(3) 
C
C-    Calorimeter parameters
      REAL XCAL,YCAL,ZCAL,ECAL,ERCAL,CVAR(3),EVAR(3),DY,OFFSET(4)
      REAL BCAL,DCAL
      INTEGER NCVAR, STATUS
C
C-    Output results
      REAL CLIST(100)
C
C-    Fit matrices
      REAL AMAT(NP,NM),YVEC(NP),RVEC(NM),PAR(NP),CHSQ(1,1)
      REAL WRK(200),ERMSC1,ERMSC
      REAL ERMAT(NM,NM),ERALL(NM,NM),ERPAR(NM,NM)
      REAL XVEC(NM),XVEC1(NM),XVEC2(NM),BMAT(NP,NP),CMAT(NP,NP)
C
      INTEGER IVER,NV,IH,I,J,K
      LOGICAL FIRST
C
      DATA MXLST/100/
      DATA ZEND/30./  
      DATA RADLENC/2.0/   
      DATA VERX,VERY/-0.23,0.18/   
      DATA EVER/0.03,0.03,2./
C
C-    initialization
C
      IF(FIRST) THEN
        CALL EZPICK('ELFIT_RCP')
        CALL EZGET('ZEND',ZEND,IER)
        CALL EZGET('RADLENF',RADLENC,IER)
        CALL EZRSET
        FIRST=.FALSE.
      ENDIF
C
C-    start processing
C
      IERR=0
C
C-    find calorimeter information 
C
      ECAL=Q(LPELC+6) 
      XCAL=CVAR(1) 
      YCAL=CVAR(2) 
      ZCAL=CVAR(3) 
      IF (ABS(ZCAL).LT.150.) THEN
        IERR=20
        GO TO 999
      END IF
      BCAL=ABS(XCAL/ZCAL)
      DCAL=ABS(YCAL/ZCAL)
      ERCAL=0.5*(15./ECAL)**0.4
C
C-    alignment corrections
C     CALL CALTRKAL(ZCAL,PHI0,DY,OFFSET)
C
C-    find FDC and VTX tracks
C
      LZTRK=LQ(LPELC-3)
      IF (LZTRK.EQ.0) THEN
        IERR=1
        GO TO 999
      END IF
      LVTXT=LQ(LZTRK-6)
      IF (LVTXT.EQ.0) THEN
        IERR=-2
      END IF
      LFDCT=LQ(LZTRK-8)
      IF (LFDCT.EQ.0) THEN
        IERR=3
        GO TO 999
      END IF
C
C  Fill vector of measured quantities and make initial estimate
C  of the parameters and their errors.
C
      CALL VZERO(XVEC,NM)
C
C  Get vertex and calorimeter point
C
      CALL GTVERH(ICONT)
      NV=ICONT(2)
      IF (NV.GT.0) THEN
        CALL GTVERT(1,VERT)
        VER(1)=VERT(3)
        VER(2)=VERT(4)
        VER(3)=VERT(5)
        EVER(1)=MAX(BCAL*VERT(8),0.001)  ! make sure it's not 0
        EVER(2)=MAX(DCAL*VERT(8),0.001)
      ELSE
        VER(1)=VERX
        VER(2)=VERY
        VER(3)=0.
        EVER(1)=0.5
        EVER(2)=0.5
      END IF
      XVEC(1)= VER(1) 
      XVEC(2)= VER(2) 
      XVEC(13)= XCAL 
      XVEC(14)= YCAL
      COXYZ= VER(3) 
      COEND= ZEND*ZCAL/ABS(ZCAL)
      COCAL= ZCAL 
C
C  VTX track parameters, use FDCs values with large errors if VTX not available
C  FDC track parameters,
C
      SCERR=1.
      LVTX=.TRUE.
      L1=LVTXT
      IF(LVTXT.LE.0 .OR. ABS(VERT(3)).GT.30.) THEN
        LVTX=.FALSE.
        L1=LFDCT
        SCERR=100000.
        CALL FTRKPAR(L1,SCERR,COVTX,XVEC(3),ERVTX)
      ELSE
        SCERR=1.
        CALL FTRKROT(L1,SCERR,COVTX,XVEC(3),ERVTX)
      ENDIF
      SCERR=1.
      L1=LFDCT
      CALL FTRKPAR(L1,SCERR,COFDC,XVEC(9),ERFDC)
C
C  Initial values of parameters and errors
C
      CALL VZERO(PAR(1),NP)
      CALL UCOPY2(XVEC(3),PAR(1),4)     ! a,b,c,d
      CALL VZERO(ERALL(1,1),NM*NM)
      ERALL(1,1)=EVER(1)**2
      ERALL(2,2)=EVER(2)**2
      DO 1 K=1,4
        CALL UCOPY2(ERVTX(1,K),ERALL(3,K+2),4)
        CALL UCOPY2(ERFDC(1,K),ERALL(9,K+8),4)
  1   CONTINUE
      ERMSC1=0.0136*SQRT(RADLENC)*(1.+0.038*LOG(RADLENC))
      P1=SQRT(Q(LPELC+3)**2+Q(LPELC+4)**2) 
      P2=Q(LPELC+6) 
      ERMSC=(ERMSC1/P1)**2
      ERALL(7,7)=MAX(ERMSC,1.0E-8)
      ERMSC=(ERMSC1/P2)**2
      ERALL(8,8)=MAX(ERMSC,1.0E-8)
C     ERALL(13,13)=ERCAL**2
C     ERALL(14,14)=ERCAL**2
      ERALL(13,13)=EVAR(2)**2
      ERALL(14,14)=EVAR(3)**2
C
C     derivatives AMAT(parameter,measurement)
C
      CALL VZERO(AMAT(1,1),NM*NP)
      AMAT(1,1)=1.
      AMAT(2,1)=COXYZ-COVTX
      AMAT(3,2)=1.
      AMAT(4,2)=COXYZ-COVTX
      AMAT(1,3)=1.
      AMAT(2,4)=1.
      AMAT(3,5)=1.
      AMAT(4,6)=1.
      AMAT(5,7)=1.
      AMAT(6,8)=1.
      AMAT(1,9)=1.
      AMAT(2,9)=COFDC-COVTX
      AMAT(5,9)=COFDC-COEND
      AMAT(2,10)=1.
      AMAT(5,10)=1.
      AMAT(3,11)=1.
      AMAT(4,11)=COFDC-COVTX
      AMAT(6,11)=COFDC-COEND
      AMAT(4,12)=1.
      AMAT(6,12)=1.
      AMAT(1,13)=1.
      AMAT(2,13)=COCAL-COVTX
      AMAT(5,13)=COCAL-COEND
      AMAT(3,14)=1.
      AMAT(4,14)=COCAL-COVTX
      AMAT(6,14)=COCAL-COEND
C
C   Fit sequence
C
      CALL UCOPY2(XVEC(1),RVEC(1),NM)
      CALL UCOPY2(ERALL(1,1),ERMAT(1,1),NM*NM)
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
C  Calculate residuals and chisquare. Store them.
C
      CALL MXMPY(AMAT(1,1),YVEC(1),XVEC1(1),NM,NP,1)
      CALL VSUB(RVEC(1),XVEC1(1),XVEC2(1),NM)
      CALL MXMLTR(XVEC2,ERMAT,CHSQ,1,NM)
C
C-  Output
C
      CALL VZERO(CLIST(1),MXLST)
C
C     CLIST(1)    :  number of words.
C     CLIST(2)    :  degrees of freedom.
C     CLIST(3)    :  chisquare.
C     CLIST(4)    :  phi0
C
C     CLIST(5:8)  :  covtx,coend,cofdc,cocal
C     CLIST(9:14) :  PAR(1:6)
C     CLIST(15:50):  error matrix for the fit.
C     CLIST(51:64):  residuals
C     CLIST(65:66):  THETA, PHI fitted
C
      CLIST(1)=FLOAT(MXLST)
      CLIST(2)=8.
      IF (.NOT.LVTX) CLIST(2)=4.
      CLIST(3)=CHSQ(1,1)
      CLIST(5)=COVTX
      CLIST(6)=COEND
      CLIST(7)=COFDC
      CLIST(8)=COCAL
      CALL UCOPY2(YVEC(1),CLIST(9),NP)
      CALL UCOPY2(CMAT(1,1),CLIST(15),NP*NP)
      DO IH=1,NM
        RATIO(IH)=XVEC2(IH)/SQRT(ERALL(IH,IH))
      END DO
      CALL UCOPY2(RATIO(1),CLIST(51),NM)
C
C-  convert back to THETA, PHI
C
      DXDZ=CLIST(10)
      DYDZ=CLIST(12)
      COSIN(3)=1./SQRT(1.+DXDZ**2+DYDZ**2)
      PHI = ATAN2(DYDZ/ZCAL,DXDZ/ZCAL) 
      IF (PHI.LT.0.) PHI = PHI + TWOPI
      THETA = ACOS(COSIN(3))
      IF (ZCAL.LT.0.) THETA = -THETA + PI
C
      CLIST(65)=THETA
      CLIST(66)=PHI
C
  999 RETURN
      END
