      SUBROUTINE Z_FIT_MUMU(IFLAG,VP,SIGMAP,VMET,EMET,SCET,FITPAR,IER)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Refit the momenta of a muon pair leaving the
C-      angles fixed. Constraint the lepton-pair mass to M(Z), and the
C-      MET to balance the Muon total pt.
C-
C-      The Chi^2 is a function of P1 only.  A modified golden secant
C-      method is used (See Numerical Recipes).
C-
C-   Inputs  : IFLAG   - 0==>Initialize coeffcients and skip minimizaiton
C-             VP      - VP(1,I) = Theta (muon I)
C-                     - VP(2,I) = Phi   (muon I)
C-                     - VP(3,I) = 1/P   (muon I)
C-             SIGMAP  - Muon 1/p errors**2
C-             VMET    - Missing ET vector
C-             EMET    - Missing ET error matrix.
C-             SCET    - Scalar MET.
C-   Outputs : FITPAR  - Fit results
C-             IER    - 0 ==> OK
C-   Controls: 
C-
C-   Created  22-Apr-1994   John D. Hobbs
C-   $Log$
C-   Revision 1.2  1996/03/22  21:13:08  hobbs
C-   Bug fix to MET calculation
C-
C-   Revision 1.1  1995/11/02  02:46:02  hobbs
C-   Version used for Nov. 3 Top results
C-
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER IFLAG,IER,I,J,II,JJ,N
      REAL   VP(3,2),VMET(2),EMET(2,2),FITPAR(*),RDUMMY,SIGMAP(2),CHI2
      REAL   SCET,ADUMMY(2)
C
      REAL*8 KAPPA_ERR_CONST,KAPPA_ERR_MULT,ZMASS,MET_ERR_SCALE,EPSCHI
      REAL*8 DCHI_MAX,DELTA_CHI,EPSCONV,EPS,MET_ERR_CONST,MET_ERR_MULT
      REAL   EMETTMP(2,2),EMETINV(2,2),DET
      INTEGER NGRID,ORDER
      LOGICAL DOERRORS,DOMETERRS
      PARAMETER(EPS=1.0E-15)
C
      INCLUDE 'D0$INC:Z_FIT_MUMU.INC'
C
      REAL*8 DVP1(3),DVP2(3),DCHI_MIN,CHIMIN,KAPPA_MIN,X,X0,X1,DX,XMIN
      REAL*8 ET,ERRSCET,RX,RY
      CHARACTER*60 ERRTXT
C
      REAL   PROB
      REAL*8 ZFE_CHI2,GOLDEN_SECANT,GRID_MINIMIZER
      EXTERNAL ZFE_CHI2,GOLDEN_SECANT,GRID_MINIMIZER,PROB
C
      LOGICAL FIRST,LDUMMY,USE_MC_KAPPA_ERR,MCDATA
      DATA FIRST/.TRUE./
      SAVE FIRST
      SAVE KAPPA_ERR_CONST,KAPPA_ERR_MULT,ZMASS,MET_ERR_SCALE,EPSCHI
      SAVE DCHI_MAX,DELTA_CHI,DOERRORS,NGRID,DOMETERRS
      SAVE MET_ERR_MULT,MET_ERR_CONST
C-----------------------------------------------------------------------
C
      IER=0
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL INRCP('Z_FIT_MUMU_RCP',IER)
        IF(IER.NE.0) THEN
          ERRTXT='No RCP file Z_FIT_MUMU found'
          CALL ERRMSG('Z_FIT_MUMU_NORCP','Z_FIT_MUMU',ERRTXT,'F')
        ELSE
          CALL EZGET('MET_ERR_SCALE',RDUMMY,IER)
          MET_ERR_SCALE=RDUMMY
          CALL EZGET('MET_ERR_CONST',RDUMMY,IER)
          MET_ERR_CONST=RDUMMY
          CALL EZGET('MET_ERR_MULT',RDUMMY,IER)
          MET_ERR_MULT=RDUMMY
          IF(IER.EQ.0) CALL EZGET('USE_MC_KAPPA_ERR',LDUMMY,IER)
          USE_MC_KAPPA_ERR = LDUMMY
          IF (IER.EQ.0) CALL EZGETA('KAPPA_ERR_CONST2',1,N,1,
     &      ADUMMY,IER)
          MCDATA = IQ(LHEAD+1).GT.1000
          IF (MCDATA.AND.USE_MC_KAPPA_ERR) THEN
            KAPPA_ERR_CONST=ADUMMY(2)
          ELSE  
            KAPPA_ERR_CONST=ADUMMY(1)
          ENDIF
          IF(IER.EQ.0) CALL EZGET('KAPPA_ERR_MULT2',RDUMMY,IER)
          KAPPA_ERR_MULT=RDUMMY
          IF(IER.EQ.0) CALL EZGET('MZ0',RDUMMY,IER)
          ZMASS=RDUMMY
          IF(IER.EQ.0) CALL EZGET('EPSILON',RDUMMY,IER)
          EPSCHI=RDUMMY
          IF(IER.EQ.0) CALL EZGET('CHI_MAX',RDUMMY,IER)
          DCHI_MAX=RDUMMY
          IF(IER.EQ.0) CALL EZGET('N_GRID_FRAC',RDUMMY,IER)
          DELTA_CHI=RDUMMY
          IF(IER.EQ.0)   CALL EZGET('COMPUTE_MET_ERRORS',DOMETERRS,IER)
          IF(IER.EQ.0)   CALL EZGET('COMPUTE_MUON_ERRORS',DOERRORS,IER)
          IF(IER.EQ.0)   CALL EZGET('MET_CONSTRAINT',MET_CONSTRAINT,IER)
          ERRTXT='RCP parameter error'
          IF(IER.NE.0)
     >         CALL ERRMSG('Z_FIT_MUMU_RCPPAR','Z_FIT_MUMU',ERRTXT,'F')
C
          CALL EZGET('USE_OFF_DIAGONAL',USE_OFF_DIAGONAL,IER) 
          IF(IER.NE.0) THEN
            ERRTXT='RCP parameter missing: USE_OFF_DIAGONAL. Set FALSE'
            CALL ERRMSG('NO_CORRELATION','Z_FIT_MUMU',ERRTXT,'W')
            USE_OFF_DIAGONAL = .FALSE.
          ENDIF 
C
          NGRID=INT(DCHI_MAX/DELTA_CHI)
          IF( MET_CONSTRAINT.NE.1 .AND. MET_CONSTRAINT.NE.3 ) THEN
            WRITE(ERRTXT,1234) MET_CONSTRAINT
 1234       FORMAT('Constraint ',I2,' is invalid.  Setting to 3(Full)')
            CALL ERRMSG('UNKNOWN_CONSTRAINT','Z_FIT_MUMU',ERRTXT,'W')
            MET_CONSTRAINT=3
          ENDIF
        ENDIF
      ENDIF
C
C  Convert to double precision before trig functions are used
C
      DO I=1,3
        DVP1(I)=VP(I,1)
        DVP2(I)=VP(I,2)
      ENDDO
      DVP1(3)=ABS(DVP1(3))
      DVP2(3)=ABS(DVP2(3))
C
C  Get the muon kappa errors...
C
      IF(DOERRORS) THEN
        SIGMA1SQR=KAPPA_ERR_MULT*DVP1(3)**2 + KAPPA_ERR_CONST
        SIGMA2SQR=KAPPA_ERR_MULT*DVP2(3)**2 + KAPPA_ERR_CONST
      ELSE
        SIGMA1SQR=SIGMAP(1)
        SIGMA2SQR=SIGMAP(2)
      ENDIF
C
C  Use muon with better momentum measurement in fit...
C
      IF( SQRT(SIGMA1SQR).GT.SQRT(SIGMA2SQR) ) THEN
        ORDER=2
        DO I=1,3
          X=DVP1(I)
          DVP1(I)=DVP2(I)
          DVP2(I)=X
        ENDDO
        X=SIGMA1SQR
        SIGMA1SQR=SIGMA2SQR
        SIGMA2SQR=X
      ELSE
        ORDER=1
      ENDIF
C
C  Now compute constants for Chi^2 function w/correct ordering for fit...
C
      KAPPA1_MEAS=DVP1(3)
      NX1=COS(DVP1(2))*SIN(DVP1(1))
      NY1=SIN(DVP1(2))*SIN(DVP1(1))
      NZ1=COS(DVP1(1))
C
      KAPPA2_MEAS=DVP2(3)
      NX2=COS(DVP2(2))*SIN(DVP2(1))
      NY2=SIN(DVP2(2))*SIN(DVP2(1))
      NZ2=COS(DVP2(1))
C
      ET_MEAS(1)=VMET(1)
      ET_MEAS(2)=VMET(2)
      K = 1.0D0 - NX1*NX2 - NY1*NY2 - NZ1*NZ2
      IF( K.LT.0 ) K=0.0
      K = SQRT(2.0*K)/ZMASS
      K2 = K**2
C
C  Check for calculating MET errors...
C
      CALL VZERO(EMETTMP,4)
      IF(DOMETERRS) THEN
        ET=SQRT(ET_MEAS(1)**2+ET_MEAS(2)**2)
        ERRSCET=MET_ERR_CONST+MET_ERR_MULT*SCET
        RX=ET_MEAS(1)/ET
        RY=ET_MEAS(2)/ET
        EMETTMP(1,1)=(ERRSCET*RX)**2
        EMETTMP(2,2)=(ERRSCET*RY)**2
        IF( USE_OFF_DIAGONAL ) THEN
          EMETTMP(1,2)=(1.6**2)*RX*RY*ERRSCET**2
          EMETTMP(2,1)=EMETTMP(1,2)
        ENDIF
      ELSE
        EMETTMP(1,1)=EMET(1,1)
        EMETTMP(2,2)=EMET(2,2)
        EMETTMP(1,2)=EMET(1,2)
        EMETTMP(2,1)=EMET(2,1)
      ENDIF
C
C  Apply the scale factor...
C
      EMETTMP(1,1)=EMETTMP(1,1)*MET_ERR_SCALE**2
      EMETTMP(2,2)=EMETTMP(2,2)*MET_ERR_SCALE**2
      EMETTMP(1,2)=EMETTMP(1,2)*MET_ERR_SCALE**2
      EMETTMP(2,1)=EMETTMP(2,1)*MET_ERR_SCALE**2
C
C  Invert the (symmetric) error matrix...
C
      DET=EMETTMP(1,1)*EMETTMP(2,2)-EMETTMP(1,2)*EMETTMP(2,1)
      IF( DET.EQ.0 ) THEN
        FITPAR(4)=-2.0
        FITPAR(5)=-2.0
        IER=2
        GOTO 999
      ENDIF
C
      DO I=1,2
      DO J=1,2
        II=MOD(I,2)+1
        JJ=MOD(J,2)+1
        EMETINV(I,J)=(-1)**(I+J)*EMETTMP(II,JJ)/DET
      ENDDO
      ENDDO
C
      ERR_MET(1)=EMETINV(1,1)
      ERR_MET(2)=EMETINV(2,2)
      ERR_MET(3)=EMETINV(1,2)+EMETINV(2,1)
C
C  Do we really want to perform minimization?
C
      IF( IFLAG.EQ.0 ) GOTO 999
C
C  Do the minimization.  First do a grid search for good region, 
C  then call minimizer...
C
      DX=SQRT(SIGMA1SQR)*SQRT(DCHI_MAX)
      X0=KAPPA1_MEAS-DX
      X1=KAPPA1_MEAS+DX
      DX=DX/NGRID
      CHIMIN=GRID_MINIMIZER(X0,X1,DX,ZFE_CHI2,XMIN)
C
      X0=XMIN-DX
      X1=XMIN+DX
      CHIMIN=GOLDEN_SECANT(X0,XMIN,X1,ZFE_CHI2,EPSCHI,KAPPA_MIN)
      IF( KAPPA_MIN.EQ.X0 ) THEN
        ERRTXT='Minimum at lower boundary'
        CALL ERRMSG('Z_FIT_MUMU_LOWER','Z_FIT_MUMU',ERRTXT,'W')
      ELSEIF( KAPPA_MIN.EQ.X1 ) THEN
        ERRTXT='Minimum at upper boundary'
        CALL ERRMSG('Z_FIT_MUMU_UPPER','Z_FIT_MUMU',ERRTXT,'W')
      ENDIF
C
C  Copy the output into an array...
C
      CHI2=CHIMIN
      FITPAR(1)=2
      FITPAR(2)=MET_CONSTRAINT
      FITPAR(3)=KAPPA_MIN
      FITPAR(4)=CHIMIN
      FITPAR(5)=PROB(CHI2,MET_CONSTRAINT)
      FITPAR(6)=KAPPA_MIN
      FITPAR(7)=K2/(KAPPA_MIN+EPS)
      FITPAR(8)=NX1/ABS(FITPAR(6))+NX2/ABS(FITPAR(7))
      FITPAR(9)=NY1/ABS(FITPAR(6))+NY2/ABS(FITPAR(7))
      FITPAR(11)=PULL(1)
      FITPAR(12)=PULL(2)
      FITPAR(13)=PULL(3)
      FITPAR(14)=PULL(4)
C
      IF(ORDER.EQ.2) THEN
        FITPAR(6)=K2/(KAPPA_MIN+EPS)
        FITPAR(7)=KAPPA_MIN
        FITPAR(11)=PULL(2)
        FITPAR(12)=PULL(1)
      ENDIF
C
  999 RETURN
      END
