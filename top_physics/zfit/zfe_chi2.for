C------------------------------------------------------------------------
      REAL*8 FUNCTION ZFE_CHI2(X)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Compute the Chi^2 used in Z_FIT_MUMU.  The
C-     coefficients must be computed elsewhere (In Z_FIT_MUMU).
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  22-Apr-1994   John D. Hobbs
C-   Modified 17-May-1994   JDH - Opposite sign introduced into
C-       MZ constraint.  Fit parameter is muon with better errors
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      REAL*8 X,EPS,KAPPA1,KAPPA2,SUMPX,SUMPY,SUMP,NX,NY
      REAL*8 NX0,NY0,DOT,METPROJ,ERRPROJ,MET0
      REAL*8 TERM1,TERM2,TERM3,TERM4,TERM5
      PARAMETER(EPS=1.0D-10)
      CHARACTER*60 ERRTXT
      INCLUDE 'D0$INC:Z_FIT_MUMU.INC'
C-----------------------------------------------------------------------
*
*  Pure momentum terms...
*
      KAPPA1=(X+EPS)
      KAPPA2=K2/KAPPA1
*
      TERM1 = (KAPPA1-KAPPA1_MEAS) / SQRT(SIGMA1SQR)
      TERM2 = (KAPPA2-KAPPA2_MEAS) / SQRT(SIGMA2SQR)
*
      SUMPX = NX1/ABS(KAPPA1)+NX2/ABS(KAPPA2)
      SUMPY = NY1/ABS(KAPPA1)+NY2/ABS(KAPPA2)
      SUMP  = SQRT(SUMPX**2+SUMPY**2)
      NX=SUMPX/SUMP
      NY=SUMPY/SUMP
*
*  Missing ET component
*
      MET0 = SQRT(ET_MEAS(1)**2+ET_MEAS(2)**2)
      NX0=ET_MEAS(1)/MET0
      NY0=ET_MEAS(2)/MET0
*
      IF( MET_CONSTRAINT.EQ.1 ) THEN
        TERM3=0.0
        TERM4=0.0
        TERM5=0.0
      ELSEIF( MET_CONSTRAINT.EQ.3 ) THEN
        TERM3=(ET_MEAS(1)-SUMPX)*ERR_MET(1)*(ET_MEAS(1)-SUMPX)
        TERM4=(ET_MEAS(2)-SUMPY)*ERR_MET(2)*(ET_MEAS(2)-SUMPY)
        TERM5=(ET_MEAS(1)-SUMPX)*ERR_MET(3)*(ET_MEAS(2)-SUMPY)
      ELSE 
        DOT=NX0*NX+NY0*NY
        METPROJ=MET0*DOT
        ERRPROJ=SQRT( ERR_MET(1)*NX*NX + ERR_MET(2)*NY*NY 
     >     + ERR_MET(3)*NX*NY)
        TERM3=(METPROJ-SUMP)/ERRPROJ
        TERM4=0.0
        TERM5=0.0
      ENDIF
C
C  Put it together...
C
      ZFE_CHI2 = TERM1**2 + TERM2**2 + TERM3 + TERM4 + TERM5
      PULL(1)=TERM1
      PULL(2)=TERM2
      PULL(3)=TERM3
      PULL(4)=TERM4
C
      IF( MET_CONSTRAINT.LT.1 .OR. MET_CONSTRAINT.GT.3 ) THEN
        ERRTXT='Unknown MET constraint type.  Use 3 dof'
        CALL ERRMSG('Z_FIT_MUMU_METCONS','Z_FIT_MUMU_METCONS',
     >        ERRTXT,'W')
        MET_CONSTRAINT=3
      ENDIF

C
 999  RETURN
      END
