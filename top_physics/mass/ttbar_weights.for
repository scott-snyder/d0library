      DOUBLE PRECISION FUNCTION TTBAR_WEIGHTS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GIVEN THE KINEMATICS, WORK OUT PARTON MODEL WEIGHTS
C-   FOR CONFIGURATION
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   8-FEB-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PARTON_KINE.INC'
      REAL    STRUC
      DOUBLE PRECISION    GG,QQ,NORM_GLUON,NORM_QUARK
      DOUBLE PRECISION    DSIGMA_DT_GLUON, DSIGMA_DT_QUARK ,
     &  DECAY_WEIGHTS
      DOUBLE PRECISION    TMASSL,DSIG,TMASSQ,TPQ,HSH,
     &  TMIN,TMAX,EPSIN,EPSOUT
      DOUBLE PRECISION    DGQUAD
      EXTERNAL DSIGMA_DT_GLUON,DSIGMA_DT_QUARK
      INTEGER NGAUSS,IER
      LOGICAL first
      SAVE first
      DATA first / .true. /
      DATA TMASSL /0.0/  !LAST TOP MASS
C----------------------------------------------------------------------
      IF( first ) THEN
        first = .false.
        CALL EZPICK('TOP_MASS_RCP')
        CALL EZGET('NGAUSS',NGAUSS,IER)
        CALL EZRSET
      ENDIF
C
      IH = 1120     !PROTON
      IQ = 1   !Gluon
C
      STR_FN1 = STRUC(X1,QSQ,IQ,IH)/X1
      STR_FN2 = STRUC(X2,QSQ,IQ,IH)/X2
C
      GG  = STR_FN1*STR_FN2
C
      IF ( TESTQ ) THEN
        CALL DO_HF1D(198,X1,STR_FN1)
        CALL DO_HF1D(200,X2,STR_FN2)
C
        CALL DO_HF1D(298,X1,1.0D0)
        CALL DO_HF1D(300,X2,1.0D0)
C
      ENDIF
C
      IQ = 2   !Up quark
C
      STR_FN1 = 2.0*STRUC(X1,QSQ,IQ,IH)/X1
      STR_FN2 = 2.0*STRUC(X2,QSQ,IQ,IH)/X2
C
      IQ = 4   !Down quark
      STR_FN1 = STR_FN1 + STRUC(X1,QSQ,IQ,IH)/X1
      STR_FN2 = STR_FN2 + STRUC(X2,QSQ,IQ,IH)/X2
      STR_FN1 = STR_FN1/3.0
      STR_FN2 = STR_FN2/3.0
C
      QQ = STR_FN1*STR_FN2
C
      IF ( TESTQ ) THEN
        CALL DO_HF1D(199,X1,STR_FN1)
        CALL DO_HF1D(201,X2,STR_FN2)
C
        CALL DO_HF1D(299,X1,1.0)
        CALL DO_HF1D(301,X2,1.0)
C
      ENDIF
C
      IF ( TMASS.NE.TMASSL ) THEN
        TMASSL = TMASS
        HSH = 0.5*SH
        TMASSQ = TMASS*TMASS
        TPQ = HSH*SQRT(1.0-4.0*TMASSQ/SH)
        TMIN = -(HSH-TMASSQ+TPQ)
        TMAX = -(HSH-TMASSQ-TPQ)
        NORM_GLUON = DGQUAD(DSIGMA_DT_GLUON,TMIN,TMAX,NGAUSS)
        NORM_QUARK = DGQUAD(DSIGMA_DT_QUARK,TMIN,TMAX,NGAUSS)
C
C ****  CONVERT TO DOUBLE PRECISION
C
      ENDIF
C
      DSIG = DSIGMA_DT_GLUON(TH)/NORM_GLUON
      GG = GG*DSIG
C
      IF ( TESTQ ) THEN
        CALL DO_HF1D(202,DSQRT(ABS(TH)),DSIG)
        CALL DO_HF1D(302,DSQRT(ABS(TH)),1.0D0)
      ENDIF
C
      DSIG = DSIGMA_DT_QUARK(TH)/NORM_QUARK
      QQ = QQ*DSIG
C
      IF ( TESTQ ) THEN
        CALL DO_HF1D(203,DSQRT(ABS(TH)),DSIG)
        CALL DO_HF1D(303,DSQRT(ABS(TH)),1.0D0)
      ENDIF
C
      IF ( TESTQ ) THEN
        CALL DO_HF1D(204,TMASS,NORM_QUARK)
        CALL DO_HF1D(205,TMASS,NORM_GLUON)
        CALL DO_HF1D(304,TMASS,1.0)
      ENDIF
C
      TTBAR_WEIGHTS = (GG+QQ)*DECAY_WEIGHTS()
C
  999 RETURN
      END
