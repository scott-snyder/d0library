      SUBROUTINE FIND_LAMBDA_LIMITS(IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : FIND THE LIMITS OF THE NEUTRINO ET
C-   FOR A GIVEN NEUTRINO DIRECTION. 1ST ELECTRON ASSUMED
C-
C-   Inputs  : PHI = DIRECTION OF NEUTRINO IN DEGREES (AZIMUTH)
C-   Outputs : LAMBDA_LO,LAMBDA_HI LO AND HI VALUES OF ET THAT
C-             CAN BE PRODUCED BY W DECAY
C-             IER =NON ZERO IF NO SOLUTION AVAILABLE
C-   Controls:
C-
C-   Created  24-JAN-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:KINEQ.INC'
      INCLUDE 'D0$INC:PI.DEF'
      DOUBLE PRECISION    A,B,Y,MUC,TEMP,C,AA,BB,CC,D
      DOUBLE PRECISION    PZLO1,PZLO2,PZHI1,PZHI2,ROOT,MAXD
      REAL    MAX_ABS_LAMBDA
      INTEGER IER
      LOGICAL first
      SAVE first
      DATA first / .true. /
C----------------------------------------------------------------------
      IF( first ) THEN
        first = .false.
        CALL EZPICK('TOP_MASS_RCP')
        CALL EZGET('MAX_ABS_LAMBDA',MAX_ABS_LAMBDA,IER)
        MAXD=MAX_ABS_LAMBDA
        CALL EZRSET
      ENDIF
C
      A = WMASS*WMASS/2.0
      B = LEPTON1(4)    !ENERGY OF 1ST ELECTTRON
      Y = LEPTON1(3)
      MUC = LEPTON1(1)*COS(PHI*RADIAN) + LEPTON1(2)*SIN(PHI*RADIAN)
      IER = 0
      ROOT = B*B-Y*Y
      IF ( ROOT.LT.0.0 ) THEN
        IER = 1
        RETURN
      ELSE
        ROOT = DSQRT(ROOT)
      ENDIF
C
      LAMBDA_LO = A/(ROOT-MUC)
      LAMBDA_HI = A/(-ROOT-MUC)
      IF ( ABS(LAMBDA_LO).GT.MAXD ) THEN
        LAMBDA_LO = SIGN(MAXD,LAMBDA_LO)
      ENDIF
      IF ( ABS(LAMBDA_HI).GT.MAXD ) THEN
        LAMBDA_HI = SIGN(MAXD,LAMBDA_HI)
      ENDIF
C
      IF ( LAMBDA_LO.GT.LAMBDA_HI ) THEN
        TEMP = LAMBDA_LO
        LAMBDA_LO = LAMBDA_HI
        LAMBDA_HI = TEMP
      ENDIF
C
C ****  now find the two solutions at lambda = lambdalo
C ****  FOR TESTING
C
      LAMBDA = LAMBDA_LO
      C = (A + LAMBDA*MUC)/B
      D = (Y/B)
      BB = -C*D
      AA = 1-D**2
      CC = LAMBDA**2-C*C
C
      ROOT = BB*BB-AA*CC
      IF ( ROOT.LT.0.0 ) THEN
        ROOT = 0.0
      ENDIF
      ROOT = DSQRT(ROOT)
C
      PZLO1 = (-BB - ROOT)/AA
      PZHI1 = (-BB + ROOT)/AA
C
  999 RETURN
      END
