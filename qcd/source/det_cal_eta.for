      SUBROUTINE DET_CAL_ETA(Z,PETA,ETA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : Z vertex and PETA physics eta
C-   Outputs : ETA detector eta at EM/HAD interface
C-   Controls: 
C-
C-   Created  28-APR-1993   Brent J. May
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL Z, PETA, ETA
      INCLUDE 'D0$INC:PI.DEF'
      REAL CAL_TH
      REAL TH
      REAL R_CC,Z_EC,R_EC,Z_CC,TANTH,SGN
      DATA R_CC,Z_EC/95.,178./
C----------------------------------------------------------------------
      TH = 2.0*ATAN(EXP(-PETA))
      TANTH=TAN(TH)
      Z_CC=R_CC/TANTH+Z
      IF(ABS(Z_CC).LT.Z_EC) THEN
        CAL_TH=ATAN2(R_CC,Z_CC)
        IF(CAL_TH.LT.0.) CAL_TH=CAL_TH+TWOPI
      ELSE
        SGN=SIGN(1.,TANTH)
        R_EC=(Z_EC*SGN-Z)*TANTH
        CAL_TH=ATAN2(R_EC,Z_EC*SGN)
        IF(CAL_TH.LT.0.) CAL_TH=CAL_TH+TWOPI
      ENDIF
      ETA=-ALOG(TAN(CAL_TH/2.))
  999 RETURN
      END
