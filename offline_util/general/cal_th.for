      FUNCTION CAL_TH(TH,Z)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      calculate corresponding theta from 0,0 to calorimeter surface
C-   Returned value  : calorimeter theta
C-
C-   Inputs  : 
C-   TH = true physical theta
C-   Z  = z of vertex
C-
C-   Created   7-FEB-1992   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      REAL CAL_TH
      REAL TH,Z
      REAL R_CC,Z_EC,R_EC,Z_CC,TANTH,SGN
      DATA R_CC,Z_EC/91.6,178.9/
C----------------------------------------------------------------------
C
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
  999 RETURN
      END
