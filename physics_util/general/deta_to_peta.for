      REAL FUNCTION DETA_TO_PETA(DETA,Z)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : convert detector eta to physics eta
C-
C-   Returned value  : Physics eta
C-   Inputs  : DETA [R]    : Detector eta
C-   Outputs : Z [R]: Z position of vertex
C-   Controls:
C-
C-   Created   14-APR-1994  Victor Daniel Elvira
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      REAL R_CC,Z_EC,R_EC,Z_CC
      REAL DETA,DTH,PTH,Z,TANDTH,SGN
C----------------------------------------------------------------------
      DATA R_CC,Z_EC/91.6,178.9/
C----------------------------------------------------------------------
C-
      DTH=2.*ATAN(EXP(-DETA))
      IF (DTH.LT.0.) DTH=DTH+TWOPI
      TANDTH=TAN(DTH)
C-
      Z_CC=R_CC/TANDTH
      SGN=SIGN(1.,TANDTH)
C-
      IF (ABS(Z_CC).LT.Z_EC) THEN
        PTH=ATAN2(R_CC,Z_CC-Z)
      ELSE
        R_EC=(Z_EC*SGN)*TANDTH
        PTH=ATAN2(R_EC,Z_EC*SGN-Z)
      ENDIF
      IF (PTH.LT.0.) PTH=PTH+TWOPI
      DETA_TO_PETA=-LOG(TAN(PTH/2.))
C----------------------------------------------------------------------
  999 RETURN
      END
