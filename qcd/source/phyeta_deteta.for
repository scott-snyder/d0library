      SUBROUTINE PHYETA_DETETA(Z_VTX,PHYETA,DETETA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert Physics eta to Detector eta
C-
C-   Inputs  : Z_VTX, PHYETA
C-   Outputs : DETETA
C-   Controls: NONE
C-
C-   Created  30-JAN-1994   Freedy Nang
C-   Based on a couple of routines written by Serban D. Protopopescu
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    Z_VTX, PHYETA, PHYTHETA, DETETA, DETTHETA
      REAL    TANTH, R_CC, Z_EC, SGN, PI, TWOPI
      REAL    Z_CC, R_EC
      PARAMETER( PI = 3.14159265358979323846 )
      PARAMETER( TWOPI = 6.28318530717958647693 )
      DATA R_CC,Z_EC/91.6,178.9/
C----------------------------------------------------------------------
      IF ( ABS(Z_VTX).GT.150. ) THEN
        Z_VTX = -11.
      ENDIF
      PHYTHETA = 2.*ATAN( EXP(-1.*PHYETA) )
      TANTH=TAN(PHYTHETA)
      Z_CC = R_CC/TANTH+Z_VTX
      IF ( ABS(Z_CC).LT.Z_EC ) THEN
        DETTHETA=ATAN2(R_CC,Z_CC)
        IF ( DETTHETA.LE.0. ) DETTHETA=DETTHETA+TWOPI
      ELSE
        SGN=SIGN(1.,TANTH)
        R_EC=(Z_EC*SGN-Z_VTX)*TANTH
        DETTHETA=ATAN2(R_EC,Z_EC*SGN)
        IF ( DETTHETA.LE.0. ) DETTHETA=DETTHETA+TWOPI
      ENDIF
      DETETA = -ALOG( TAN(DETTHETA/2.) )
  999 RETURN
      END

