      REAL FUNCTION SAMUS_BDL(P,FS,ETA,RUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate SAMUS B*dL parameter for muon
C-
C-   Inputs  :
C-              P     -   Muon momentum
C-                        P > 0.0  for mu +
C-                        P < 0.0  for mu -
C
C-              FI    -   Fi angle  ( 0 ... 2 Pi )
C-
C-              ETA   -   Pseudorapidity
C-                        ETA > 0.0  for z > 0.0
C-                        ETA < 0.0  for z < 0.0
C-
C-              RUN   -   Run type
C-                        Not implemented yet
C-   Outputs :
C-
C-   Controls:
C-
C-   Created  10-JUL-1996   O.Eroshin
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER       RUN
      LOGICAL       FIRST
      REAL          X, Y
      REAL          P, FI, FS, PI, ETA, PIN, FIIN, ETAIN, BDL
      REAL	    SAMUS_BDL_MU_PLUS,
     +              SAMUS_BDL_MU_MINUS
C----------------------------------------------------------------------
      DATA          FIRST /.TRUE./
C
C..........
C
      IF( FIRST )             THEN
        PI = 4. * ATAN( 1. )
        FIRST = .FALSE.
      ENDIF
C
      PIN             = ABS(P)
      ETAIN           = ABS(ETA)
C
      IF ( PIN .LE. 6.0 ) PIN = 6.0
      IF ( PIN .GT. 200.) PIN = 200.
C
      X    = COS(FS)
      Y    = SIN(FS)
C
      FI = ATAN2(X,Y)
      IF (FI .LT. 0.0) FI = 2*PI-ABS(FI)
C
      IF (FI .LT. 0.1) FI = 0.1
      IF (FI .GT. 6.2) FI = 6.2
C
      FIIN = FI
C
      IF (ETA .GE. 0.0)				    THEN
C
	IF (P .GT. 0.0)				    THEN
	    BDL = SAMUS_BDL_MU_PLUS   (PIN,FIIN,ETAIN)
						    ELSE
	    BDL = SAMUS_BDL_MU_MINUS  (PIN,FIIN,ETAIN)
	END IF
C
						    ELSE
        IF (FI .LE. PI ) FIIN = PI - FI
        IF (FI .GT. PI ) FIIN = 3*PI-FI
C
	IF (P .GT. 0.0)				    THEN
	    BDL = SAMUS_BDL_MU_MINUS  (PIN,FIIN,ETAIN)
						    ELSE
	    BDL = SAMUS_BDL_MU_PLUS   (PIN,FIIN,ETAIN)
	END IF
      END IF
C
      IF (BDL .LT. 0.0) BDL = 0.0
      SAMUS_BDL = BDL
C
  999 RETURN
      END
