      SUBROUTINE SAMUS_RESOLUTION
     +                           (P,FI,ETA,DP,SP,DFI,SFI,DETA,SETA,RUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate SAMUS resolution parameters for muons
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
C-		DP    -   Shift in P     ( DP = P(measured) - P(real) )
C-              SP    -   Sigma P
C-		DFI   -   Shift in FI    ( DFI = FI(measured) - FI(real) )
C-              SFI   -   Sigma FI
C-		DETA  -   Shift in ETA   ( DETA = ETA(measured) - ETA(real) )
C-              SETA  -   Sigma ETA
C-
C-   Controls:
C-
C-   Created  10-JUN-1995   O.Eroshin
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER       RUN
      LOGICAL       FIRST
      REAL          P, FI, PI, ETA, PIN, FIIN, ETAIN
      REAL          DP, SP, PR, DFI, SFI, DETA, SETA
      REAL	    SAMUS_P_DIF_MU_PLUS_1B,
     +              SAMUS_P_DIF_MU_MINUS_1B,
     +              SAMUS_FI_DIF_MU_PLUS_1B,
     +              SAMUS_FI_DIF_MU_MINUS_1B,
     +              SAMUS_ETA_DIF_MU_PLUS_1B,
     +              SAMUS_ETA_DIF_MU_MINUS_1B,
     +              SAMUS_P_SIG_MU_PLUS_1B,
     +              SAMUS_P_SIG_MU_MINUS_1B,
     +              SAMUS_FI_SIG_MU_PLUS_1B,
     +              SAMUS_FI_SIG_MU_MINUS_1B,
     +              SAMUS_ETA_SIG_MU_PLUS_1B,
     +              SAMUS_ETA_SIG_MU_MINUS_1B
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
      FIIN = FI
C
      IF (ETA .GE. 0.0)				    THEN
C
	IF (P .GT. 0.0)				    THEN
	    DP   = SAMUS_P_DIF_MU_PLUS_1B   (PIN,FIIN,ETAIN)
	    SP   = SAMUS_P_SIG_MU_PLUS_1B   (PIN,FIIN,ETAIN)
	    DFI  = SAMUS_FI_DIF_MU_PLUS_1B  (PIN,FIIN,ETAIN)
	    SFI  = SAMUS_FI_SIG_MU_PLUS_1B  (PIN,FIIN,ETAIN)
	    DETA = SAMUS_ETA_DIF_MU_PLUS_1B (PIN,FIIN,ETAIN)
	    SETA = SAMUS_ETA_SIG_MU_PLUS_1B (PIN,FIIN,ETAIN)
						    ELSE
	    DP   = SAMUS_P_DIF_MU_MINUS_1B  (PIN,FIIN,ETAIN)
	    SP   = SAMUS_P_SIG_MU_MINUS_1B  (PIN,FIIN,ETAIN)
	    DFI  = SAMUS_FI_DIF_MU_MINUS_1B (PIN,FIIN,ETAIN)
	    SFI  = SAMUS_FI_SIG_MU_MINUS_1B (PIN,FIIN,ETAIN)
	    DETA = SAMUS_ETA_DIF_MU_MINUS_1B(PIN,FIIN,ETAIN)
	    SETA = SAMUS_ETA_SIG_MU_MINUS_1B(PIN,FIIN,ETAIN)
	END IF
C
	DFI  = -0.001*DFI
	DETA = -0.001*DETA
						    ELSE
        IF (FI .LE. PI ) FIIN = PI - FI
        IF (FI .GT. PI ) FIIN = 3*PI-FI
C
	IF (P .GT. 0.0)				    THEN
	    DP   = SAMUS_P_DIF_MU_MINUS_1B  (PIN,FIIN,ETAIN)
	    SP   = SAMUS_P_SIG_MU_MINUS_1B  (PIN,FIIN,ETAIN)
	    DFI  = SAMUS_FI_DIF_MU_MINUS_1B (PIN,FIIN,ETAIN)
	    SFI  = SAMUS_FI_SIG_MU_MINUS_1B (PIN,FIIN,ETAIN)
	    DETA = SAMUS_ETA_DIF_MU_MINUS_1B(PIN,FIIN,ETAIN)
	    SETA = SAMUS_ETA_SIG_MU_MINUS_1B(PIN,FIIN,ETAIN)
						    ELSE
	    DP   = SAMUS_P_DIF_MU_PLUS_1B   (PIN,FIIN,ETAIN)
	    SP   = SAMUS_P_SIG_MU_PLUS_1B   (PIN,FIIN,ETAIN)
	    DFI  = SAMUS_FI_DIF_MU_PLUS_1B  (PIN,FIIN,ETAIN)
	    SFI  = SAMUS_FI_SIG_MU_PLUS_1B  (PIN,FIIN,ETAIN)
	    DETA = SAMUS_ETA_DIF_MU_PLUS_1B (PIN,FIIN,ETAIN)
	    SETA = SAMUS_ETA_SIG_MU_PLUS_1B (PIN,FIIN,ETAIN)
	END IF
C
	DFI  = 0.001*DFI
	DETA = 0.001*DETA
C
      END IF
C
C
      IF (P .GT. 0)				    THEN
	SP   = 0.011 *SP*PIN
	SFI  = 0.0011*SFI
	SETA = 0.001*SETA
C
	DP   = 0.01*DP/PIN
	DP   = 0.75*(1./(1./PIN+DP)-PIN)
						    ELSE
	SP   = 0.011 *SP*PIN
	SFI  = 0.0011*SFI
	SETA = 0.001*SETA
C
	DP   = 0.01*DP/PIN
	DP   = 0.5*(PIN-1./(1./PIN+DP))
      END IF
C
  999 RETURN
      END
