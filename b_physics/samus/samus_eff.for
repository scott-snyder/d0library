      REAL FUNCTION SAMUS_EFF (P,FS,ETA,TYPE,RUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate SAMUS efficiency for muons
C-
C-   Returned value  : SAMUS efficiency for muon
C-   Inputs  :
C-              P     -   Muon momentum
C-                        P > 0.0  for mu +
C-                        P < 0.0  for mu -
C
C-              FS    -   Fi angle  ( 0 ... 2 Pi )
C-
C-              ETA   -   Pseudorapidity
C-                        ETA > 0.0  for z > 0.0
C-                        ETA < 0.0  for z < 0.0
C-
C-              TYPE  -   Efficiency type
C-                        'LEVL'  -  for SAMUS L1 efficiency
C-                        'TRIG'  -  for SAMUS L1 & L1.5 efficiency (1B ONLY)
C-                        'ALL'   -  for all detection efficiency
C-
C-              RUN   -   Run type
C-                        1:  run 1A
C-                        2:  run 1B
C-                        3:  run 1B - new L1.5 tables
C-                        4:  run 1C
C-   Outputs :
C-   Controls:
C-
C-   Created  29-OCT-1993   O.Eroshin
C-   Modified 12-DEC-1993   E.Kozlovsky made the corrected routiones
C-                          which  used the real symetry of  the
C-                          MAG. field
C-   Modified 10-SEP-1994   O.Eroshin data for run 1b
C-   Modified 10-JUN-1995   O.Eroshin data for run 1b ( New L1.5 tables )
C-   Modified 10-JUL-1996   O.Eroshin data for run 1C
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      REAL SE,SD
      PARAMETER (SE = 1.17)
      PARAMETER (SD = 0.017)
C----------------------------------------------------------------------
      INTEGER       RUN
      CHARACTER*4   TYPE
      LOGICAL       FIRST
      REAL          P, FI, FS, PI, ETA, PIN, FIIN, ETAIN
      REAL          SAMUS_MIN_BIAS_CORRECTION
      REAL          SAMUS_RECO_EFF_MU_PLUS_1A,
     +              SAMUS_RECO_EFF_MU_MINUS_1A,
     +              SAMUS_LEVEL1_EFF_MU_PLUS_1A,
     +              SAMUS_LEVEL1_EFF_MU_MINUS_1A,
     +              SAMUS_RECO_EFF_MU_PLUS_1B,
     +              SAMUS_RECO_EFF_MU_MINUS_1B,
     +              SAMUS_LEVEL1_EFF_MU_PLUS_1B,
     +              SAMUS_LEVEL1_EFF_MU_MINUS_1B,
     +              SAMUS_LEVEL15_EFF_MU_PLUS_1B,
     +              SAMUS_LEVEL15_EFF_MU_MINUS_1B,
     +              SAMUS_RECO_EFF_MU_PLUS_1B_2,
     +              SAMUS_RECO_EFF_MU_MINUS_1B_2,
     +              SAMUS_LEVEL15_EFF_MU_PLUS_1B_2,
     +              SAMUS_LEVEL15_EFF_MU_MINUS_1B_2
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
      SAMUS_EFF       = 0.0
C
      FI              = FS
      IF (FI .LT. 0.1) FI = 0.1
      IF (FI .GT. 6.2) FI = 6.2
C
      PIN             = ABS(P)
      ETAIN           = ABS(ETA)
C
      IF ( PIN .LE. 6.0 ) RETURN
      IF ( PIN .GT. 200.) PIN = 200.
C
      IF (RUN .EQ. 1)         THEN
C
C......................................................................
C       Trnasform the fi angles to I quad
C..
        IF( FI .GT. 0.5*PI .AND. FI .LE. PI ) THEN
*
          FIIN  = PI - FI
*
        ELSEIF( FI .GT. PI .AND. FI .LE. 1.5*PI ) THEN
*
          FIIN = FI - PI
*
        ELSEIF( FI .GT. 1.5*PI .AND. FI .LE. 2.*PI ) THEN
*
          FIIN = 2.*PI - FI
*
        ELSE
*
          FIIN = FI
*
        ENDIF
C......................................................................
C
        IF (ETA .GE. 0.0)       THEN
C..
          IF (TYPE(1:1) .EQ. 'L' .OR. TYPE(1:1) .EQ. 'l') THEN
            IF (P .GT. 0.0)     THEN
              SAMUS_EFF = SAMUS_LEVEL1_EFF_MU_PLUS_1A (PIN,FIIN,ETAIN)
            ELSE
              SAMUS_EFF = SAMUS_LEVEL1_EFF_MU_MINUS_1A(PIN,FIIN,ETAIN)
            END IF
          END IF
C..
          IF (TYPE(1:1) .EQ. 'A' .OR. TYPE(1:1) .EQ. 'a') THEN
            IF (P .GT. 0.0)     THEN
              SAMUS_EFF = SAMUS_RECO_EFF_MU_PLUS_1A (PIN,FIIN,ETAIN)
            ELSE
              SAMUS_EFF = SAMUS_RECO_EFF_MU_MINUS_1A(PIN,FIIN,ETAIN)
            END IF
          END IF
        ELSE
C..
          IF (TYPE(1:1) .EQ. 'L' .OR. TYPE(1:1) .EQ. 'l') THEN
            IF (P .GT. 0.0)     THEN
              SAMUS_EFF = SAMUS_LEVEL1_EFF_MU_MINUS_1A(PIN,FIIN,ETAIN)
            ELSE
              SAMUS_EFF = SAMUS_LEVEL1_EFF_MU_PLUS_1A (PIN,FIIN,ETAIN)
            END IF
          END IF
C..
          IF (TYPE(1:1) .EQ. 'A' .OR. TYPE(1:1) .EQ. 'a') THEN
            IF (P .GT. 0.0)     THEN
              SAMUS_EFF = SAMUS_RECO_EFF_MU_MINUS_1A(PIN,FIIN,ETAIN)
            ELSE
              SAMUS_EFF = SAMUS_RECO_EFF_MU_PLUS_1A (PIN,FIIN,ETAIN)
            END IF
          END IF
        END IF
C
C......   Multiple interaction correction
C
        SAMUS_EFF = SE*SAMUS_EFF - SD
                                                          ELSE
        FIIN = FI
C
	IF (RUN .EQ. 2)					  THEN
C..
        IF (ETA .GE. 0.0)       THEN
C..
          IF (TYPE(1:1) .EQ. 'L' .OR. TYPE(1:1) .EQ. 'l') THEN
            IF (P .GT. 0.0)     THEN
              SAMUS_EFF = SAMUS_LEVEL1_EFF_MU_PLUS_1B (PIN,FIIN,ETAIN)
            ELSE
              SAMUS_EFF = SAMUS_LEVEL1_EFF_MU_MINUS_1B(PIN,FIIN,ETAIN)
            END IF
          END IF
C..
          IF (TYPE(1:1) .EQ. 'T' .OR. TYPE(1:1) .EQ. 't') THEN
            IF (P .GT. 0.0)     THEN
              SAMUS_EFF = SAMUS_LEVEL15_EFF_MU_PLUS_1B (PIN,FIIN,ETAIN)
            ELSE
              SAMUS_EFF = SAMUS_LEVEL15_EFF_MU_MINUS_1B(PIN,FIIN,ETAIN)
            END IF
          END IF
C..
          IF (TYPE(1:1) .EQ. 'A' .OR. TYPE(1:1) .EQ. 'a') THEN
            IF (P .GT. 0.0)     THEN
              SAMUS_EFF = SAMUS_RECO_EFF_MU_PLUS_1B (PIN,FIIN,ETAIN)
            ELSE
              SAMUS_EFF = SAMUS_RECO_EFF_MU_MINUS_1B(PIN,FIIN,ETAIN)
            END IF
          END IF
                                                          ELSE
C..
          IF (FI .LE. PI ) FIIN = PI - FI
          IF (FI .GT. PI ) FIIN = 3*PI-FI
C
          IF (TYPE(1:1) .EQ. 'L' .OR. TYPE(1:1) .EQ. 'l') THEN
            IF (P .GT. 0.0)     THEN
              SAMUS_EFF = SAMUS_LEVEL1_EFF_MU_MINUS_1B(PIN,FIIN,ETAIN)
            ELSE
              SAMUS_EFF = SAMUS_LEVEL1_EFF_MU_PLUS_1B (PIN,FIIN,ETAIN)
            END IF
          END IF
C..
          IF (TYPE(1:1) .EQ. 'T' .OR. TYPE(1:1) .EQ. 't') THEN
            IF (P .GT. 0.0)     THEN
              SAMUS_EFF = SAMUS_LEVEL15_EFF_MU_MINUS_1B(PIN,FIIN,ETAIN)
            ELSE
              SAMUS_EFF = SAMUS_LEVEL15_EFF_MU_PLUS_1B (PIN,FIIN,ETAIN)
            END IF
          END IF
C..
          IF (TYPE(1:1) .EQ. 'A' .OR. TYPE(1:1) .EQ. 'a') THEN
            IF (P .GT. 0.0)     THEN
              SAMUS_EFF = SAMUS_RECO_EFF_MU_MINUS_1B(PIN,FIIN,ETAIN)
            ELSE
              SAMUS_EFF = SAMUS_RECO_EFF_MU_PLUS_1B (PIN,FIIN,ETAIN)
            END IF
          END IF
        END IF
							  ELSE
C..
        IF (ETA .GE. 0.0)       THEN
C..
          IF (TYPE(1:1) .EQ. 'L' .OR. TYPE(1:1) .EQ. 'l') THEN
            IF (P .GT. 0.0)     THEN
              SAMUS_EFF = SAMUS_LEVEL1_EFF_MU_PLUS_1B (PIN,FIIN,ETAIN)
            ELSE
              SAMUS_EFF = SAMUS_LEVEL1_EFF_MU_MINUS_1B(PIN,FIIN,ETAIN)
            END IF
          END IF
C..
          IF (TYPE(1:1) .EQ. 'T' .OR. TYPE(1:1) .EQ. 't') THEN
            IF (P .GT. 0.0)     THEN
              SAMUS_EFF = 
     +                   SAMUS_LEVEL15_EFF_MU_PLUS_1B_2 (PIN,FIIN,ETAIN)
            ELSE
              SAMUS_EFF = 
     +                   SAMUS_LEVEL15_EFF_MU_MINUS_1B_2(PIN,FIIN,ETAIN)
            END IF
          END IF
C..
          IF (TYPE(1:1) .EQ. 'A' .OR. TYPE(1:1) .EQ. 'a') THEN
            IF (P .GT. 0.0)     THEN
              SAMUS_EFF = SAMUS_RECO_EFF_MU_PLUS_1B_2 (PIN,FIIN,ETAIN)
            ELSE
              SAMUS_EFF = SAMUS_RECO_EFF_MU_MINUS_1B_2(PIN,FIIN,ETAIN)
            END IF
          END IF
                                                          ELSE
C..
          IF (FI .LE. PI ) FIIN = PI - FI
          IF (FI .GT. PI ) FIIN = 3*PI-FI
C
          IF (TYPE(1:1) .EQ. 'L' .OR. TYPE(1:1) .EQ. 'l') THEN
            IF (P .GT. 0.0)     THEN
              SAMUS_EFF = SAMUS_LEVEL1_EFF_MU_MINUS_1B(PIN,FIIN,ETAIN)
            ELSE
              SAMUS_EFF = SAMUS_LEVEL1_EFF_MU_PLUS_1B (PIN,FIIN,ETAIN)
            END IF
          END IF
C..
          IF (TYPE(1:1) .EQ. 'T' .OR. TYPE(1:1) .EQ. 't') THEN
            IF (P .GT. 0.0)     THEN
              SAMUS_EFF = 
     +                   SAMUS_LEVEL15_EFF_MU_MINUS_1B_2(PIN,FIIN,ETAIN)
            ELSE
              SAMUS_EFF = 
     +                   SAMUS_LEVEL15_EFF_MU_PLUS_1B_2 (PIN,FIIN,ETAIN)
            END IF
          END IF
C..
          IF (TYPE(1:1) .EQ. 'A' .OR. TYPE(1:1) .EQ. 'a') THEN
            IF (P .GT. 0.0)     THEN
              SAMUS_EFF = SAMUS_RECO_EFF_MU_MINUS_1B_2(PIN,FIIN,ETAIN)
            ELSE
              SAMUS_EFF = SAMUS_RECO_EFF_MU_PLUS_1B_2 (PIN,FIIN,ETAIN)
            END IF
          END IF
        END IF
	END IF
      END IF
C
      IF (SAMUS_EFF .LT. 0.0) SAMUS_EFF = 0.0
C
      SAMUS_EFF = SAMUS_EFF*SAMUS_MIN_BIAS_CORRECTION(P,ETA,TYPE,RUN)
C
      IF (RUN .GE. 2 .AND. RUN .LE. 3) SAMUS_EFF = 0.46*SAMUS_EFF
      IF (RUN .EQ. 4)                  SAMUS_EFF = 0.76*SAMUS_EFF
C
  999 RETURN
      END
