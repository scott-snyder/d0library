      REAL FUNCTION SAMUS_MIN_BIAS_CORRECTION(P,ETA,TYPE,RUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate SAMUS min_bias background correction
C-
C-   Returned value  :
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
C-
C-   Outputs : 
C-   Controls: 
C-
C-   Created  10-JUN-1995   O.Eroshin
C-
C----------------------------------------------------------------------
      IMPLICIT    NONE
C----------------------------------------------------------------------
      INTEGER     RUN
      CHARACTER*4 TYPE
      LOGICAL     FIRST
      REAL        P, PI, ETA, PIN, ETAIN
      REAL        SAMUS_BIAS_L15_MU_PLUS_1B,
     *            SAMUS_BIAS_L15_MU_MINUS_1B,
     *            SAMUS_BIAS_RC_MU_PLUS_1B,
     *            SAMUS_BIAS_RC_MU_MINUS_1B
      REAL        SAMUS_BIAS_L15_MU_PLUS_1C,
     *            SAMUS_BIAS_L15_MU_MINUS_1C,
     *            SAMUS_BIAS_RC_MU_PLUS_1C,
     *            SAMUS_BIAS_RC_MU_MINUS_1C
C----------------------------------------------------------------------
      DATA        FIRST / .TRUE. /
C
C..........
C
      IF( FIRST )             THEN
        PI = 4. * ATAN( 1. )
        FIRST = .FALSE.
      ENDIF
C
      SAMUS_MIN_BIAS_CORRECTION = 1.0
C
      PIN             = ABS(P)
      ETAIN           = ABS(ETA)
C
      IF ( PIN .LE. 6.0 ) RETURN
      IF ( PIN .GT. 200.) PIN = 200.
C
      IF (RUN .EQ. 1) RETURN
C
      IF (RUN .GT. 1 .AND. RUN .LT. 4)                      THEN
        IF (ETA .GE. 0.0)                                   THEN
C..
          IF (TYPE(1:1) .EQ. 'T' .OR. TYPE(1:1) .EQ. 't') THEN
            IF (P .GT. 0.0)     THEN
              SAMUS_MIN_BIAS_CORRECTION = 
     *                      SAMUS_BIAS_L15_MU_PLUS_1B (PIN,ETAIN)
            ELSE
              SAMUS_MIN_BIAS_CORRECTION = 
     *                      SAMUS_BIAS_L15_MU_MINUS_1B(PIN,ETAIN)
            END IF
          END IF
C..
          IF (TYPE(1:1) .EQ. 'A' .OR. TYPE(1:1) .EQ. 'a')   THEN
            IF (P .GT. 0.0)     THEN
              SAMUS_MIN_BIAS_CORRECTION = 
     *                      SAMUS_BIAS_RC_MU_PLUS_1B (PIN,ETAIN)
            ELSE
              SAMUS_MIN_BIAS_CORRECTION = 
     *                      SAMUS_BIAS_RC_MU_MINUS_1B(PIN,ETAIN)
            END IF
          END IF
        ELSE
C..
          IF (TYPE(1:1) .EQ. 'T' .OR. TYPE(1:1) .EQ. 't') THEN
            IF (P .GT. 0.0)     THEN
              SAMUS_MIN_BIAS_CORRECTION = 
     *                      SAMUS_BIAS_L15_MU_MINUS_1B(PIN,ETAIN)
            ELSE
              SAMUS_MIN_BIAS_CORRECTION = 
     *                      SAMUS_BIAS_L15_MU_PLUS_1B (PIN,ETAIN)
            END IF
          END IF
C..
          IF (TYPE(1:1) .EQ. 'A' .OR. TYPE(1:1) .EQ. 'a') THEN
            IF (P .GT. 0.0)     THEN
              SAMUS_MIN_BIAS_CORRECTION = 
     *                      SAMUS_BIAS_RC_MU_MINUS_1B(PIN,ETAIN)
            ELSE
              SAMUS_MIN_BIAS_CORRECTION = 
     *                      SAMUS_BIAS_RC_MU_PLUS_1B (PIN,ETAIN)
            END IF
          END IF
        END IF
                                                            ELSE
        IF (ETA .GE. 0.0)                                   THEN
C..
          IF (TYPE(1:1) .EQ. 'T' .OR. TYPE(1:1) .EQ. 't') THEN
            IF (P .GT. 0.0)     THEN
              SAMUS_MIN_BIAS_CORRECTION = 
     *                      SAMUS_BIAS_L15_MU_PLUS_1C (PIN,ETAIN)
            ELSE
              SAMUS_MIN_BIAS_CORRECTION = 
     *                      SAMUS_BIAS_L15_MU_MINUS_1C(PIN,ETAIN)
            END IF
          END IF
C..
          IF (TYPE(1:1) .EQ. 'A' .OR. TYPE(1:1) .EQ. 'a')   THEN
            IF (P .GT. 0.0)     THEN
              SAMUS_MIN_BIAS_CORRECTION = 
     *                      SAMUS_BIAS_RC_MU_PLUS_1C (PIN,ETAIN)
            ELSE
              SAMUS_MIN_BIAS_CORRECTION = 
     *                      SAMUS_BIAS_RC_MU_MINUS_1C(PIN,ETAIN)
            END IF
          END IF
        ELSE
C..
          IF (TYPE(1:1) .EQ. 'T' .OR. TYPE(1:1) .EQ. 't') THEN
            IF (P .GT. 0.0)     THEN
              SAMUS_MIN_BIAS_CORRECTION = 
     *                      SAMUS_BIAS_L15_MU_MINUS_1C(PIN,ETAIN)
            ELSE
              SAMUS_MIN_BIAS_CORRECTION = 
     *                      SAMUS_BIAS_L15_MU_PLUS_1C (PIN,ETAIN)
            END IF
          END IF
C..
          IF (TYPE(1:1) .EQ. 'A' .OR. TYPE(1:1) .EQ. 'a') THEN
            IF (P .GT. 0.0)     THEN
              SAMUS_MIN_BIAS_CORRECTION = 
     *                      SAMUS_BIAS_RC_MU_MINUS_1C(PIN,ETAIN)
            ELSE
              SAMUS_MIN_BIAS_CORRECTION = 
     *                      SAMUS_BIAS_RC_MU_PLUS_1C (PIN,ETAIN)
            END IF
          END IF
        END IF
      END IF
C
  999 RETURN
      END
