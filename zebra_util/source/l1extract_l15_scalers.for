      SUBROUTINE L1EXTRACT_L15_SCALERS(L1_BLOCK,
     &                                 L15_CYCLE_SCALER,
     &                                 L15_POTENTIAL_SCALER,
     &                                 L15_SKIP_SCALER,
     &                                 AFTER_L15_DEAD_BEAMX_SCALER,
     &                                 AFTER_L15_PASS_SCALER,
     &                                 AFTER_L15_FAIL_SCALER,
     &                                 AFTER_L15_TIMEOUT_SCALER,
     &                                 BEFORE_L15_DEAD_BEAMX_SCALER,
     &                                 BEFORE_L15_PASS_SCALER,
     &                                 BEFORE_L15_FAIL_SCALER,
     &                                 BEFORE_L15_TIMEOUT_SCALER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract the Level 1.5 Scalers from the Level 1
C-      Crate in the TRGR bank. Each scaler is 40 bits in size and is returned
C-      in the following format:
C-
C-      INTEGER SCALER(2)
C-
C-              SCALER(1) holds the low  24 bits
C-              SCALER(2) holds the high 16 bits
C-              (unused bits are padded with zero)
C-
C-   Inputs  : L1_BLOCK [I]
C-
C-             First word of the Level-1 Crate header in the TRGR bank.
C-          -> Use IQ( LTRGR_LEVEL1 )
C-             Where LTRGR_LEVEL1 = GZFIND_CRATE ( 'TRGR', GZTRGR(), 11 )
C-             cf. header of D0$ZEBRA_UTIL$SOURCE:GZFIND_CRATE.FOR for details
C-
C-           +------------------------------------------------------------+
C-           | You must pass the variable IQ(LTRGR_LEVEL1)   DIRECTLY     |
C-           | and NOT A COPY of it.                                      |
C-           |------------------------------------------------------------|
C-           | YES :   L1EXTRACT_L15_SCALERS ( IQ(LTRGR_LEVEL1),... )     |
C-           |------------------------------------------------------------|
C-           | NO  :   L1_BLOCK = IQ( LTRGR_LEVEL1 )                      |
C-           |         L1EXTRACT_L15_SCALERS ( L1_BLOCK, ... )            |
C-           +------------------------------------------------------------+
C- 
C-   Outputs : L15_CYCLE_SCALER         The Level 1.5 Cycle Scaler
C-             L15_POTENTIAL_SCALER     The Potential Level 1.5 Event Scaler
C-             L15_SKIP_SCALER          The Level 1.5 Skip Scaler
C-
C-              The following scalers are taken after the Level 1.5 cycle has
C-                completed for the current event:
C-
C-             AFTER_L15_DEAD_BEAMX_SCALER  The number of Beam crossings lost
C-                                            due to Level 1.5
C-             AFTER_L15_PASS_SCALER        The number of events Passed by L1.5
C-             AFTER_L15_FAIL_SCALER        The number of events Rejected by
C-                                            L1.5
C-             AFTER_L15_TIMEOUT_SCALER     The number of events Level 1.5
C-                                            exited due to a timeout.
C-
C-              The following analogous scalers are taken before the 
C-                Level 1.5 cycle begins: 
C-
C-             BEFORE_L15_DEAD_BEAMX_SCALER
C-             BEFORE_L15_PASS_SCALER
C-             BEFORE_L15_FAIL_SCALER
C-             BEFORE_L15_TIMEOUT_SCALER
C-
C-              By comparing the BEFORE_* and AFTER_* scalers, the result of
C-              the Level 1.5 cycle can be determined, and also the number of
C-              Beam Crossings lost due to the Level 1.5 cycle.
C-
C-   Controls: none
C-
C-   Created   4-JUN-1992   Philippe Laurens, Steven Klocek
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS'
C
      INTEGER L1_BLOCK(0:*)
      INTEGER L15_CYCLE_SCALER(2)
      INTEGER L15_POTENTIAL_SCALER(2)
      INTEGER L15_SKIP_SCALER(2)
      INTEGER AFTER_L15_DEAD_BEAMX_SCALER(2)
      INTEGER AFTER_L15_PASS_SCALER(2)
      INTEGER AFTER_L15_FAIL_SCALER(2)
      INTEGER AFTER_L15_TIMEOUT_SCALER(2)
      INTEGER BEFORE_L15_DEAD_BEAMX_SCALER(2)
      INTEGER BEFORE_L15_PASS_SCALER(2)
      INTEGER BEFORE_L15_FAIL_SCALER(2)
      INTEGER BEFORE_L15_TIMEOUT_SCALER(2)
C
      CALL PRTRGR_FIRST_BYTE_DECODING(3,
     &                                LEVEL15_CYCLE_SCALER,
     &                                L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                                L15_CYCLE_SCALER(1))
C
      CALL PRTRGR_FIRST_BYTE_DECODING(2,
     &                                LEVEL15_CYCLE_SCALER + 3,
     &                                L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                                L15_CYCLE_SCALER(2))
C
C
      CALL PRTRGR_FIRST_BYTE_DECODING(3,
     &                                LEVEL15_POTENTIAL_SCALER,
     &                                L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                                L15_POTENTIAL_SCALER(1))
C
      CALL PRTRGR_FIRST_BYTE_DECODING(2,
     &                                LEVEL15_POTENTIAL_SCALER + 3,
     &                                L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                                L15_POTENTIAL_SCALER(2))
C
C
      CALL PRTRGR_FIRST_BYTE_DECODING(3,
     &                                LEVEL15_SKIP_SCALER,
     &                                L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                                L15_SKIP_SCALER(1))
C
      CALL PRTRGR_FIRST_BYTE_DECODING(2,
     &                                LEVEL15_SKIP_SCALER + 3,
     &                                L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                                L15_SKIP_SCALER(2))
C
C
      CALL PRTRGR_FIRST_BYTE_DECODING(3,
     &                                LEVEL15_DEAD_BEAMX_SCALER,
     &                                L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                                AFTER_L15_DEAD_BEAMX_SCALER(1))
C
      CALL PRTRGR_FIRST_BYTE_DECODING(2,
     &                                LEVEL15_DEAD_BEAMX_SCALER+3,
     &                                L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                                AFTER_L15_DEAD_BEAMX_SCALER(2))
C
      CALL PRTRGR_FIRST_BYTE_DECODING(3,
     &                        LEVEL15_DEAD_BEAMX_SCALER+4*SCALER_L,
     &                        L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                        BEFORE_L15_DEAD_BEAMX_SCALER(1))
C
      CALL PRTRGR_FIRST_BYTE_DECODING(2,
     &                        LEVEL15_DEAD_BEAMX_SCALER+4*SCALER_L+3,
     &                        L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                        BEFORE_L15_DEAD_BEAMX_SCALER(2))
C
C
      CALL PRTRGR_FIRST_BYTE_DECODING(3,
     &                        LEVEL15_PASS_SCALER,
     &                        L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                        AFTER_L15_PASS_SCALER(1))
C
      CALL PRTRGR_FIRST_BYTE_DECODING(2,
     &                        LEVEL15_PASS_SCALER+3,
     &                        L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                        AFTER_L15_PASS_SCALER(2))
C
      CALL PRTRGR_FIRST_BYTE_DECODING(3,
     &                        LEVEL15_PASS_SCALER+4*SCALER_L,
     &                        L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                        BEFORE_L15_PASS_SCALER(1))
C
      CALL PRTRGR_FIRST_BYTE_DECODING(2,
     &                        LEVEL15_PASS_SCALER+4*SCALER_L+3,
     &                        L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                        BEFORE_L15_PASS_SCALER(2))
C
C
      CALL PRTRGR_FIRST_BYTE_DECODING(3,
     &                        LEVEL15_FAIL_SCALER,
     &                        L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                        AFTER_L15_FAIL_SCALER(1))
C
      CALL PRTRGR_FIRST_BYTE_DECODING(2,
     &                        LEVEL15_FAIL_SCALER+3,
     &                        L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                        AFTER_L15_FAIL_SCALER(2))
C
      CALL PRTRGR_FIRST_BYTE_DECODING(3,
     &                        LEVEL15_FAIL_SCALER+4*SCALER_L,
     &                        L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                        BEFORE_L15_FAIL_SCALER(1))
C
      CALL PRTRGR_FIRST_BYTE_DECODING(2,
     &                        LEVEL15_FAIL_SCALER+4*SCALER_L+3,
     &                        L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                        BEFORE_L15_FAIL_SCALER(2))
C
C
      CALL PRTRGR_FIRST_BYTE_DECODING(3,
     &                        LEVEL15_TIME_OUT_SCALER,
     &                        L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                        AFTER_L15_TIMEOUT_SCALER(1))
C
      CALL PRTRGR_FIRST_BYTE_DECODING(2,
     &                        LEVEL15_TIME_OUT_SCALER+3,
     &                        L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                        AFTER_L15_TIMEOUT_SCALER(2))
C
      CALL PRTRGR_FIRST_BYTE_DECODING(3,
     &                        LEVEL15_TIME_OUT_SCALER+4*SCALER_L,
     &                        L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                        BEFORE_L15_TIMEOUT_SCALER(1))
C
      CALL PRTRGR_FIRST_BYTE_DECODING(2,
     &                        LEVEL15_TIME_OUT_SCALER+4*SCALER_L+3,
     &                        L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                        BEFORE_L15_TIMEOUT_SCALER(2))
C
C----------------------------------------------------------------------
  999 RETURN
      END
