      SUBROUTINE L1EXTRACT_BEAMX_SCALER(L1_BLOCK, RAW_BEAMX_SCALER,
     &                                  GATED_BEAMX_SCALER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract the Beam Crossing Scaler from the Level 1
C-      Crate in the TRGR bank.
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
C-           | YES :   L1EXTRACT_BEAMX_SCALER ( IQ(LTRGR_LEVEL1),... )    |
C-           |------------------------------------------------------------|
C-           | NO  :   L1_BLOCK = IQ( LTRGR_LEVEL1 )                      |
C-           |         L1EXTRACT_BEAMX_SCALER ( L1_BLOCK, ... )           |
C-           +------------------------------------------------------------+
C- 
C-   Outputs : RAW_BEAMX_SCALER(2) [I]
C-
C-              Array holding the 40-bit Beam Crossing scaler. 
C-              RAW_BEAMX_SCALER(1) holds the low  24 bits
C-              RAW_BEAMX_SCALER(2) holds the high 16 bits
C-              (unused bits are padded with zero)
C-
C-             GATED_BEAMX_SCALER(2) [I]
C-
C-              Array holding the 40-bit Gated Beam Crossing scaler.
C-              RAW_BEAMX_SCALER(1) holds the low  24 bits
C-              RAW_BEAMX_SCALER(2) holds the high 16 bits
C-              (unused bits are padded with zero)
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
      INTEGER RAW_BEAMX_SCALER(2)
      INTEGER GATED_BEAMX_SCALER(2)
C
      CALL PRTRGR_FIRST_BYTE_DECODING(3,
     &                                BEAM_CROSS_SCALER,
     &                                L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                                RAW_BEAMX_SCALER(1))
C
      CALL PRTRGR_FIRST_BYTE_DECODING(2,
     &                                BEAM_CROSS_SCALER+3,
     &                                L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                                RAW_BEAMX_SCALER(2))
C
      CALL PRTRGR_FIRST_BYTE_DECODING(3,
     &                                GATED_BEAM_CROSS_SCALER,
     &                                L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                                GATED_BEAMX_SCALER(1))
C
      CALL PRTRGR_FIRST_BYTE_DECODING(2,
     &                                GATED_BEAM_CROSS_SCALER+3,
     &                                L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                                GATED_BEAMX_SCALER(2))
C
C----------------------------------------------------------------------
  999 RETURN
      END
