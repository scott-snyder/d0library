      SUBROUTINE L1EXTRACT_L0_FAST_Z_SCALERS(L1_BLOCK, L0GOOD_SCALER,
     &                                       L0_PER_BUNCH_SCALERS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract the Level 0 Fast Vertex scalers from the 
C-      Level 1 Crate in the TRGR bank.
C-
C-   Inputs  : L1_BLOCK [I]
C-
C-             First word of the Level-1 Crate header in the TRGR bank.
C-          -> Use IQ( LTRGR_LEVEL1 )
C-             Where LTRGR_LEVEL1 = GZFIND_CRATE ( 'TRGR', GZTRGR(), 11 )
C-             cf. header of D0$ZEBRA_UTIL$SOURCE:GZFIND_CRATE.FOR for details
C-
C-           +-------------------------------------------------------------+
C-           | You must pass the variable IQ(LTRGR_LEVEL1)   DIRECTLY      |
C-           | and NOT A COPY of it.                                       |
C-           |-------------------------------------------------------------|
C-           | YES :   L1EXTRACT_L0_FAST_Z_SCALERS ( IQ(LTRGR_LEVEL1),... )|
C-           |-------------------------------------------------------------|
C-           | NO  :   L1_BLOCK = IQ( LTRGR_LEVEL1 )                       |
C-           |         L1EXTRACT_L0_FAST_Z_SCALERS ( L1_BLOCK, ... )       |
C-           +-------------------------------------------------------------+
C- 
C-   Outputs : L0GOOD_SCALER(2) [I]
C-
C-              Array holding the 40-bit Level 0 Good scaler. 
C-              L0GOOD_SCALER(1) holds the low  24 bits
C-              L0GOOD_SCALER(2) holds the high 16 bits
C-              (unused bits are padded with zero)
C-
C-             L0_PER_BUNCH_SCALERS(2, 6) [I]
C-
C-              Array holding the 40-bit Level 0 Good Per Bunch scalers.
C-              L0_PER_BUNCH_SCALER(1,i) holds the low  24 bits for Bunch #i
C-              L0_PER_BUNCH_SCALER(2,i) holds the high 16 bits for Bunch #i
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
      INTEGER L0GOOD_SCALER(2)
      INTEGER L0_PER_BUNCH_SCALERS(2, 6)
C
      INTEGER BUNCH
C
      CALL PRTRGR_FIRST_BYTE_DECODING(3,
     &                                FASTZ_GOOD_SCALER, 
     &                                L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                                L0GOOD_SCALER(1))
C
      CALL PRTRGR_FIRST_BYTE_DECODING(2,
     &                                FASTZ_GOOD_SCALER+3,
     &                                L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                                L0GOOD_SCALER(2))
C
      DO BUNCH = 1, 6
        CALL PRTRGR_FIRST_BYTE_DECODING(3,
     &                           BUNCH_LEVEL0_GOOD+(BUNCH-1)*SCALER_L,
     &                           L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                           L0_PER_BUNCH_SCALERS(1, BUNCH) )
C
        CALL PRTRGR_FIRST_BYTE_DECODING(2,
     &                           BUNCH_LEVEL0_GOOD+(BUNCH-1)*SCALER_L+3,
     &                           L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                           L0_PER_BUNCH_SCALERS(2, BUNCH) )
      END DO
C
C----------------------------------------------------------------------
  999 RETURN
      END
