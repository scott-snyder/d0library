      SUBROUTINE L1EXTRACT_SPTRG_EXPOS_SCALERS(L1_BLOCK, EXPOS_SCALERS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract the Trigger Exposition scalers from the 
C-      Level 1 Crate in the TRGR bank.
C-
C-   Inputs  : L1_BLOCK [I]
C-
C-             First word of the Level-1 Crate header in the TRGR bank.
C-          -> Use IQ( LTRGR_LEVEL1 )
C-             Where LTRGR_LEVEL1 = GZFIND_CRATE ( 'TRGR', GZTRGR(), 11 )
C-             cf. header of D0$ZEBRA_UTIL$SOURCE:GZFIND_CRATE.FOR for details
C-
C-           +---------------------------------------------------------------+
C-           | You must pass the variable IQ(LTRGR_LEVEL1)   DIRECTLY        |
C-           | and NOT A COPY of it.                                         |
C-           |---------------------------------------------------------------|
C-           | YES :   L1EXTRACT_SPTRG_EXPOS_SCALERS ( IQ(LTRGR_LEVEL1),... )|
C-           |---------------------------------------------------------------|
C-           | NO  :   L1_BLOCK = IQ( LTRGR_LEVEL1 )                         |
C-           |         L1EXTRACT_SPTRG_EXPOS_SCALERS ( L1_BLOCK, ... )       |
C-           +---------------------------------------------------------------+
C- 
C-   Outputs : EXPOS_SCALERS(2, 0:31) [I]
C-
C-                The Trigger Enable scaler for each Specific Trigger. 
C-                The 40-bit scalers are stored as follows:
C-                EXPOS_SCALERS(1, ST) holds the low  24 bits
C-                EXPOS_SCALERS(2, ST) holds the high 16 bits
C-                (unused bits are padded with zero)
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
      INTEGER EXPOS_SCALERS(2, 0:31)
C
      INTEGER TRG_NUM
C
      DO TRG_NUM = TRG_NUM_MIN, TRG_NUM_MAX
C        
        CALL PRTRGR_FIRST_BYTE_DECODING(3,
     &                                  SP_TRG_SCALERS + SCALER_L
     &                                    + 2 * SCALER_L * TRG_NUM,
     &                                  L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                                  EXPOS_SCALERS(1, TRG_NUM) )
C
        CALL PRTRGR_FIRST_BYTE_DECODING(2,
     &                                  SP_TRG_SCALERS + SCALER_L
     &                                    + 2 * SCALER_L * TRG_NUM
     &                                    + 3,
     &                                  L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                                  EXPOS_SCALERS(2, TRG_NUM) )
C 
      END DO
C----------------------------------------------------------------------
  999 RETURN
      END
