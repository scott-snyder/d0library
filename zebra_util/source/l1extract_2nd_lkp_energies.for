      SUBROUTINE L1EXTRACT_2ND_LKP_ENERGIES(L1_BLOCK, 
     &                                      EM_L2, HD_L2, TOT_L2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract the 2nd Lookup sums from the Level 1
C-     Crate of the TRGR bank. See also D0 Note 967.
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
C-           | YES :   L1EXTRACT_2ND_LKP_ENERGIES ( IQ(LTRGR_LEVEL1),... )|
C-           |------------------------------------------------------------|
C-           | NO  :   L1_BLOCK = IQ( LTRGR_LEVEL1 )                      |
C-           |         L1EXTRACT_2ND_LKP_ENERGIES ( L1_BLOCK, ... )       |
C-           +------------------------------------------------------------+
C- 
C-   Outputs : EM_L2    [R]
C-             HD_L2    [R]
C-             TOT_L2   [R]
C-             
C-              The extracted Second Lookup quantities, in GeV.
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
C
      REAL EM_L2, HD_L2, TOT_L2
C
C       Hard-code the scale of the quantities until the scale changes.
C
      REAL HARDCODED_SCALE_ET
      PARAMETER (HARDCODED_SCALE_ET = 0.25)
C
      INTEGER COUNTS
C
C       Extract EM 2nd Lookup
C
      CALL PRTRGR_FIRST_BYTE_DECODING(GLOBAL_L,
     &                                EM_L2_TOTAL,
     &                                L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                                COUNTS)
      EM_L2 = COUNTS * HARDCODED_SCALE_ET
C
C       Extract HD 2nd Lookup
C
      CALL PRTRGR_FIRST_BYTE_DECODING(GLOBAL_L,
     &                                HD_L2_TOTAL,
     &                                L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                                COUNTS)
      HD_L2 = COUNTS * HARDCODED_SCALE_ET
C
C       Extract TOT 2nd Lookup
C
      CALL PRTRGR_FIRST_BYTE_DECODING(GLOBAL_L,
     &                                TOT_L2_TOTAL,
     &                                L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                                COUNTS)
      TOT_L2 = COUNTS * HARDCODED_SCALE_ET
C
C----------------------------------------------------------------------
  999 RETURN
      END
