      SUBROUTINE L1EXTRACT_TRANSV_ENERGIES( L1_BLOCK,
     &                                      EM_ET, HD_ET,
     &                                      TOT_ET, MIS_PT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract from the Level 1 crate data in the TRGR bank
C-                      the global transverse energy quantities (EM Et, HD Et,
C-                      TOT Et, and Missing Pt). See also D0 Note 967.
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
C-           | YES :   L1EXTRACT_TRANSV_ENERGIES ( IQ(LTRGR_LEVEL1),... ) |
C-           |------------------------------------------------------------|
C-           | NO  :   L1_BLOCK = IQ( LTRGR_LEVEL1 )                      |
C-           |         L1EXTRACT_TRANSV_ENERGIES ( L1_BLOCK, ... )        |
C-           +------------------------------------------------------------+
C- 
C-   Outputs : EM_ET  [R]
C-             HD_ET  [R]
C-             TOT_ET [R]
C-             MIS_PT [R]
C-                      
C-              The extracted global energies. They are given in units of
C-              GeV. 
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
      REAL EM_ET
      REAL HD_ET
      REAL TOT_ET
      REAL MIS_PT
C
C       Hard-code the scale of the quantities until the scale changes.
C
      REAL HARDCODED_SCALE_ET
      PARAMETER (HARDCODED_SCALE_ET = 0.25)
      REAL HARDCODED_SCALE_PT
      PARAMETER (HARDCODED_SCALE_PT = 0.5)
C
      INTEGER COUNTS
C
C       Extract EM Et
C
      CALL PRTRGR_FIRST_BYTE_DECODING(GLOBAL_L, 
     &                                EM_ET_TOTAL, 
     &                                L1_BLOCK(TRGR_HEADER_LENGTH+1), 
     &                                COUNTS)
      CALL PRTRGR_SIGN_EXTEND_PT(COUNTS)
      EM_ET = COUNTS * HARDCODED_SCALE_ET
C
C       Extract HD Et
C
      CALL PRTRGR_FIRST_BYTE_DECODING(GLOBAL_L,
     &                                HD_ET_TOTAL,
     &                                L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                                COUNTS)
      CALL PRTRGR_SIGN_EXTEND_PT(COUNTS)
      HD_ET = COUNTS * HARDCODED_SCALE_ET
C
C       Extract Total Et
C
      CALL PRTRGR_FIRST_BYTE_DECODING(GLOBAL_L,
     &                                TOT_ET_TOTAL, 
     &                                L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                                COUNTS)
      CALL PRTRGR_SIGN_EXTEND_PT(COUNTS)
      TOT_ET = COUNTS * HARDCODED_SCALE_ET
C
C       Extract Missing Pt
C      
      CALL PRTRGR_FIRST_BYTE_DECODING(1,
     &                                MPT_TOTAL, 
     &                                L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                                COUNTS)
      MIS_PT = COUNTS * HARDCODED_SCALE_PT
C
C----------------------------------------------------------------------
  999 RETURN
      END
