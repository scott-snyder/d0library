      SUBROUTINE L1EXTRACT_MOMENTUM ( L1_BLOCK,
     &                                PX, PY, MIS_PT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract from the Level 1 crate data in the TRGR bank
C-                      the momentum quantities (Px, Py, and Missing Pt).
C-                      See also D0 Note 967.
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
C-           | YES :   L1EXTRACT_MOMENTUM ( IQ(LTRGR_LEVEL1),... )        |
C-           |------------------------------------------------------------|
C-           | NO  :   L1_BLOCK = IQ( LTRGR_LEVEL1 )                      |
C-           |         L1EXTRACT_MOMENTUM ( L1_BLOCK, ... )               |
C-           +------------------------------------------------------------+
C- 
C-   Outputs : PX     [R]
C-             PY     [R]
C-             MIS_PT [R]
C-                      
C-              The extracted momentum quantities. They are given in units of
C-              GeV. 
C-                
C-   Controls: none
C-
C-   Created  13-JUL-1992 Philippe Laurens, Steven Klocek
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
      REAL PX
      REAL PY
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
C       Extract Px
C
      CALL PRTRGR_FIRST_BYTE_DECODING(GLOBAL_L,
     &                                PX_TOTAL,
     &                                L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                                COUNTS)
      CALL PRTRGR_SIGN_EXTEND_PT(COUNTS)
      PX = COUNTS * HARDCODED_SCALE_PT
C
C       Extract Py
C
      CALL PRTRGR_FIRST_BYTE_DECODING(GLOBAL_L,
     &                                PY_TOTAL,
     &                                L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                                COUNTS)
      CALL PRTRGR_SIGN_EXTEND_PT(COUNTS)
      PY = COUNTS * HARDCODED_SCALE_PT
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
