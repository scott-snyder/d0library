      SUBROUTINE L1EXTRACT_L0_FAST_Z_DATA(L1_BLOCK, 
     &                                    Z_BIN, GOOD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract the Level 0 Fast Vertex data from the 
C-      Level 1 Crate in the TRGR bank.
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
C-           | YES :   L1EXTRACT_L0_FAST_Z_DATA ( IQ(LTRGR_LEVEL1),... )  |
C-           |------------------------------------------------------------|
C-           | NO  :   L1_BLOCK = IQ( LTRGR_LEVEL1 )                      |
C-           |         L1EXTRACT_L0_FAST_Z_DATA ( L1_BLOCK, ... )         |
C-           +------------------------------------------------------------+
C- 
C-   Outputs : Z_BIN [I]
C-
C-              The Level 0 Bin containing the interaction vertex. 
C-
C-             GOOD [L]
C-
C-              The Good bit from Level 0 indicating whether it could report
C-              the vertex position.
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
      INTEGER Z_BIN
      LOGICAL GOOD
C
      INTEGER L0_GOOD_BIT
      PARAMETER (L0_GOOD_BIT = 5)
      INTEGER L0_SIGN_BIT
      PARAMETER (L0_SIGN_BIT = 4)
C
      INTEGER Z_DATA
C
      CALL PRTRGR_FIRST_BYTE_DECODING(1,
     &                                FAST_VERTEX,
     &                                L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                                Z_DATA)
C
      GOOD = BTEST(Z_DATA, L0_GOOD_BIT)
C
      Z_BIN = 0
      IF (BTEST(Z_DATA, L0_SIGN_BIT) .EQV. .TRUE.) Z_BIN = -1
      CALL MVBITS(Z_DATA, 0, L0_SIGN_BIT, Z_BIN, 0)
C        
C----------------------------------------------------------------------
  999 RETURN
      END
