      SUBROUTINE L1EXTRACT_ACNET_TIME(L1_BLOCK, ACNET_DATE, ACNET_TIME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract the ACnet format absolute time from the
C-      Level 1 Crate of the TRGR Bank. 
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
C-           | YES :   L1EXTRACT_ACNET_TIME ( IQ(LTRGR_LEVEL1),... )      |
C-           |------------------------------------------------------------|
C-           | NO  :   L1_BLOCK = IQ( LTRGR_LEVEL1 )                      |
C-           |         L1EXTRACT_ACNET_TIME ( L1_BLOCK, ... )             |
C-           +------------------------------------------------------------+
C- 
C-   Outputs : ACNET_DATE  [I]
C-             ACNET_TIME  [I]
C-                
C-                The ACnet format time.
C-
C-   Controls: none
C-
C-   Created  11-JUN-1992   Philippe Laurens, Steven Klocek
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
      INTEGER ACNET_DATE, ACNET_TIME
C
      ACNET_DATE = L1_BLOCK(TRGR_HEADER_LENGTH+1+(ABSOLUTE_TIME-1)/2)
      ACNET_TIME = L1_BLOCK(TRGR_HEADER_LENGTH+1+(ABSOLUTE_TIME-1)/2+1)
C
C----------------------------------------------------------------------
  999 RETURN
      END
