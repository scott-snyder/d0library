      SUBROUTINE L1EXTRACT_GLOBAL_TOWER_COUNTS(L1_BLOCK,
     &  EM_TOWER_COUNTS, TOT_TOWER_COUNTS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract the Global Tower Counts from the Level 1
C-      Crate in the TRGR bank.
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
C-           | YES :   L1EXTRACT_GLOBAL_TOWER_COUNTS ( IQ(LTRGR_LEVEL1),... )|
C-           |---------------------------------------------------------------|
C-           | NO  :   L1_BLOCK = IQ( LTRGR_LEVEL1 )                         |
C-           |         L1EXTRACT_GLOBAL_TOWER_COUNTS ( L1_BLOCK, ... )       |
C-           +---------------------------------------------------------------+
C- 
C-   Outputs : EM_TOWER_COUNTS(4) [I]
C-
C-              The count of towers clearing each EM Et reference set.
C-              
C-             TOT_TOWER_COUNTS(4) [I]
C-             
C-              The count of towers clearing each TOT Et reference set.
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
      INTEGER EM_TOWER_COUNTS(4)
      INTEGER TOT_TOWER_COUNTS(4)
C
      INTEGER REF_SET
C
      DO REF_SET = 1, 4
        CALL PRTRGR_FIRST_BYTE_DECODING(2,
     &                                  HOT_TOWER_FINAL+(REF_SET-1)*2,
     &                                  L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                                  EM_TOWER_COUNTS(REF_SET))
C
        CALL PRTRGR_FIRST_BYTE_DECODING(2,
     &                                  HOT_TOWER_FINAL+8+(REF_SET-1)*2,
     &                                  L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                                  TOT_TOWER_COUNTS(REF_SET))
      END DO
C
C----------------------------------------------------------------------
  999 RETURN
      END
