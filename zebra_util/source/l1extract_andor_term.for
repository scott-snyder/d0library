      SUBROUTINE L1EXTRACT_ANDOR_TERM(L1_BLOCK, 
     &                                ANDOR_TERM_INDEX, 
     &                                STATE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract the state of an Andor Term from the Level 1
C-      Crate of the TRGR Bank.
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
C-           | YES :   L1EXTRACT_ANDOR_TERM ( IQ(LTRGR_LEVEL1),... )      |
C-           |------------------------------------------------------------|
C-           | NO  :   L1_BLOCK = IQ( LTRGR_LEVEL1 )                      |
C-           |         L1EXTRACT_ANDOR_TERM ( L1_BLOCK, ... )             |
C-           +------------------------------------------------------------+
C- 
C-             ANDOR_TERM_INDEX [I]
C-             
C-                The hardware index of the desired Andor Term. The following
C-                is a short example of how to retrieve a hardware index given
C-                the logical name used by COOR:
C-
C-              CALL INRCP('D0$LEVEL1$DATA:TRIGGER_RESOURCES.RCP', IER)
C-              CALL L1UTIL_PICK_RESOURCE_RCP
C-              CALL EZGET('LOGICAL_NAME', ANDOR_TERM_INDEX, IER)
C-              CALL EZRSET
C-
C-                For a more detailed example with error checking, etc., see
C-                the source of L1_SPECIAL_TERMS_SIM.FOR.
C-
C-   Outputs : STATE [L]
C-      
C-                LOGICAL value of the state of the Andor Term.
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
      INTEGER ANDOR_TERM_INDEX
C
      LOGICAL STATE
C       
      INTEGER BIT_OFFSET(0:15)
      DATA BIT_OFFSET / 0,  1,  2,  3,  4,  5,  6,  7, 
     &                 16, 17, 18, 19, 20, 21, 22, 23 /
C
      STATE = BTEST(L1_BLOCK(   TRGR_HEADER_LENGTH + 1 
     &                        + ANDOR_01_TO_16/2 
     &                        + ANDOR_TERM_INDEX / 16),
     &              BIT_OFFSET(MOD(ANDOR_TERM_INDEX, 16) ) )
C
C----------------------------------------------------------------------
  999 RETURN
      END
