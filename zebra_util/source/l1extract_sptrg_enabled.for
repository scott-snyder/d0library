      SUBROUTINE L1EXTRACT_SPTRG_ENABLED( L1_BLOCK, SPTRG_ENABLED_MASK )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract the Specific Trigger Andor Fired states
C-                         from the Level 1 Crate of the TRGR Bank.
C-                         When a Specific Trigger Andor fires for a given
C-                         beam crossing, the Specific Trigger Final
C-                         Decision may still be to NOT fire because it is
C-                         currently disable by its prescaler, or an
C-                         external source, like COOR, Level 2, or Front-Ends.
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
C-           | YES :   L1EXTRACT_SPTRG_ENABLED ( IQ(LTRGR_LEVEL1),... )      |
C-           |---------------------------------------------------------------|
C-           | NO  :   L1_BLOCK = IQ( LTRGR_LEVEL1 )                         |
C-           |         L1EXTRACT_SPTRG_ENABLED ( L1_BLOCK, ... )             |
C-           +---------------------------------------------------------------+
C- 
C-   Outputs : SPTRG_ENABLED_MASK [I] Mask of Specfic Trigger Enabled status 
C-                                   for this event.
C-                                   Each bit in the mask corresponds to one of
C-                                   the 32 Specific Triggers. 
C-                                   The LSB in the mask corresponds to SpTrg #0
C-                                   The MSB corresponds to SpTrg #31.
C-                                   A Bit is set to 1 when the corresponding
C-                                   SpTrg was enabled for this event, 
C-                                   and is set to 0 as soon as the SpTrg was 
C-                                   disabled by any one of its possible 
C-                                   sources of veto (Front-End Busy, Level 2
C-                                   Supervisor, COOR, or Prescaler). 
C-                
C-   Controls: none
C-
C-   Created  14-FEB-1994   Philippe Laurens - MSU L1 Trigger   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS'
C
      INTEGER   L1_BLOCK(0:*)
      INTEGER   SPTRG_ENABLED_MASK 
C
      INTEGER   FSTD_WORD, ZEBRA_INDEX
      INTEGER   LEAST_SIGNIFICANT_BIT, MOVE_FOUR_BITS 
      PARAMETER ( LEAST_SIGNIFICANT_BIT = 0, MOVE_FOUR_BITS = 4 )
C
C     cf. D0 note 967 for location of the specific trigger FSTD data 
C         and for the formula to match note 967 item numbers to zebra word.
C     The information is in Items #361:368, bits #0:3
      ZEBRA_INDEX = TRGR_HEADER_LENGTH + (SP_TRG_FSTD+1)/2 
C
      FSTD_WORD = L1_BLOCK ( ZEBRA_INDEX + 0 )
      CALL MVBITS ( FSTD_WORD, LEAST_SIGNIFICANT_BIT,
     &              MOVE_FOUR_BITS,
     &              SPTRG_ENABLED_MASK, LEAST_SIGNIFICANT_BIT + 0 ) 
      CALL MVBITS ( FSTD_WORD, LEAST_SIGNIFICANT_BIT + 16,
     &              MOVE_FOUR_BITS,
     &              SPTRG_ENABLED_MASK, LEAST_SIGNIFICANT_BIT + 4 ) 
C      
      FSTD_WORD = L1_BLOCK ( ZEBRA_INDEX + 1 )
      CALL MVBITS ( FSTD_WORD, LEAST_SIGNIFICANT_BIT,
     &              MOVE_FOUR_BITS,
     &              SPTRG_ENABLED_MASK, LEAST_SIGNIFICANT_BIT + 8 ) 
      CALL MVBITS ( FSTD_WORD, LEAST_SIGNIFICANT_BIT + 16,
     &              MOVE_FOUR_BITS,
     &              SPTRG_ENABLED_MASK, LEAST_SIGNIFICANT_BIT + 12 ) 
C      
      FSTD_WORD = L1_BLOCK ( ZEBRA_INDEX + 2 )
      CALL MVBITS ( FSTD_WORD, LEAST_SIGNIFICANT_BIT,
     &              MOVE_FOUR_BITS,
     &              SPTRG_ENABLED_MASK, LEAST_SIGNIFICANT_BIT + 16 ) 
      CALL MVBITS ( FSTD_WORD, LEAST_SIGNIFICANT_BIT + 16,
     &              MOVE_FOUR_BITS,
     &              SPTRG_ENABLED_MASK, LEAST_SIGNIFICANT_BIT + 20 ) 
C      
      FSTD_WORD = L1_BLOCK ( ZEBRA_INDEX + 3 )
      CALL MVBITS ( FSTD_WORD, LEAST_SIGNIFICANT_BIT,
     &              MOVE_FOUR_BITS,
     &              SPTRG_ENABLED_MASK, LEAST_SIGNIFICANT_BIT + 24 ) 
      CALL MVBITS ( FSTD_WORD, LEAST_SIGNIFICANT_BIT + 16,
     &              MOVE_FOUR_BITS,
     &              SPTRG_ENABLED_MASK, LEAST_SIGNIFICANT_BIT + 28 ) 
C      
  999 RETURN
      END
