      SUBROUTINE L1_FW_AND_CT_FILL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Books and fills the TRGR bank.
C-
C-   Inputs  : LEVEL1_TRIGGER_DATA_BLOCK common.
C-
C-   Outputs : TRGR Zebra bank.
C-
C-   Controls: None.
C-
C-   Created  21-MAR-1990   Sylvain Tisserant (MSU)
C-   Revised  18-JUN-1990
C-   Revised 27-Nov-90      Maris Abolins
C-   change IQ(ltrgr+2) =   High order 16 bits - lower 16 bits of 
C-                          IQ(LHEAD+7), lower 16 bits - FF
C-   IQ(LHEAD+11) = IQ(LTRGR+5)
C-   Revised 29-Nov-90: Put trigger mask word in Header (6)
C-                      Put Revision number in Header (5)
C-   Updated  Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                          - Modified for use with L1SIM 
C-                          - Changed name of routine from TRGRFL to
C-                            L1_FW_AND_CT_FILL. 
C-                          - Changed all occurances of
C-                            LEVEL1_DATA_BLOCK_BUILDER to
C-                            L1DBB_DATA_BLOCK_BUILDER. 
C-                          - Replaced D0$PARAMS:LEVEL1_FRAMEWORK.PARAMS with
C-                            D0$PARAMS:L1_FRAMEWORK.PARAMS 
C-                          - Replaced D0$PARAMS:LEVEL1_CAL_TRIG.PARAMS with
C-                            D0$PARAMS:L1_CALTRIG.PARAMS 
C-                          - Replaced D0$INC:LEVEL1_TRIGGER_DATA_BLOCK.INC
C-                            with D0$INC:L1DBB_DATA_BLOCK.INC 
C-                          - Replaced D0$INC:SPECIFIC_TRIGGER.INC with
C-                            D0$INC:L1_SPECIFIC_TRIGGER.INC 
C-                          - Replaced
C-                            D0$PARAMS:LEVEL1_TRIGGER_DATA_BLOCK.PARAMS with
C-                            D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS 
C-   Updated   7-JAN-1992   Philippe Laurens, Steven Klocek  
C-                          Moved header and trailer parameters to
C-                              D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS
C-                          Moved header and trailer declarations to 
C-                              D0$INC:L1DBB_DATA_BLOCK.INC 
C-   Updated   6-FEB-1992   Philippe Laurens, Steven Klocek  
C-                          Moved the call to L1DBB_DATA_BLOCK_BUILDER to
C-                          L1SIM_EVENT so that the call will occur even if no
C-                          triggers fire on an event. 
C-   Updated  28-MAY-1993   Philippe Laurens - MSU L1 Trigger  
C-                          trailer synch word: upper not lower half of word
C-                          first trailer word: whole length, not just trailer
C-   Updated   3-JUN-1993   Philippe Laurens - MSU L1 Trigger   
C-                          Add call to jet list builder (previously done in
C-                          L1DBB_DATA_BLOCK_BUILDER) and use >official<
C-                          routine L1UTIL_JET_LIST_BUILDER (same as in L2) 
C-                          Zero the jet list sections first, 
C-                          also back-annotate the common block copy.
C-                          Add a switch to create or not create the Jet Lists.
C-                          Set version number depending on Jet List Building
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS'
      INCLUDE 'D0$INC:L1SIM_CONTROL.INC'
      INCLUDE 'D0$INC:L1DBB_DATA_BLOCK.INC'
      INCLUDE 'D0$INC:L1_SPECIFIC_TRIGGER.INC'
C
      INTEGER    FFFFFFFF,      FFFF
      PARAMETER (FFFFFFFF = -1, FFFF = 65535)
C
      INTEGER   EM_ET_LIST, TOT_ET_LIST    
      PARAMETER ( EM_ET_LIST = 0, TOT_ET_LIST = 1 )
      INTEGER   JET_LIST_LENGTH
      PARAMETER (JET_LIST_LENGTH = 16)
C
      LOGICAL FIRST
      INTEGER ADDRESS, BYTE, IW, J
      INTEGER LTRGR, I, PNTR
C
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
C     Header and Trailer Block filling
C     ================================
C                                                                     Only once
C                                                                     ---------
      IF (FIRST) THEN
        L1_CRATE_HEADER(1) = TRGR_HEADER_LENGTH - 1
        L1_CRATE_HEADER(2) = FFFF
        L1_CRATE_HEADER(3) = 0
        CALL SBYT (CRATE_ID, L1_CRATE_HEADER(3), FOURTH_BYTE, 
     &               BYTE_LENGTH)
C        
        IF ( CREATE_JET_LISTS .EQV. .TRUE.) THEN 
          L1_CRATE_HEADER(4) = VERSION_NUMBER_ALT
        ELSE
          L1_CRATE_HEADER(4) = VERSION_NUMBER
        ENDIF
        CALL SBIT1 (L1_CRATE_HEADER(4), BIT_29)
        L1_CRATE_HEADER(5) = 0
        CALL SBYT (REVISION_NUMBER, L1_CRATE_HEADER(5), FOURTH_BYTE, 
     &             BYTE_LENGTH)
        L1_CRATE_TRAILER(1) = TRGR_BANK_LENGTH
        L1_CRATE_TRAILER(2) = CRATE_ID
        L1_CRATE_TRAILER(3) = FFFFFFFF
        FIRST = .FALSE.
      ENDIF
C                                                                 At each event
C                                                                 -------------
C
C       Moved the following call to L1SIM_EVENT  6-FEB-1992 
C***      CALL L1DBB_DATA_BLOCK_BUILDER()
C
C
C    now fill in "synch bits" in header and trailer   
      CALL SBYT (TRIGGER_SCALER(1), L1_CRATE_HEADER(2),  THIRD_BYTE, 
     &           WORD_LENGTH)
      CALL SBYT (TRIGGER_SCALER(1), L1_CRATE_TRAILER(2), THIRD_BYTE, 
     &           WORD_LENGTH)
      L1_CRATE_HEADER(6) = FIRED_MASK
C
C     TRGR Zebra bank booking and filling
C     ===================================
C
      CALL BKTRGR (LTRGR, TRGR_BANK_LENGTH)
C*** DEBUGGING CODE
C      CALL MZNEED(IXMAIN, 0, ' ')
C      WRITE (6,*) 'DEBUG: There are', IQUEST(11),
C     &  ' words free space in ZEBCOM'
C***      
C                                                                  Header Block
C                                                                  ------------
      CHECKSUM = 0
      DO I = 1, TRGR_HEADER_LENGTH
        IQ(LTRGR+I) = L1_CRATE_HEADER(I)
C        CHECKSUM    = CHECKSUM + L1_CRATE_HEADER(I)
      ENDDO
C       ADD trigger mask word into block header
      IQ (LHEAD + 11) = IQ (LTRGR + 6)
C                                                                    Data Block
C                                                                    ----------
      PNTR = LTRGR + TRGR_HEADER_LENGTH + 1
      L1_WORD_COUNT = DATA_BLOCK_MAX
      IQ(PNTR) = DATA_BLOCK_MAX
C      CHECKSUM = CHECKSUM + IQ(PNTR)
      DO I = 1, DATA_BLOCK_MAX
        PNTR = PNTR + 1
        IQ(PNTR) = LVL1_DATA_BLOCK (I)
C        CHECKSUM = CHECKSUM + IQ(PNTR)
      ENDDO
C      
C                                                                 Trailer Block
C                                                                 -------------
      DO I = 1, TRGR_TRAILER_LENGTH - 1
        PNTR = PNTR + 1
        IQ(PNTR) = L1_CRATE_TRAILER(I)
C        CHECKSUM = CHECKSUM + IQ(PNTR)
      ENDDO
      IQ(PNTR+1) = -CHECKSUM
C
C
C     Add Jet Lists to L1 Crate in TRGR bank, but zero the section first 
C     (it was copied from the common block which has dragged the old jet lists)
C
      PNTR = (EM_ET_JET_LIST + 1) / 2 
      DO I =  PNTR, (PNTR + JET_LIST_L )
        IQ( LTRGR+TRGR_HEADER_LENGTH+1 + I ) = 0
      ENDDO
C
      IF ( CREATE_JET_LISTS .EQV. .TRUE.) THEN
C
        CALL L1UTIL_JET_LIST_BUILDER( LTRGR+1, EM_ET_LIST,
     &                                JET_LIST_LENGTH, 
     &                                IQ( LTRGR+TRGR_HEADER_LENGTH+1
     &                                  + (EM_ET_JET_LIST+1)/2 ), 
     &                                IQ( LTRGR+TRGR_HEADER_LENGTH+1
     &                                  + (EM_ET_JET_LIST+1)/2 + 1 ) )
        CALL L1UTIL_JET_LIST_BUILDER_STMASK( LTRGR+1, EM_ET_LIST )
C
        CALL L1UTIL_JET_LIST_BUILDER( LTRGR+1, TOT_ET_LIST, 
     &                                JET_LIST_LENGTH, 
     &                                IQ( LTRGR+TRGR_HEADER_LENGTH+1
     &                                  + (TOT_ET_JET_LIST+1)/2 ), 
     &                                IQ( LTRGR+TRGR_HEADER_LENGTH+1
     &                                  + (TOT_ET_JET_LIST+1)/2 + 1 ) )
        CALL L1UTIL_JET_LIST_BUILDER_STMASK( LTRGR+1, TOT_ET_LIST )
C
C     Back annotate jet list in common block 
C     The common block is later used in hex dump, maybe more?)
C
C     2 jet lists + word count (remember the *_L are for the 16-bit array)
        PNTR = (EM_ET_JET_LIST + 1) / 2 
        DO I =  PNTR, (PNTR + JET_LIST_L )
          LVL1_DATA_BLOCK( I ) = IQ( LTRGR+TRGR_HEADER_LENGTH+1 + I ) 
        ENDDO
C      
C     programmed and fired masks
        PNTR = (JET_FIRED + 1) / 2 
        DO I =  PNTR, (PNTR + JET_FIRED_L/2 - 1)
          LVL1_DATA_BLOCK( I ) = IQ( LTRGR+TRGR_HEADER_LENGTH+1 + I ) 
        ENDDO
C
      END IF
C
      RETURN
      END
