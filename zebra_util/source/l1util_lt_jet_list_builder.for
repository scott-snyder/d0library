      SUBROUTINE L1UTIL_LT_JET_LIST_BUILDER( L1CRATE, MAX_ENTRY,
     &                                       TOT_ENTRY, ENTRY_LIST )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Building the Large Tile jet_list (cf D0 note 967).
C-                         This code is meant to be used by Level 2 nodes. 
C-                         This routine can be called directly or via
C-                         L1UTIL_JET_LIST_BUILDER.FOR which immediately
C-                         transfers control here. 
C-                         
C-   Inputs  : L1CRATE     ZEBRA pointer to the first word of the L1 data in
C-                         the TRGR bank 
C-                         (e.g. use GZFIND_CRATE ( 'TRGR', GZTRGR(), 11 )
C-             MAX_ENTRY   Saturation point of the Jet List
C-
C-   Outputs : TOT_ENTRY   If the list is complete, this is the number of
C-                         entries in the list. If the list is incomplete
C-                         (saturated) the most significant bit of the lower 16
C-                         bit word is set to 1.
C-             ENTRY_LIST  list of the TOT_ENTRY towers in the format described
C-                         in D0 note 967
C-
C-   Controls: None.
C-
C-
C- ENTRY L1UTIL_LT_JET_LIST_BUILD_FIRST
C-
C-   Purpose and Methods : Set the FIRST flag to .TRUE., cf. below
C-
C-   Created  10-JUL-1993   Philippe Laurens - MSU L1 Trigger  
C-   Updated  12-JAN-1994   Philippe Laurens - Herb Greenlee
C-                          fix machine architecture dependency problem 
C-                            with array LT_PATTERN_BY_BYT
C-                          add DATA statement on LAST_PROCESSED_EVENT for FLINT
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS/LIST'
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS/LIST'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS/LIST'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS/LIST'
      INCLUDE 'D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS/LIST'
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
C       Arguments
C
      INTEGER   L1CRATE
      INTEGER   MAX_ENTRY
      INTEGER   TOT_ENTRY
      INTEGER   SPEC_TRIG_MASK, TWR_ADDR
      PARAMETER ( SPEC_TRIG_MASK = 0, TWR_ADDR = 1 )
      INTEGER   ENTRY_LIST ( SPEC_TRIG_MASK:TWR_ADDR, 1:TOWER_MAX )
C
C       Parameters
C
      INTEGER   NO_BIT_SET, LOWER_16_BITS 
      PARAMETER ( NO_BIT_SET = 0, LOWER_16_BITS = 255 * 256 + 255 ) 
      INTEGER   FLAG_INCOMPLETE 
      PARAMETER ( FLAG_INCOMPLETE = 128 * 256 ) 
      INTEGER   REF_0, REF_7
      PARAMETER ( REF_0 = 0, REF_7 = 7 ) 
C
C       I_* ARE offsets in the TRGR Bank cf. d0 note 967
C       parameters derived from L1DBB_DATA_BLOCK.PARAMS
      INTEGER    I_EM_ADC, I_HD_ADC
      PARAMETER (I_EM_ADC       = ( TT_FADC + 1 ) / 2 )
      PARAMETER (I_HD_ADC       = I_EM_ADC + TT_FADC_L/4 )
      INTEGER    I_LT_PATTERN 
      PARAMETER (I_LT_PATTERN   = ( LRG_TILE_JET_PTTRN_MSK + 1 ) / 2 )
      INTEGER    I_SPTRG_LT_PRG  
      PARAMETER (I_SPTRG_LT_PRG = ( LRG_TILE_JET_PROGRAMMING + 1 ) / 2 )
C
C       Local Variables
C
      LOGICAL   LIST_SATURATED 
      INTEGER   SPTRG_FIRED
      INTEGER   SPTRG_FIRED_FOR_REFSET ( REF_0 : REF_7 )
      INTEGER   CHECK_LIST_NEEDED
      INTEGER   LT_ETA_SIGN, LT_ETA_MAGN, LT_PHI, REF_NUM
      INTEGER   BASE_TT_ETA, BASE_TT_PHI 
      INTEGER   REL_TT_ETA, REL_TT_PHI 
      INTEGER   BIT_NUM_IN_BYT 
      INTEGER   LT_PHI_MASK_ALL_REF 
      INTEGER   ENTRY_MASK
      INTEGER   ENTRY_ADDR
      INTEGER   ENTRY_ENERGY
      INTEGER   ZEBRA_BYTE
      INTEGER   BIT_MASK
      INTEGER   SINGLE_BIT_MASK(1:8)
C       ZI_* are indices in the zebra array IQ
      INTEGER   ZI_PNTR
      INTEGER   ZI_EM, ZI_HD
      INTEGER   ZI_SPTRG_FIRED, ZI_LT_PATTERN, ZI_SPTRG_PROGR
C
C       byte unpacking equivalence
C
      INTEGER   SPTRG_MASK_BY_INT,  ZEBRA_BY_INT,
     &          LT_PHI_MASK_BY_INT, ENERGY_BY_INT, TEMP_BY_INT
      INTEGER*2 ENERGY_BY_WRD(1:2)
      BYTE      SPTRG_MASK_BY_BYT(1:4),  ZEBRA_BY_BYT(1:4),  
     &          LT_PHI_MASK_BY_BYT(1:4), ENERGY_BY_BYT(1:4),
     &          TEMP_BY_BYT(1:4)
      EQUIVALENCE ( SPTRG_MASK_BY_INT,  SPTRG_MASK_BY_BYT  )
      EQUIVALENCE ( ZEBRA_BY_INT,       ZEBRA_BY_BYT       )
      EQUIVALENCE ( TEMP_BY_INT,        TEMP_BY_BYT       )
      EQUIVALENCE ( LT_PHI_MASK_BY_INT, LT_PHI_MASK_BY_BYT ) 
      EQUIVALENCE ( ENERGY_BY_INT, ENERGY_BY_BYT, ENERGY_BY_WRD )
      INTEGER   LT_PATTERN_BY_INT(1:20)
      BYTE      LT_PATTERN_BY_BYT(1:2,LT_ETA_MIN:LT_ETA_MAX,REF_0:REF_7)
      EQUIVALENCE ( LT_PATTERN_BY_INT,  LT_PATTERN_BY_BYT )
C
C       variables used in sorting the jet_lists
C
      INTEGER   SWAP_REACH, SWAPPER
      INTEGER   LOW_INDEX, HIGH_INDEX
      INTEGER   LOW_INDEX_LIMIT, LOW_INDEX_LEVEL
C
C       Variables Saved between calls
C
      LOGICAL   FIRST 
      INTEGER   LAST_PROCESSED_EVENT 
      SAVE      FIRST, LAST_PROCESSED_EVENT, SPTRG_FIRED_FOR_REFSET
C
C                               TABLES used to calculate the jet_address
C                               ---------------------------------
      INTEGER   PHI_OFFSET (LT_PHI_MIN:LT_PHI_MAX)
      INTEGER   ETA_OFFSET (LT_ETA_MIN:LT_ETA_MAX)
      INTEGER   SIGN_OFFSET 
      PARAMETER ( SIGN_OFFSET = 640 ) 
C
C               Calculate the offset in the ADC list corresponding to phi
C               ---------------------------------------------------------
      DATA      PHI_OFFSET / 0, 16, 1, 17 /
C
C               Calculate the offset in the ADC list corresponding to eta
C               ---------------------------------------------------------
      DATA      ETA_OFFSET / 0, 128, 256, 384, 512 /
C
      DATA      SINGLE_BIT_MASK / 1, 2, 4, 8, 16, 32, 64, 128 /
      DATA      FIRST /.TRUE./
      DATA      LAST_PROCESSED_EVENT /-1/
C
C =============================================================================
C       Initialize appropriate variables
C
      TOT_ENTRY      = 0
      LIST_SATURATED = .FALSE.
C =============================================================================
C       Check that the routine hasn't already been called and exit
C       if a jet list of the same type has already been built for this event.
C       Use the Event Number in IQ(LHEAD+7)
C       But, for safety, skip checking the first call
C
      IF ( FIRST .EQV. .FALSE. ) THEN
        IF (IQ(LHEAD+7) .EQ. LAST_PROCESSED_EVENT ) GOTO 999
      ELSE
        FIRST = .FALSE.
      ENDIF
C
      LAST_PROCESSED_EVENT = IQ( LHEAD + 7 )
C
C =============================================================================
C       Find pointers in TRGR bank
C
C       ZI_* are indices in the zebra array IQ
C
C       pointer to the specific trigger pattern
      ZI_SPTRG_FIRED    = L1CRATE + TRGR_HEADER_LENGTH - 1
C       pointer to the beginning of the EM ADC list
      ZI_EM             = L1CRATE + TRGR_HEADER_LENGTH + I_EM_ADC
C       pointer to the beginning of the HD ADC list
      ZI_HD             = L1CRATE + TRGR_HEADER_LENGTH + I_HD_ADC
C
C       pointer to the beginning of the Large Tile Jet pattern list
      ZI_LT_PATTERN = L1CRATE + TRGR_HEADER_LENGTH + I_LT_PATTERN
C       pointer to the beginning of the Sptrg Programming Section
      ZI_SPTRG_PROGR = L1CRATE + TRGR_HEADER_LENGTH + I_SPTRG_LT_PRG
C
C =============================================================================
C    For each reference set, build the masks of specific trigger which fired
C    and were using the reference set. )
C
      SPTRG_FIRED = IQ( ZI_SPTRG_FIRED )
      CHECK_LIST_NEEDED = 0
C
      ZI_PNTR = ZI_SPTRG_PROGR
C
      DO REF_NUM = REF_0, REF_7
        ZEBRA_BY_INT             = IQ(ZI_PNTR)
        SPTRG_MASK_BY_BYT(BYTE1) = ZEBRA_BY_BYT(BYTE1)
        SPTRG_MASK_BY_BYT(BYTE2) = ZEBRA_BY_BYT(BYTE3)
        ZI_PNTR                  = ZI_PNTR + 1
        ZEBRA_BY_INT             = IQ(ZI_PNTR)
        SPTRG_MASK_BY_BYT(BYTE3) = ZEBRA_BY_BYT(BYTE1)
        SPTRG_MASK_BY_BYT(BYTE4) = ZEBRA_BY_BYT(BYTE3)
        ZI_PNTR = ZI_PNTR + 1
C      to be in the final mask, a sptrg must be using the refset AND have fired
        SPTRG_FIRED_FOR_REFSET(REF_NUM) 
     &      = IAND( SPTRG_FIRED, SPTRG_MASK_BY_INT )
        CHECK_LIST_NEEDED = IOR( CHECK_LIST_NEEDED, 
     &                           SPTRG_FIRED_FOR_REFSET(REF_NUM) )
      END DO
C
C       No work needs to be done if none of the sptrg that fired were using
C       any of the ref sets.
C
      IF ( CHECK_LIST_NEEDED .EQ. NO_BIT_SET ) GOTO 999
C
C =============================================================================
C       Look for Jet entries
C       ====================
C
C       Transfer the cumbersome longword array of Lage Tile Jet Pattern
C       into a more convenient byte unpacked variable
      DO ZI_PNTR = ZI_LT_PATTERN, ZI_LT_PATTERN + 19
C-
C- The following machine block is for efficiency in level 2.  The ELSE
C- part of the machine block is machine independent.  The IF part is only
C- for little endian machines.  -- HBG.
C-
C&IF VAXVMS,VAXELN
        ZEBRA_BY_INT = IQ( ZI_PNTR )
C&ELSE
C&        TEMP_BY_INT = IQ( ZI_PNTR )
C&        ZEBRA_BY_BYT(BYTE1) = TEMP_BY_BYT(1)
C&        ZEBRA_BY_BYT(BYTE2) = TEMP_BY_BYT(2)
C&        ZEBRA_BY_BYT(BYTE3) = TEMP_BY_BYT(3)
C&        ZEBRA_BY_BYT(BYTE4) = TEMP_BY_BYT(4)
C&ENDIF
        LT_PATTERN_BY_INT( ZI_PNTR - ZI_LT_PATTERN + 1 ) = ZEBRA_BY_INT
      ENDDO
C
C     Now explicitely build a DO-loop: DO ETA_MAGN = LT_ETA_MIN, LT_ETA_MAX
C     That is to satisfy D0flavor, as we need to "GOTO" back in.
      LT_ETA_MAGN = LT_ETA_MIN
      LT_PHI_MASK_BY_INT = 0 
  100 CONTINUE
C
C         Is there at least one entry for the 8 large tiles at |LT_ETA_MAGN|?
C         Start by ORing all reference sets, then globally check for a bit set
        LT_PHI_MASK_ALL_REF = 0
C
        DO REF_NUM = REF_0, REF_7
          IF ( SPTRG_FIRED_FOR_REFSET( REF_NUM ) .NE. NO_BIT_SET ) THEN
            LT_PHI_MASK_BY_BYT(BYTE1) =  
     &        LT_PATTERN_BY_BYT( 1, LT_ETA_MAGN, REF_NUM ) 
            LT_PHI_MASK_ALL_REF = IOR( LT_PHI_MASK_ALL_REF,
     &                                 LT_PHI_MASK_BY_INT )
          END IF
        END DO
C
        IF ( LT_PHI_MASK_ALL_REF .NE. NO_BIT_SET ) GOTO 200
C
  110   CONTINUE
C
C       end of explicit DO-loop
      LT_ETA_MAGN = LT_ETA_MAGN + 1
      IF ( LT_ETA_MAGN .LE. LT_ETA_MAX ) GOTO 100
C
  130 CONTINUE
      IF ( TOT_ENTRY .GT. MAX_ENTRY ) THEN
C*        PRINT *, ' Jet List Saturated'
        LIST_SATURATED = .TRUE.
        TOT_ENTRY      = MAX_ENTRY 
        GOTO 600 
      ELSE
C       order list
        IF ( TOT_ENTRY .GT. 1 ) THEN 
C*          PRINT *, ' Sorting Jet List'
          GOTO 500 
        ELSE
          GOTO 600 
        ENDIF
      ENDIF
C
C =============================================================================
C       Find non-zero-bit within non-zero-byte
C       ======================================
  200 CONTINUE
      BIT_NUM_IN_BYT = 8
  300 CONTINUE
      BIT_MASK = SINGLE_BIT_MASK( BIT_NUM_IN_BYT ) 
      IF ( IAND ( LT_PHI_MASK_ALL_REF, BIT_MASK ) .NE. 0 ) GOTO 400
C
      BIT_NUM_IN_BYT = BIT_NUM_IN_BYT - 1
      GOTO 300
C =============================================================================
C       Compute Tower's Entry mask and Entry address
C       ============================================
  400 CONTINUE
      LT_ETA_SIGN = (BIT_NUM_IN_BYT-1) / 4 
      LT_PHI  = BIT_NUM_IN_BYT - 4 * LT_ETA_SIGN
C
C       the tag is a one dimension address of the tower cf D0 Note 967
      ENTRY_ADDR = PHI_OFFSET( LT_PHI ) + ETA_OFFSET( LT_ETA_MAGN )
      IF ( LT_ETA_SIGN .EQ. NEG_ETA ) 
     &  ENTRY_ADDR = ENTRY_ADDR + SIGN_OFFSET
C
      ENTRY_MASK       = 0
C
C       The sptrg fired masks of all the refsets that this tower passed
C       are ORed to obtain the tower's entry mask.
C
      DO REF_NUM = REF_0 , REF_7
        IF ( SPTRG_FIRED_FOR_REFSET( REF_NUM ) .NE. NO_BIT_SET ) THEN
          LT_PHI_MASK_BY_BYT(BYTE1) = 
     &        LT_PATTERN_BY_BYT( 1, LT_ETA_MAGN, REF_NUM ) 
          IF ( IAND( LT_PHI_MASK_BY_INT, BIT_MASK ) .NE. 0 ) 
     &       ENTRY_MASK = IOR ( ENTRY_MASK, 
     &                          SPTRG_FIRED_FOR_REFSET ( REF_NUM ) )
        ENDIF
      ENDDO
C
C =============================================================================
C       add entry to (not yet ordered) Jet List
C       =======================================
C
      TOT_ENTRY = TOT_ENTRY + 1
      IF ( TOT_ENTRY .GT. MAX_ENTRY ) GOTO 130
C
      ENTRY_LIST ( SPEC_TRIG_MASK, TOT_ENTRY ) = ENTRY_MASK
C
C       Now build Large Tile Eenergy
      ENTRY_ENERGY  = 0 
      ENERGY_BY_INT = 0
C
C       find zebra word holding Trigger Tower's EM energy
      DO REL_TT_ETA = 1 , 4
        DO REL_TT_PHI = 1 , 8 , 2
C         Base addresses for Large Tiles always is 
C         on byte#1 for LT_PHI [1..8] and [9..16] and 
C         on byte#2 for LT_PHI [17..24] and [25..32]
C         The EM and HD energy for next 8 towers in phi 
C         and 4 in eta need to be summed (cf. D0 note 967 for ADC order)
          ZI_PNTR = ENTRY_ADDR / 4 
     &            + ( 16 * (REL_TT_ETA-1) + (REL_TT_PHI-1) ) /2
          IF ( LT_PHI .LT. 3 ) THEN 
            ZEBRA_BY_INT  = IQ ( ZI_EM + ZI_PNTR )
            ENERGY_BY_BYT(BYTE1) = ZEBRA_BY_BYT (BYTE1)
            ENTRY_ENERGY         = ENTRY_ENERGY + ENERGY_BY_INT
            ENERGY_BY_BYT(BYTE1) = ZEBRA_BY_BYT (BYTE3)
            ENTRY_ENERGY         = ENTRY_ENERGY + ENERGY_BY_INT
            ZEBRA_BY_INT  = IQ ( ZI_HD + ZI_PNTR )
            ENERGY_BY_BYT(BYTE1) = ZEBRA_BY_BYT (BYTE1)
            ENTRY_ENERGY         = ENTRY_ENERGY + ENERGY_BY_INT
            ENERGY_BY_BYT(BYTE1) = ZEBRA_BY_BYT (BYTE3)
            ENTRY_ENERGY         = ENTRY_ENERGY + ENERGY_BY_INT
          ELSE
            ZEBRA_BY_INT  = IQ ( ZI_EM + ZI_PNTR )
            ENERGY_BY_BYT(BYTE1) = ZEBRA_BY_BYT (BYTE2)
            ENTRY_ENERGY         = ENTRY_ENERGY + ENERGY_BY_INT
            ENERGY_BY_BYT(BYTE1) = ZEBRA_BY_BYT (BYTE4)
            ENTRY_ENERGY         = ENTRY_ENERGY + ENERGY_BY_INT
            ZEBRA_BY_INT  = IQ ( ZI_HD + ZI_PNTR )
            ENERGY_BY_BYT(BYTE1) = ZEBRA_BY_BYT (BYTE2)
            ENTRY_ENERGY         = ENTRY_ENERGY + ENERGY_BY_INT
            ENERGY_BY_BYT(BYTE1) = ZEBRA_BY_BYT (BYTE4)
            ENTRY_ENERGY         = ENTRY_ENERGY + ENERGY_BY_INT
          END IF
        END DO
      END DO
C
C       The list must be ordered in decreasing EM (or TOT=EM+HD) energy. To
C       achieve this, we will TEMPORARILY force the upper two bytes (actually
C       only the lower 14 bits of the upper 2 bytes) of the entry tag (which
C       only uses 11 bits of the lower two bytes) to hold the tower's energy.
C       The list will then be sorted on this extended tag value. After the
C       ordering the tag will be stripped down to its normal content. 
C
      ENERGY_BY_INT        = ENTRY_ENERGY
      ENERGY_BY_WRD(WORD2) = ENERGY_BY_WRD(WORD1)
      ENERGY_BY_WRD(WORD1) = 0 
C
      ENTRY_LIST( TWR_ADDR, TOT_ENTRY) = IOR( ENTRY_ADDR, ENERGY_BY_INT)
C
C =============================================================================
C       clear bit found and go back to globally check for byte-level zero
C       =================================================================
C
C       Clear bit that was just found
C
      LT_PHI_MASK_ALL_REF = IEOR ( LT_PHI_MASK_ALL_REF, BIT_MASK )
C
C       Globally check if all Large tiles were exhausted before going to next
C       ETA, or globally check same byte mask before looking for further bits
C
      IF ( LT_PHI_MASK_ALL_REF .EQ. NO_BIT_SET ) THEN
        GOTO 110 
      ELSE
        BIT_NUM_IN_BYT = BIT_NUM_IN_BYT - 1
        GOTO 300
      END IF
C
C =============================================================================
C =============================================================================
C       Now order the Jet List 
C       ======================
C       This algorithm is derived from the D0 routine SRTINT.
C       It is an enhanced "bubble-up" algorithm where the "reach" of element
C       swapping starts at half the size of the list and is dynamically shrunk.
  500 CONTINUE
      SWAP_REACH = TOT_ENTRY / 2       
C
C     Start a cycle at current swap reach
  510 LOW_INDEX_LIMIT = TOT_ENTRY - SWAP_REACH
      LOW_INDEX       = 1 
      LOW_INDEX_LEVEL = 1
C
C     Compare the current element to the reachable target 
  520 HIGH_INDEX = LOW_INDEX + SWAP_REACH
C
      IF   ( ENTRY_LIST( TWR_ADDR, LOW_INDEX ) 
     &  .LE. ENTRY_LIST( TWR_ADDR, HIGH_INDEX ) ) THEN
C
C       Swap high and low index elements
        SWAPPER = ENTRY_LIST( TWR_ADDR, LOW_INDEX ) 
        ENTRY_LIST( TWR_ADDR, LOW_INDEX ) 
     &          = ENTRY_LIST( TWR_ADDR, HIGH_INDEX )
        ENTRY_LIST( TWR_ADDR, HIGH_INDEX ) = SWAPPER 
        SWAPPER = ENTRY_LIST( SPEC_TRIG_MASK, LOW_INDEX ) 
        ENTRY_LIST( SPEC_TRIG_MASK, LOW_INDEX ) 
     &          = ENTRY_LIST( SPEC_TRIG_MASK, HIGH_INDEX )
        ENTRY_LIST( SPEC_TRIG_MASK, HIGH_INDEX ) = SWAPPER 
C
C       See if there is room to push element down some more, before moving on
        LOW_INDEX  = LOW_INDEX - SWAP_REACH
        IF ( LOW_INDEX .GT. 0 ) GOTO 520
      END IF
C
C     Keep track of highest considered low index within curent reach cycle
      LOW_INDEX_LEVEL = LOW_INDEX_LEVEL + 1
C
C     Shrink Swap reach untill done
      IF ( LOW_INDEX_LEVEL .GT. LOW_INDEX_LIMIT ) THEN 
        SWAP_REACH = SWAP_REACH / 2       
        IF ( SWAP_REACH .EQ. 0 ) GOTO 600 
        GOTO 510
      END IF
C
C     Back for next low index for current swap reach
      LOW_INDEX = LOW_INDEX_LEVEL 
      GOTO 520
C
C===========================================================================
C       Strip the upper 16 bits of the tower address tags
C       =================================================
  600 CONTINUE
C
      DO  LOW_INDEX =  1, TOT_ENTRY
C
C       strip the temporary energy value in the upper 16 bits
        ENTRY_LIST( TWR_ADDR, LOW_INDEX ) 
     &    = IAND( ENTRY_LIST( TWR_ADDR, LOW_INDEX ), LOWER_16_BITS )
C
      ENDDO
C
C       merge the saturation flag with the Entry Count if needed
      IF ( LIST_SATURATED .EQV. .TRUE.) 
     &  TOT_ENTRY = TOT_ENTRY + FLAG_INCOMPLETE
C----------------------------------------------------------------------------
  999 RETURN
C
C
C
      ENTRY L1UTIL_LT_JET_LIST_BUILD_FIRST()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set the FIRST flag to .TRUE. so that the Jet List
C-                         Builder can be run multiple times on  the same event.
C-                         Normally the Jet List Builder keeps track of the
C-                         Event Number from the HEAD bank, to avoid
C-                         recalculating the same Jet List multiple times.
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  10-APR-1992   Philippe Laurens, Steven Klocek
C-   Updated  10-JUL-1993   Philippe Laurens - MSU L1 Trigger   
C-              Add Large Tile
C-
C----------------------------------------------------------------------
      FIRST = .TRUE.
      RETURN
      END
