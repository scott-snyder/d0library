      SUBROUTINE L1UTIL_JET_LIST_BUILDER( L1CRATE, LIST_TYPE, MAX_ENTRY,
     &                                    TOT_ENTRY, ENTRY_LIST )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Building the jet_list (cf D0 note 967).
C-                         Written to be equivalent to the 68k assembly
C-                         language code of the Level 1 VME Transfer Program.
C-                         This code is meant to be used by the Level 1
C-                         Simulator, and online by Level 2 processing nodes.
C- 
C-   Note:                 This code has been implemented using the EQUIVALENCE
C-                         and BYTE arrays to transfer bytes. It also only uses
C-                         AND, OR, EXOR intrinsic function that are compiled
C-                         to VAX machine instructions (not calls to RTL
C-                         routines like for MVBITS). 
C-                         The performance gain of IAND over MVBITS is in the
C-                         range from 2 to 5 
C-
C-   Inputs  : L1CRATE     ZEBRA pointer to the first word of the L1 data in
C-                         the TRGR bank 
C-                         (e.g. use GZFIND_CRATE ( 'TRGR', GZTRGR(), 11 )
C-             LIST_TYPE   0 for EM Et Jet List, 
C-                         1 for TOT Et Jet List,
C-                         2 for Large Tile Jet List 
C-                            (the code for the Large Tile Jet List is in
C-                            L1UTIL_LT_JET_LIST_BUILDER)
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
C- ENTRY L1UTIL_JET_LIST_BUILDER_FIRST(LIST_TYPE)
C-
C-   Purpose and Methods : Set the FIRST flag to .TRUE. for a particular type
C-     Jet List so that the Jet List Builder can be run multiple times on 
C-     the same event.  Normally the Jet List Builder keeps track of the 
C-     Event Number from the HEAD bank, to avoid recalculating a Jet List 
C-     multiple times for the same event.
C-
C- ENTRY L1UTIL_JET_LIST_BUILDER_STMASK(L1CRATE, LIST_TYPE)
C- 
C-   Purpose and Methods : Write to the TRGR bank the masks of Specific
C-     Triggers fired using each Reference Set. This entry point should be
C-     called immediately after building the Jet List
C-
C-
C-   Created   9-MAR-1990   Sylvain Tisserant (MSU)
C-   Revised  18-JUN-1990  Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-   REWRITE  30-AUG-1991   Philippe Laurens (MSU)
C-   -------                Add arguments for re-building a longer (complete)
C-                          Jet list in Level 2 nodes.
C-                          Re-Write necessary to read from ZEBRA bank TRGR
C-   Updated   6-FEB-1992   Philippe Laurens, Steven Klocek   
C-                          Pass pointer to first word of L1 crate rather than
C-                            pointer to TRGR bank.
C-                          Various bug fixes.
C-                          Add entry point L1UTIL_JET_LIST_BUILDER_FIRST.
C-   Updated   3-JUN-1993   Philippe Laurens - MSU L1 Trigger  
C-        add entry point to retrieve the masks of Sptrg fired using each Refset
C-   Updated  10-JUL-1993   Philippe Laurens - MSU L1 Trigger  
C-         add option to generate a Large Tile Jet List. The code for
C-         this service is in L1UTIL_LT_JET_LIST_BUILDER.FOR. This routine
C-         immediately transfers control.
C-   Updated  12-JAN-1994   Philippe Laurens - MSU L1 Trigger   
C-                          add DATA statement on LAST_PROCESSED_EVENT for FLINT
C-
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
      INTEGER   EM_ET_LIST, TOT_ET_LIST, LT_LIST
      PARAMETER ( EM_ET_LIST = 0, TOT_ET_LIST = 1, LT_LIST = 2 )
      INTEGER   LIST_TYPE ! allowed values EM_ET_LIST, TOT_ET_LIST, or LT_LIST
      INTEGER   MAX_ENTRY
      INTEGER   TOT_ENTRY
      INTEGER   SPEC_TRIG_MASK, TWR_ADDR
      PARAMETER ( SPEC_TRIG_MASK = 0, TWR_ADDR = 1 )
      INTEGER   ENTRY_LIST ( SPEC_TRIG_MASK:TWR_ADDR, 1:TOWER_MAX )
C
C       Arguments to entry point L1UTIL_JET_LIST_BUILDER_FIRST
C
      INTEGER LIST_TYPE_FIRST ! allowed EM_ET_LIST, TOT_ET_LIST, or LT_LIST
C
C       Parameters
C
      INTEGER   NO_BIT_SET, LOWER_16_BITS 
      PARAMETER ( NO_BIT_SET = 0, LOWER_16_BITS = 255 * 256 + 255 ) 
      INTEGER   FLAG_INCOMPLETE 
      PARAMETER ( FLAG_INCOMPLETE = 128 * 256 ) 
      INTEGER   REF_0, REF_1, REF_2, REF_3 
      PARAMETER ( REF_0 = 0, REF_1 = 1, REF_2 = 2, REF_3 = 3 ) 
C
      INTEGER   ETA_RANGE 
      PARAMETER ( ETA_RANGE = ETA_MAX - ETA_MIN + 1 ) 
C       Jet pattern offset to each Ref Set
      INTEGER   JP_RS_0,  JP_RS_1,  JP_RS_2,  JP_RS_3
      PARAMETER ( JP_RS_0 = 0, 
     &            JP_RS_1 = JP_RS_0 + 2 * ETA_RANGE, 
     &            JP_RS_2 = JP_RS_1 + 2 * ETA_RANGE, 
     &            JP_RS_3 = JP_RS_2 + 2 * ETA_RANGE )
C
C       I_* ARE offsets in the TRGR Bank cf. d0 note 967
C       parameters derived from L1DBB_DATA_BLOCK.PARAMS
      INTEGER    I_EM_ADC, I_HD_ADC
      PARAMETER (I_EM_ADC      = ( TT_FADC + 1 ) / 2 )
      PARAMETER (I_HD_ADC      = I_EM_ADC + TT_FADC_L/4 )
      INTEGER    I_EM_PATTERN, I_TOT_PATTERN
      PARAMETER (I_EM_PATTERN  = ( JET_PTTRN_MSK + 1 ) / 2 )
      PARAMETER (I_TOT_PATTERN = I_EM_PATTERN + JET_PTTRN_MSK_L/4 )
      INTEGER    I_SPTRG_EM_PRG, I_SPTRG_TOT_PRG 
      PARAMETER (I_SPTRG_EM_PRG  = ( JET_PROGRAMMING + 1 ) / 2 ) 
      PARAMETER (I_SPTRG_TOT_PRG = I_SPTRG_EM_PRG + JET_PROGRAMMING_L/4)
      INTEGER    I_SPTRG_EM_MASK, I_SPTRG_TOT_MASK
      PARAMETER (I_SPTRG_EM_MASK = I_SPTRG_TOT_PRG 
     &                             + JET_PROGRAMMING_L/4 + 1)
      PARAMETER (I_SPTRG_TOT_MASK = I_SPTRG_EM_MASK 
     &                              + (REF_3 - REF_0 + 1) )
C
C       Local Variables
C
      LOGICAL   LIST_SATURATED 
      INTEGER   SPTRG_FIRED
      INTEGER   SPTRG_FIRED_FOR_REFSET ( REF_0 : REF_3 )
      INTEGER   ETA_SIGN, ETA_MAGN, PHI, REF_NUM
      INTEGER   BYTE_NUM, BIT_NUM_IN_BYT, BIT_NUM_IN_INT
      INTEGER   ENTRY_MASK
      INTEGER   ENTRY_ADDR
      INTEGER   ENTRY_ENERGY
      INTEGER   ZEBRA_BYTE
      INTEGER   BIT_MASK
      INTEGER   SINGLE_BIT_MASK(1:8)
      LOGICAL   REF_0_PARTICIPATE, REF_1_PARTICIPATE,
     &          REF_2_PARTICIPATE, REF_3_PARTICIPATE
C       ZI_* are indices in the zebra array IQ
      INTEGER   ZI_PNTR
      INTEGER   ZI_EM, ZI_HD
      INTEGER   ZI_SPTRG_FIRED, ZI_JET_PATTERN, ZI_SPTRG_PROGR
      INTEGER   ZI_SPTRG_REF_MASK
C
C       byte unpacking equivalence
C
      INTEGER   SPTRG_MASK_BY_INT, ZEBRA_BY_INT,
     &          PHI_MASK_BY_INT, BYTE_MASK_BY_INT,
     &          ENERGY_BY_INT
      INTEGER*2 ENERGY_BY_WRD(1:2)
      BYTE      SPTRG_MASK_BY_BYT(1:4), ZEBRA_BY_BYT(1:4),
     &          PHI_MASK_BY_BYT(1:4), BYTE_MASK_BY_BYT(1:4),
     &          ENERGY_BY_BYT(1:4)
      EQUIVALENCE ( SPTRG_MASK_BY_INT,  SPTRG_MASK_BY_BYT  )
      EQUIVALENCE ( ZEBRA_BY_INT,       ZEBRA_BY_BYT       )
      EQUIVALENCE ( PHI_MASK_BY_INT,    PHI_MASK_BY_BYT    )
      EQUIVALENCE ( BYTE_MASK_BY_INT,   BYTE_MASK_BY_BYT   )
      EQUIVALENCE ( ENERGY_BY_INT, ENERGY_BY_BYT, ENERGY_BY_WRD )
C
C       variables used in sorting the jet_lists
C
      INTEGER   SWAP_REACH, SWAPPER
      INTEGER   LOW_INDEX, HIGH_INDEX
      INTEGER   LOW_INDEX_LIMIT, LOW_INDEX_LEVEL
C
C       Variables Saved between calls
C
      LOGICAL   FIRST(EM_ET_LIST:TOT_ET_LIST)
      INTEGER   LAST_PROCESSED_EVENT (EM_ET_LIST:TOT_ET_LIST)
      SAVE      FIRST, LAST_PROCESSED_EVENT, SPTRG_FIRED_FOR_REFSET
C
C                               TABLES used to calculate the jet_address
C                               ---------------------------------
      INTEGER   PHI_FOR_BIT ( 1:32, EM_ET_LIST:TOT_ET_LIST)
      INTEGER   PHI_OFFSET (PHI_MIN:PHI_MAX)
      INTEGER   ETA_OFFSET (ETA_MIN:ETA_MAX)
      INTEGER   SIGN_OFFSET 
      PARAMETER ( SIGN_OFFSET = 640 ) 
C
C                Calculate the phi corresponding to the bit_rank for Em jet list
C                ---------------------------------------------------------------
      DATA      PHI_FOR_BIT
C            Em jet list
C            -----------
     &                   /  8,  7,  6,  5,  4,  3,  2,  1,
     &                     24, 23, 22, 21, 20, 19, 18, 17,
     &                     16, 15, 14, 13, 12, 11, 10,  9,
     &                     32, 31, 30, 29, 28, 27, 26, 25,
C            Tot Et jet list
C            ---------------
     &                      1,  2,  3,  4,  5,  6,  7,  8,
     &                     17, 18, 19, 20, 21, 22, 23, 24,
     &                      9, 10, 11, 12, 13, 14, 15, 16,
     &                     25, 26, 27, 28, 29, 30, 31, 32 /
C
C               Calculate the offset in the ADC list corresponding to phi
C               ---------------------------------------------------------
      DATA      PHI_OFFSET / 0,  2,  4,  6,  8, 10, 12, 14,
     &                      16, 18, 20, 22, 24, 26, 28, 30,
     &                       1,  3,  5,  7,  9, 11, 13, 15,
     &                      17, 19, 21, 23, 25, 27, 29, 31 /
C
C                                     Calculate the offset corresponding to eta
C                                     -----------------------------------------
      DATA      ETA_OFFSET / 0,  32,  64,  96, 128,
     &                     160, 192, 224, 256, 288,
     &                     320, 352, 384, 416, 448,
     &                     480, 512, 544, 576, 608 /
C
      DATA      SINGLE_BIT_MASK / 1, 2, 4, 8, 16, 32, 64, 128 /
      DATA      FIRST /.TRUE., .TRUE./
      DATA      LAST_PROCESSED_EVENT /-1, -1/
C
C =============================================================================
C       This routine has the code for the EM Et and Tot Et Jet Lists. The code
C       for the Large Tile Jet List is in L1UTIL_LT_JET_LIST_BUILDER.FOR
      IF ( LIST_TYPE .EQ. LT_LIST ) THEN 
        CALL L1UTIL_LT_JET_LIST_BUILDER( L1CRATE, MAX_ENTRY,
     &                                   TOT_ENTRY, ENTRY_LIST )
        GOTO 999
      END IF
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
      IF ( FIRST(LIST_TYPE) .EQV. .FALSE. ) THEN
        IF (IQ(LHEAD+7) .EQ. LAST_PROCESSED_EVENT (LIST_TYPE) ) GOTO 999
      ELSE
        FIRST(LIST_TYPE) = .FALSE.
      ENDIF
C
      LAST_PROCESSED_EVENT (LIST_TYPE)  = IQ( LHEAD + 7 )
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
      IF ( LIST_TYPE .EQ. EM_ET_LIST ) THEN
C       pointer to the beginning of the EM Jet pattern list
        ZI_JET_PATTERN = L1CRATE + TRGR_HEADER_LENGTH + I_EM_PATTERN
C       pointer to the beginning of the Sptrg Programming Section
        ZI_SPTRG_PROGR = L1CRATE + TRGR_HEADER_LENGTH + I_SPTRG_EM_PRG
      ELSE
        ZI_JET_PATTERN = L1CRATE + TRGR_HEADER_LENGTH + I_TOT_PATTERN
        ZI_SPTRG_PROGR = L1CRATE + TRGR_HEADER_LENGTH + I_SPTRG_TOT_PRG
      END IF
C
C =============================================================================
C    For each reference set, build the masks of specific trigger which fired
C    and were using the reference set. )
C
      SPTRG_FIRED = IQ( ZI_SPTRG_FIRED )
C
      ZI_PNTR = ZI_SPTRG_PROGR
C
      DO REF_NUM = 0, 3
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
      END DO
C
      REF_0_PARTICIPATE = .TRUE.
      IF ( SPTRG_FIRED_FOR_REFSET( REF_0 ) .EQ. NO_BIT_SET )
     &  REF_0_PARTICIPATE = .FALSE.
C
      REF_1_PARTICIPATE = .TRUE.
      IF ( SPTRG_FIRED_FOR_REFSET( REF_1 ) .EQ. NO_BIT_SET )
     &  REF_1_PARTICIPATE = .FALSE.
C
      REF_2_PARTICIPATE = .TRUE.
      IF ( SPTRG_FIRED_FOR_REFSET( REF_2 ) .EQ. NO_BIT_SET )
     &  REF_2_PARTICIPATE = .FALSE.
C
      REF_3_PARTICIPATE = .TRUE.
      IF ( SPTRG_FIRED_FOR_REFSET( REF_3 ) .EQ. NO_BIT_SET )
     &  REF_3_PARTICIPATE = .FALSE.
C
C       No work needs to be done if none of the sptrg that fired were using
C       any of the ref sets.
C
      IF ( ( REF_0_PARTICIPATE
     &  .OR. REF_1_PARTICIPATE
     &  .OR. REF_2_PARTICIPATE
     &  .OR. REF_3_PARTICIPATE ) .EQV. .FALSE. ) GOTO 999
C
C =============================================================================
C       Look for Jet entries
C       ====================
C
C       prepare the zebra index pointer to
C       the beginning of the jet pattern
      ZI_PNTR    = ZI_JET_PATTERN
C
C     Now explicitely build a DO-loop: DO ETA_MAGN = ETA_MIN, ETA_MAX
C     That is to satisfy D0flavor, as we need to "GOTO" back in.
      ETA_MAGN = ETA_MIN
  100 CONTINUE
C
C         Is there at least one entry for the 32 phi towers at (+ ETA_MAGN)?
C         Start by ORing all 4 reference sets, then globally check for a bit set
        PHI_MASK_BY_INT = 0
C
        IF (REF_0_PARTICIPATE .EQV. .TRUE.) THEN
          PHI_MASK_BY_INT = IQ( ZI_PNTR + JP_RS_0 )
        ENDIF
C
        IF (REF_1_PARTICIPATE .EQV. .TRUE.) THEN
          PHI_MASK_BY_INT = IOR ( 
     &           PHI_MASK_BY_INT, IQ( ZI_PNTR + JP_RS_1 ) )
        ENDIF
C
        IF (REF_2_PARTICIPATE .EQV. .TRUE.) THEN
          PHI_MASK_BY_INT = IOR (
     &           PHI_MASK_BY_INT, IQ( ZI_PNTR + JP_RS_2 ) )
        ENDIF
C
        IF (REF_3_PARTICIPATE .EQV. .TRUE.) THEN
          PHI_MASK_BY_INT = IOR (
     &           PHI_MASK_BY_INT, IQ( ZI_PNTR + JP_RS_3 ) )
        ENDIF
C
        IF ( PHI_MASK_BY_INT .NE. NO_BIT_SET ) THEN
          ETA_SIGN = POS_ETA
          GOTO 200
        END IF
C
  110   CONTINUE
        ZI_PNTR = ZI_PNTR + ETA_RANGE
C
C         Is there at least one entry for the 32 phi towers at (- ETA_MAGN)?
C         Start by ORing all 4 reference sets, then globally check for a bit set
        IF (REF_0_PARTICIPATE .EQV. .TRUE.) THEN
          PHI_MASK_BY_INT = IQ( ZI_PNTR + JP_RS_0 )
        ENDIF
C
        IF (REF_1_PARTICIPATE .EQV. .TRUE.) THEN
          PHI_MASK_BY_INT = IOR (
     &           PHI_MASK_BY_INT, IQ( ZI_PNTR + JP_RS_1 ) )
        ENDIF
C
        IF (REF_2_PARTICIPATE .EQV. .TRUE.) THEN
          PHI_MASK_BY_INT = IOR (
     &           PHI_MASK_BY_INT, IQ( ZI_PNTR + JP_RS_2 ) )
        ENDIF
C
        IF (REF_3_PARTICIPATE .EQV. .TRUE.) THEN
          PHI_MASK_BY_INT = IOR (
     &           PHI_MASK_BY_INT, IQ( ZI_PNTR + JP_RS_3 ) )
        ENDIF
C
        IF ( PHI_MASK_BY_INT .NE. NO_BIT_SET ) THEN
          ETA_SIGN = NEG_ETA
          GOTO 200
        END IF
C
  120   CONTINUE
C         update Zebra Index Pointer to get ready for next eta
        ZI_PNTR = ZI_PNTR - ETA_RANGE + 1
C
C       end of explicit DO-loop
      ETA_MAGN = ETA_MAGN + 1
      IF ( ETA_MAGN .LE. ETA_MAX ) GOTO 100
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
C       Find non-zero-byte within non-zero-longword
C       ===========================================
  200 CONTINUE
      BYTE_MASK_BY_INT = 0
      BIT_NUM_IN_BYT = 8
  210 CONTINUE
      BYTE_MASK_BY_BYT(BYTE1) = PHI_MASK_BY_BYT(BYTE4)
      IF ( BYTE_MASK_BY_INT .NE. NO_BIT_SET ) THEN
        BYTE_NUM = 4
        GOTO 300
      ENDIF
  220 CONTINUE
      BYTE_MASK_BY_BYT(BYTE1) = PHI_MASK_BY_BYT(BYTE3)
      IF ( BYTE_MASK_BY_INT .NE. NO_BIT_SET ) THEN
        BYTE_NUM = 3
        GOTO 300
      ENDIF
  230 CONTINUE
      BYTE_MASK_BY_BYT(BYTE1) = PHI_MASK_BY_BYT(BYTE2)
      IF ( BYTE_MASK_BY_INT .NE. NO_BIT_SET ) THEN
        BYTE_NUM = 2
        GOTO 300
      ENDIF
  240 CONTINUE
      BYTE_MASK_BY_BYT(BYTE1) = PHI_MASK_BY_BYT(BYTE1)
      IF ( BYTE_MASK_BY_INT .NE. NO_BIT_SET ) THEN
        BYTE_NUM = 1 
        GOTO 300
      ENDIF
  250 CONTINUE
      PRINT *, ' JET_LIST_BUILDER ERROR - BYTE '
      TOT_ENTRY = 0
      GOTO 999 
C =============================================================================
C       Find non-zero-bit within non-zero-byte
C       ======================================
  300 CONTINUE
      BIT_MASK = SINGLE_BIT_MASK( BIT_NUM_IN_BYT ) 
      IF ( IAND ( BYTE_MASK_BY_INT, BIT_MASK ) .NE. 0 ) GOTO 400
C
      BIT_NUM_IN_BYT = BIT_NUM_IN_BYT - 1
      GOTO 300
C =============================================================================
C       Compute Tower's Entry mask and Entry address
C       ============================================
  400 CONTINUE
      BIT_NUM_IN_INT = 8 * (BYTE_NUM - 1) + BIT_NUM_IN_BYT
      PHI            = PHI_FOR_BIT ( BIT_NUM_IN_INT, LIST_TYPE )
C
C       the tag is a one dimension address of the tower cf D0 Note 967
      ENTRY_ADDR     = PHI_OFFSET( PHI ) + ETA_OFFSET( ETA_MAGN )
      IF ( ETA_SIGN .EQ. NEG_ETA ) ENTRY_ADDR = ENTRY_ADDR + SIGN_OFFSET
C
      ENTRY_MASK       = 0
      BYTE_MASK_BY_INT = 0 
C
C       The sptrg fired masks of all the refsets that this tower passed
C       are ORed to obtain the tower's entry mask.
C
      IF ( REF_0_PARTICIPATE .EQV. .TRUE. ) THEN
        ZEBRA_BY_INT            = IQ( ZI_PNTR + JP_RS_0 )
        BYTE_MASK_BY_BYT(BYTE1) = ZEBRA_BY_BYT(BYTE_NUM)
        IF ( IAND( BYTE_MASK_BY_INT, BIT_MASK ) .NE. 0 ) 
     &     ENTRY_MASK = IOR ( ENTRY_MASK, 
     &                        SPTRG_FIRED_FOR_REFSET ( REF_0 ) )
      ENDIF
C
      IF ( REF_1_PARTICIPATE .EQV. .TRUE. ) THEN
        ZEBRA_BY_INT            = IQ( ZI_PNTR + JP_RS_1 )
        BYTE_MASK_BY_BYT(BYTE1) = ZEBRA_BY_BYT(BYTE_NUM)
        IF ( IAND( BYTE_MASK_BY_INT, BIT_MASK ) .NE. 0 )
     &     ENTRY_MASK = IOR ( ENTRY_MASK,
     &                        SPTRG_FIRED_FOR_REFSET ( REF_1 ) )
      ENDIF
C
      IF ( REF_2_PARTICIPATE .EQV. .TRUE. ) THEN
        ZEBRA_BY_INT            = IQ( ZI_PNTR + JP_RS_2 )
        BYTE_MASK_BY_BYT(BYTE1) = ZEBRA_BY_BYT(BYTE_NUM)
        IF ( IAND( BYTE_MASK_BY_INT, BIT_MASK ) .NE. 0 )
     &     ENTRY_MASK = IOR ( ENTRY_MASK,
     &                        SPTRG_FIRED_FOR_REFSET ( REF_2 ) )
      ENDIF
C
      IF ( REF_3_PARTICIPATE .EQV. .TRUE. ) THEN
        ZEBRA_BY_INT            = IQ( ZI_PNTR + JP_RS_3 )
        BYTE_MASK_BY_BYT(BYTE1) = ZEBRA_BY_BYT(BYTE_NUM)
        IF ( IAND( BYTE_MASK_BY_INT, BIT_MASK ) .NE. 0 )
     &     ENTRY_MASK = IOR ( ENTRY_MASK,
     &                        SPTRG_FIRED_FOR_REFSET ( REF_3 ) )
      ENDIF
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
C       find zebra word holding tower's EM energy
      ZEBRA_BY_INT     = IQ ( ZI_EM + ENTRY_ADDR / 4 )
C
C       find ( entry_addr modulo 4 ) to get energy byte
      ZEBRA_BYTE       = IAND ( ENTRY_ADDR, 1+2 ) + 1
C
      ENERGY_BY_INT        = 0
      ENERGY_BY_BYT(BYTE1) = ZEBRA_BY_BYT (ZEBRA_BYTE)
      ENTRY_ENERGY         = ENERGY_BY_INT
C
      IF ( LIST_TYPE .EQ. TOT_ET_LIST ) THEN
        ZEBRA_BY_INT         = IQ ( ZI_HD + ENTRY_ADDR / 4 )
        ENERGY_BY_BYT(BYTE1) = ZEBRA_BY_BYT (ZEBRA_BYTE)
        ENTRY_ENERGY         = ENTRY_ENERGY + ENERGY_BY_INT
      END IF
C
C       The list must be ordered in decreasing EM (or TOT=EM+HD) energy. To
C       achieve this, we will TEMPORARILY force the upper two bytes (actually
C       only the lower 9 bits of the upper 2 bytes) of the entry tag (which
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
      BYTE_MASK_BY_BYT(BYTE1)   = PHI_MASK_BY_BYT(BYTE_NUM)
      BYTE_MASK_BY_INT          = IEOR ( BYTE_MASK_BY_INT, BIT_MASK )
      PHI_MASK_BY_BYT(BYTE_NUM) = BYTE_MASK_BY_BYT(BYTE1)
C
C       Globally check if all 32 PHIs were exhausted before going to next ETA,
C       or globally check same byte mask before looking for further bits, 
C       or go on to check next byte mask.
C
      IF ( PHI_MASK_BY_INT .EQ. NO_BIT_SET ) THEN
        IF ( ETA_SIGN .EQ. POS_ETA ) THEN 
          GOTO 110 
        ELSE
          GOTO 120 
        END IF
      ELSE
        IF ( BYTE_MASK_BY_INT .NE. NO_BIT_SET )THEN
          BIT_NUM_IN_BYT = BIT_NUM_IN_BYT - 1
          GOTO 300
        ELSE
          BIT_NUM_IN_BYT = 8
          GOTO (250, 240, 230, 220) BYTE_NUM
        END IF
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
      ENTRY L1UTIL_JET_LIST_BUILDER_FIRST(LIST_TYPE_FIRST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set the FIRST flag to .TRUE. for a particular type
C-     Jet List so that the Jet List Builder can be run multiple times on 
C-     the same event.  Normally the Jet List Builder keeps track of the 
C-     Event Number from the HEAD bank, to avoid recalculating a Jet List 
C-     multiple times for the same event.
C-
C-   Inputs  : LIST_TYPE   0 for EM Et Jet List, 
C-                         1 for TOT Et Jet List,
C-                         2 for Large Tile Jet List 
C-                            (the code for the Large Tile Jet List is in
C-                            L1UTIL_LT_JET_LIST_BUILDER_FIRST)
C-   Outputs : none
C-   Controls: none
C-
C-   Created  10-APR-1992   Philippe Laurens, Steven Klocek
C-   Updated  10-JUL-1993   Philippe Laurens - MSU L1 Trigger   
C-              Add Large Tile
C-   Updated  14-JAN-1994   Philippe Laurens - MSU L1 Trigger   
C-                           fix typo-bug, use LIST_TYPE_FIRST io. LIST_TYPE
C-
C----------------------------------------------------------------------
C       This routine has the code for the EM Et and Tot Et Jet Lists. The code
C       for the Large Tile Jet List is in L1UTIL_LT_JET_LIST_BUILDER.FOR
      IF ( LIST_TYPE_FIRST .EQ. LT_LIST ) THEN 
        CALL L1UTIL_LT_JET_LIST_BUILD_FIRST
      ELSE 
        FIRST(LIST_TYPE_FIRST) = .TRUE.
      END IF
      RETURN
C----------------------------------------------------------------------
      ENTRY L1UTIL_JET_LIST_BUILDER_STMASK(L1CRATE, LIST_TYPE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write to the TRGR bank the masks of Specific
C-     Triggers fired using each Reference Set. This entry point should be
C-     called immediately after building the Jet List
C-
C-   Inputs  : L1CRATE     ZEBRA pointer to the first word of the L1 data in
C-                         the TRGR bank 
C-                         (e.g. use GZFIND_CRATE ( 'TRGR', GZTRGR(), 11 )
C-             LIST_TYPE   0 for EM Et Jet List, 1 for TOT Et Jet List
C-                         This service doesn't exist for Large Tiles
C-   Outputs : common block output
C-   Controls: none
C-
C-   Created   4-DEC-1992 Philippe Laurens, Steven Klocek
C-
C----------------------------------------------------------------------
      IF (LIST_TYPE .EQ. EM_ET_LIST) THEN
        ZI_SPTRG_REF_MASK 
     &    = L1CRATE + TRGR_HEADER_LENGTH + I_SPTRG_EM_MASK
      ELSE
        ZI_SPTRG_REF_MASK
     &    = L1CRATE + TRGR_HEADER_LENGTH + I_SPTRG_TOT_MASK
      ENDIF
C
      DO REF_NUM = REF_0, REF_3
        IQ(ZI_SPTRG_REF_MASK+REF_NUM) = SPTRG_FIRED_FOR_REFSET(REF_NUM)
      END DO
C
      RETURN
      END
