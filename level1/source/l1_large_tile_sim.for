      SUBROUTINE L1_LARGE_TILE_SIM( NUM_ANDOR_USED, ANDOR_STATES,
     &                          ANDOR_INDICES)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Large Tile simulation for L1SIM.
C-
C-   Inputs  : none
C-   Outputs : NUM_ANDOR_USED   The number of Andor Terms this routine is
C-                              returning.
C-             ANDOR_STATES     The state of each returned Andor Term.
C-             ANDOR_INDICES    The corresponding Andor Term number for 
C-                              each returned Andor Term.
C-   Controls: none
C-
C-   Created  11-FEB-1992   Philippe Laurens, Steven Klocek   
C-                            Special extension to L1SIM using USER_TERMS hooks
C-   Updated  28-JUN-1993   Philippe Laurens - MSU L1 Trigger  
C-                            Made integral part of L1SIM, with update to match
C-                            final specification.
C-                            Replace use of FADC byte with PROM lookup Page 8
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$INC:LEVEL1_LOOKUP.INC'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS'
      INCLUDE 'D0$INC:L1DBB_DATA_BLOCK.INC'
      INCLUDE 'D0$INC:L1C_REFSET_AND_COUNT_THRESHOLDS.INC'
      INCLUDE 'D0$INC:L1C_GLOBAL_RESULTS.INC'
      INCLUDE 'D0$INC:L1C_EVENT.INC'
      INCLUDE 'D0$INC:L1C_INTERMEDIATE_ENERGY.INC'
C
      INTEGER NUM_ANDOR_USED
      LOGICAL ANDOR_STATES(NUM_ANDOR)
      INTEGER ANDOR_INDICES(NUM_ANDOR)
C
      LOGICAL L1UTIL_PICK_RESOURCE_RCP
      EXTERNAL L1UTIL_PICK_RESOURCE_RCP
C
      INTEGER LT_ANDOR_INDEX(LT_REF_MIN:LT_REF_MAX,1:3)
      INTEGER IER
      INTEGER RCP_OK
      PARAMETER (RCP_OK = 0)
      INTEGER OUT_OF_RANGE
      PARAMETER (OUT_OF_RANGE = 1000)
      CHARACTER*16 ANDOR_NAME
      CHARACTER*72 STRING
      INTEGER TYPE, TOTAL, LENGTH_STRING
      LOGICAL DO_CUT, BILATERAL_CUT
      INTEGER CUT_COUNTS
      REAL    CUT_VALUE
C
      INTEGER SIGN_ETA, LT_ETA, LT_PHI, REF
      INTEGER OFFSET, BITNUM
      INTEGER TT_ETA, TT_PHI
      INTEGER BASE_TT_ETA, BASE_TT_PHI
C
      INTEGER EM_BYTE, HD_BYTE, LT_BYTE
C
      INTEGER THRESH 
C
      LOGICAL FIRST
      SAVE    FIRST, DO_CUT, BILATERAL_CUT, CUT_COUNTS
      DATA    FIRST / .TRUE. /
C
C-----------------------------------------------------------------------
C
      NUM_ANDOR_USED = 0
C
      IF (FIRST .EQV. .TRUE.) THEN
        IF (L1UTIL_PICK_RESOURCE_RCP() .EQV. .FALSE.) GOTO 999
C
        DO REF = LT_REF_MIN, LT_REF_MAX 
          DO THRESH = 1, 3
            WRITE ( ANDOR_NAME, 1000 ) (REF-LT_REF_MIN), THRESH
 1000       FORMAT ( 'L1C_LT_CNT_', I1, '_GE', I1 )
            CALL EZGET_i( ANDOR_NAME, 
     &                  LT_ANDOR_INDEX( REF, THRESH ), IER)
            IF (IER .NE. RCP_OK) THEN
              LT_ANDOR_INDEX( REF, THRESH ) = OUT_OF_RANGE
            ENDIF
          ENDDO
        ENDDO
C
        CALL EZRSET()
C
C       Pick up L1SIM.RCP parameters
        CALL L1UTIL_PICK_L1SIM_RCP
C
C       The following implements an undocumented feature for special studies 
        CALL EZGET ( 'DO_TT_LOW_ENERGY_CUT_FOR_LT', DO_CUT, IER ) 
        IF (IER .NE. RCP_OK) DO_CUT = .FALSE.
C
        CALL EZGET ( 'DO_BILATERAL_CUT_FOR_LT', BILATERAL_CUT, IER ) 
        IF (IER .NE. RCP_OK) BILATERAL_CUT = .FALSE.
C
        CALL EZGETR ( 'TT_LOW_ENERGY_CUT_FOR_LT', CUT_VALUE, 
     &                TYPE, TOTAL, STRING, LENGTH_STRING ) 
        CALL EZERR(IER)
        IF (IER .EQ.  RCP_OK) THEN
C       Did SRCP think it was returning a REAL or INTEGER?
          IF ((TYPE .NE. 2) .AND. (TYPE .NE. 3)) THEN
            CALL EZGSET( 'TT_LOW_ENERGY_CUT_FOR_LT', CUT_COUNTS, 1)
            CUT_VALUE = FLOAT(CUT_COUNTS)
          ENDIF
C       Translate to Counts, 1/2 a GeV per count        
          CUT_COUNTS = NINT(CUT_VALUE/0.5)
        ELSE
          CUT_VALUE = 0.
          CUT_COUNTS = 0
        ENDIF
C
        IF ( DO_CUT .EQV. .TRUE. ) THEN 
          IF ( BILATERAL_CUT .EQV. .TRUE. ) THEN
            WRITE ( STRING, 2000 ) 'bilateral', CUT_VALUE
          ELSE 
            WRITE ( STRING, 2000 ) 'unilateral', CUT_VALUE
          END IF
 2000     FORMAT ( ' A ', A, ' Low Energy Cut of ', F5.2, ' GeV' )
          CALL INTMSG( STRING )
          CALL INTMSG( ' will be applied to Trigger Tower Energies '
     &              // 'summed into Large Tiles' ) 
        ENDIF
C
        CALL EZRSET()
C        
        FIRST = .FALSE.
      ENDIF
      
C
C     Clear the Large Tile Counts and the Mask of Jet Pattern 
C
      DO REF = LT_REF_MIN, LT_REF_MAX
        LT_COUNT(REF) = 0
      END DO
C
      DO OFFSET = 0 , LRG_TILE_JET_PTTRN_MSK_L/2 - 1 
        LVL1_DATA_BLOCK( (LRG_TILE_JET_PTTRN_MSK+1)/2 + OFFSET ) = 0
      ENDDO     
C
C       Find the energy deposited in each Large Tile
C
      DO LT_ETA = LT_ETA_MIN, LT_ETA_MAX
        DO LT_PHI = LT_PHI_MIN, LT_PHI_MAX
          DO SIGN_ETA = POS_ETA, NEG_ETA
C
C       Clear the array element
            LT_ENERGY( SIGN_ETA, LT_ETA, LT_PHI) = 0
C
C       Sum the energy in the component Trigger Towers
C
            BASE_TT_ETA = (LT_ETA-1) * TT_ETA_PER_LT + 1
            BASE_TT_PHI = (LT_PHI-1) * TT_PHI_PER_LT + 1
            DO TT_PHI = BASE_TT_PHI , BASE_TT_PHI+TT_PHI_PER_LT-1
              DO TT_ETA = BASE_TT_ETA , BASE_TT_ETA+TT_ETA_PER_LT-1
C       
                EM_BYTE = FADC_BYTE( SIGN_ETA, TT_ETA, TT_PHI, EM_TOWER)
                HD_BYTE = FADC_BYTE( SIGN_ETA, TT_ETA, TT_PHI, HD_TOWER)
C       
C       EM and Hd are summed and the LSB is dropped before the Px/Py 
C       and Large Tile PROM Lookup
C       
                LT_BYTE = (EM_BYTE + HD_BYTE) / 2
C       
C       Apply Low Energy Cut on Trigger Tower Energies                 
C       This is an undocumented feature for special studies 
C       
                IF ( DO_CUT .EQV. .TRUE. ) THEN
                  IF ( BILATERAL_CUT .EQV. .TRUE. ) THEN 
                    IF (IABS(LT_BYTE-8) .LT. CUT_COUNTS) LT_BYTE = 8
                  ELSE
                    IF ( (LT_BYTE-8) .LT. CUT_COUNTS) LT_BYTE = 8
                  ENDIF
                ENDIF
C       
C       Add the contribution from this Trigger Tower to the Large Tile Energy 
C       
                LT_ENERGY( SIGN_ETA, LT_ETA, LT_PHI) =
     &                 LT_ENERGY( SIGN_ETA, LT_ETA, LT_PHI) + LT_BYTE
              END DO
            END DO
C
C     Count the number of Large Tiles clearing the Reference Sets
C     and set the bit mask of Large Tile Jet Pattern.
C
            DO REF = LT_REF_MIN, LT_REF_MAX
C
C             note that LT_ENERGY has the energy + offset, which is
C             appropriate for the following threshold comparison              
              IF ( LT_ENERGY( SIGN_ETA, LT_ETA, LT_PHI)   
     &        .GE. LT_THRSHLD( SIGN_ETA, LT_ETA, LT_PHI, REF ) ) 
     &        THEN
C
C               Increment Large Tile Count
C
                LT_COUNT(REF) = LT_COUNT(REF) + 1
C
C               Find the address where this tower's jet pattern mask is located 
C               There are 5 16-bit words per reference set (cf. D0 note 967) so
C               that odd and even reference sets do not line up in the same way.
C               A simple approach is to compute the bit offset from the
C               beginning of the pattern and then derive from it the longword
C               and bit position. 
C       
              BITNUM = LT_ETA_MAX * (REF - LT_REF_MIN) * 16 
     &               + ( LT_ETA - 1 ) * 16
     &               + SIGN_ETA * 4 
     &               + LT_PHI
              OFFSET = ( BITNUM - 1 ) / 32
              BITNUM  = BITNUM - 32 * OFFSET 
              CALL SBIT1( LVL1_DATA_BLOCK( (LRG_TILE_JET_PTTRN_MSK+1)/2
     &                                   + OFFSET ), 
     &                    BITNUM )
              ENDIF
            END DO
          END DO
        END DO
      END DO
C
C       Set Andor Terms to tell how many Large Tiles cleared
C       while checking each Andor Term to see if it was defined.
C
      DO REF = LT_REF_MIN, LT_REF_MAX
        DO THRESH = 1, 3
          IF ( (LT_ANDOR_INDEX( REF, THRESH ) .LE. ANDOR_NUM_MAX) .AND.
     &         (LT_ANDOR_INDEX( REF, THRESH ) .GE. ANDOR_NUM_MIN) ) THEN
            NUM_ANDOR_USED = NUM_ANDOR_USED + 1
            ANDOR_INDICES(NUM_ANDOR_USED) = LT_ANDOR_INDEX( REF, THRESH)
            ANDOR_STATES(NUM_ANDOR_USED) = .FALSE.
            IF ( LT_COUNT( REF ) .GE. THRESH ) 
     &      ANDOR_STATES(NUM_ANDOR_USED) = .TRUE.
          ENDIF
        END DO
      END DO
C
C----------------------------------------------------------------------
  999 RETURN
      END
