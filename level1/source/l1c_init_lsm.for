      SUBROUTINE L1C_INIT_LSM()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get the file name for the LSM file and call the
C-     routines to load it.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   3-FEB-1992   Philippe Laurens, Steven Klocek
C-                      Moved this code here from L1_FW_AND_CT_INIT
C-   Updated   9-JUL-1993   Philippe Laurens - MSU L1 Trigger  
C-                      Add initialization of Large Tile Offsets LT_ZERRESP 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:L1SIM_CONTROL.PARAMS'
      INCLUDE 'D0$INC:L1SIM_CONTROL.INC'
      INCLUDE 'D0$INC:L1SIM_CONTROL_STRINGS.INC'
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$INC:LEVEL1_LOOKUP.INC'
      INCLUDE 'D0$INC:L1C_INTERMEDIATE_ENERGY.INC'
      INCLUDE 'D0$INC:L1C_REFSET_AND_COUNT_THRESHOLDS.INC'
C
      INTEGER  GT_USER
      PARAMETER (GT_USER = 523)           ! DEFAULT USER UNIT NUMBER
      INTEGER LUN, IER
      INTEGER LOOKUP_STATUS
      INTEGER QUANT, BIN
C
      INTEGER SIGN_ETA, LT_ETA, LT_PHI
      INTEGER TT_ETA, TT_PHI
      INTEGER BASE_TT_ETA, BASE_TT_PHI
      INTEGER EM_ZER, HD_ZER
C
C       Allocate a logical unit
C
      CALL GTUNIT (GT_USER , LUN ,IER)
      IF (IER .NE. 0) THEN
        CALL ERRMSG (' LUN ','GTUNIT',
     &    'Could not allocate logical unit','F')
        GOTO 999
      ENDIF
C
C
C     LEVEL1_LOOKUP common initialization
C     ===================================
C
      CALL INIT_LOOKUP_ROUTINES (LUN, 
     &  LOOKUP_TABLE_FILE_NAME(1:LOOKUP_TABLE_FILE_NAME_LENGTH), 
     &  LOOKUP_STATUS)
      IF(LOOKUP_STATUS.NE.1) THEN
        CALL ERRMSG('L1SIM BAD LOOKUP FILE', 
     &    'L1_FW_AND_CT_INIT',
     &    'Error reading in the Lookup Table file', 'F')
      ENDIF
C
C       
      IF(FORCE_VERTEX_CENTER .EQV. .TRUE.) THEN
        CALL INTMSG(
     &    ' All Level 0 Bins will use the center Level 1 Lookup Page')
        DO BIN = L0_BIN_MIN, L0_BIN_MAX
          DO QUANT = EM_ET_QUANT, PY_QUANT
            LUQ_PAGE_NUMBER(QUANT, BIN) = 0
          END DO
        END DO
      ENDIF
C
C     Compute Large Tile Offsets
C
      DO LT_ETA = LT_ETA_MIN, LT_ETA_MAX
        DO LT_PHI = LT_PHI_MIN, LT_PHI_MAX
          DO SIGN_ETA = POS_ETA, NEG_ETA
            LT_ZERESP( SIGN_ETA, LT_ETA, LT_PHI ) = 0
C
C       Sum the energy in the component Trigger Towers
C
            BASE_TT_ETA = (LT_ETA-1) * TT_ETA_PER_LT + 1
            BASE_TT_PHI = (LT_PHI-1) * TT_PHI_PER_LT + 1
            DO TT_PHI = BASE_TT_PHI , BASE_TT_PHI+TT_PHI_PER_LT-1
              DO TT_ETA = BASE_TT_ETA , BASE_TT_ETA+TT_ETA_PER_LT-1
C
                EM_ZER = ADC_ZERESP( SIGN_ETA, TT_ETA, TT_PHI, EM_TOWER)
                HD_ZER = ADC_ZERESP( SIGN_ETA, TT_ETA, TT_PHI, HD_TOWER)
                LT_ZERESP( SIGN_ETA, LT_ETA, LT_PHI ) 
     &            = LT_ZERESP( SIGN_ETA, LT_ETA, LT_PHI ) 
     &            + (EM_ZER + HD_ZER) / 2
C
              END DO
            END DO
C
          END DO
        END DO
      END DO
C
      CALL RLUNIT (GT_USER , LUN ,IER)
C----------------------------------------------------------------------
  999 RETURN
      END
