      SUBROUTINE READ_LSO_MINIMAL(UNIT_NUM, FILE_NAME, ERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill the Level1_Lookup common blocks from an object
C-     file.
C-
C-   Inputs  : UNIT_NUM         The unit number to use for IO
C-             FILE_NAME        The name of the file to read from
C-             ERROR            Error status. Non-zero on error.
C-   Outputs : none
C-   Controls: none
C-
C-   Created  21-JUN-1991   MICHIGAN STATE UNIVERSITY, TRIGGER CONTROL SOFTWARE
C-   Updated  Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                              Fixed syntax error in non-VMS code. 
C-   Updated  10-MAR-1992   Philippe Laurens, Steven Klocek   
C-                      Removed unnecessary call to INZWRK().
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBWRK.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$INC:LEVEL1_LOOKUP.INC'
      INCLUDE 'D0$PARAMS:LSM_ZEB.PARAMS'
      INCLUDE 'D0$INC:LSM_ZEB_CHAR.INC'
      INCLUDE 'D0$INC:LSM_ZEB.INC'
C
      INTEGER UNIT_NUM, ERROR
      CHARACTER*(*) FILE_NAME
      INTEGER SIGN_ETA, MAGN_ETA, PHI, LOOKUP, PROM, TOWER, RZ
      INTEGER BIN, PAGE, LOW, INDEX
      INTEGER DUMMY
      INTEGER COUNT
      INTEGER LBANK, LDUMMY
      LOGICAL OK
C
      CALL INIT_COMMON_BLOCK_MINIMAL()
C
C       Don't use D0OPEN if we don't have to in order to keep memory usage to a
C       minimum on a VAX/ELN node.
C&IF VAXVMS,VAXELN
      OPEN (UNIT_NUM, FILE=FILE_NAME, STATUS='OLD', FORM='UNFORMATTED',
     &  READONLY, IOSTAT=ERROR, ERR=999)
C&ELSE
C&      ERROR = 1
C&      CALL D0OPEN(UNIT_NUM, FILE_NAME, 'IU', OK)
C&      IF (OK .NEQV. .TRUE.) THEN
C&        ERROR = 2
C&        GOTO 999
C&      ENDIF
C&ENDIF
      CALL FZFILE(UNIT_NUM, 0, 'I')
C
      CALL INIT_LSO_BANK_NAMES()
C
C       For each bank in the input file, read it in, copy the bank into the
C       array, and drop the bank.
C
      LDUMMY = BANK_NULL
      DO WHILE ( .TRUE. )
        CALL FZIN(UNIT_NUM,IDVWRK,LBANK,STAND_ALONE,' ',0,LDUMMY)
        IF ((IQUEST(1) .NE. 1) .AND. (IQUEST(1) .NE. 0)) GOTO 400
C
C       Process depending on which bank has been read in
C
C       L0_BIN_COVERAGE
C
        IF (IW(LBANK+LSO_BANK_ID) .EQ. BANK_LS0B) THEN
          CALL LSO_COPYWORDS_r(IW(LBANK+LSO_HEADER_SIZE+1),
     &      L0_BIN_COVERAGE(L0_BIN_MIN, Z_LOW),
     &      IW(LBANK+LSO_BANK_DLENGTH))
C
C       FIRST_LOOKUP_TYPE
C
        ELSEIF (IW(LBANK+LSO_BANK_ID) .EQ. BANK_LSFT) THEN
          CALL LSO_COPYWORDS(IW(LBANK+LSO_HEADER_SIZE+1),
     &      FIRST_LOOKUP_TYPE,
     &      IW(LBANK+LSO_BANK_DLENGTH))
C
C       SECOND_LOOKUP_TYPE
C
        ELSEIF (IW(LBANK+LSO_BANK_ID) .EQ. BANK_LSST) THEN
          CALL LSO_COPYWORDS(IW(LBANK+LSO_HEADER_SIZE+1),
     &      SECOND_LOOKUP_TYPE,
     &      IW(LBANK+LSO_BANK_DLENGTH))
C
C       LUQ_PAGE_INDEX
C
        ELSEIF (IW(LBANK+LSO_BANK_ID) .EQ. BANK_LSLI) THEN
          CALL LSO_COPYWORDS(IW(LBANK+LSO_HEADER_SIZE+1),
     &      LUQ_PAGE_INDEX(EM_ET_QUANT, PAGE_NUM_MIN),
     &      IW(LBANK+LSO_BANK_DLENGTH))
C
C       LUQ_PAGE_NUMBER
C
        ELSEIF (IW(LBANK+LSO_BANK_ID) .EQ. BANK_LSLN) THEN
          CALL LSO_COPYWORDS(IW(LBANK+LSO_HEADER_SIZE+1),
     &      LUQ_PAGE_NUMBER(EM_ET_QUANT,L0_BIN_MIN),
     &      IW(LBANK+LSO_BANK_DLENGTH))
C
C       GLOBAL_ADC_SCALE
C
        ELSEIF (IW(LBANK+LSO_BANK_ID) .EQ. BANK_LSGA) THEN
          CALL LSO_COPYWORDS_r(IW(LBANK+LSO_HEADER_SIZE+1),
     &      GLOBAL_ADC_SCALE,
     &      IW(LBANK+LSO_BANK_DLENGTH))
C
C       GLOBAL_ENERGY_SCALE
C
        ELSEIF (IW(LBANK+LSO_BANK_ID) .EQ. BANK_LSGE) THEN
          CALL LSO_COPYWORDS_r(IW(LBANK+LSO_HEADER_SIZE+1),
     &      GLOBAL_ENERGY_SCALE(EM_ET_QUANT),
     &      IW(LBANK+LSO_BANK_DLENGTH))
C
C       ELEC_NOISE_SIGMA
C
        ELSEIF (IW(LBANK+LSO_BANK_ID) .EQ. BANK_LSEN) THEN
          CALL LSO_COPYWORDS_r(IW(LBANK+LSO_HEADER_SIZE+1),
     &      ELEC_NOISE_SIGMA(POS_ETA, ETA_RANGE_MIN, PHI_MIN, EM_TOWER),
     &      IW(LBANK+LSO_BANK_DLENGTH))
C
C       INPUT_ENERGY_ERROR
C
        ELSEIF (IW(LBANK+LSO_BANK_ID) .EQ. BANK_LSEE) THEN
          CALL LSO_COPYWORDS_r(IW(LBANK+LSO_HEADER_SIZE+1),
     &      INPUT_ENERGY_ERROR(POS_ETA, ETA_RANGE_MIN, PHI_MIN,
     &      EM_TOWER),
     &      IW(LBANK+LSO_BANK_DLENGTH))
C
C       ANALOG_INPUT_SCALING
C
        ELSEIF (IW(LBANK+LSO_BANK_ID) .EQ. BANK_LSAI) THEN
          CALL LSO_COPYWORDS_r(IW(LBANK+LSO_HEADER_SIZE+1),
     &      ANALOG_INPUT_SCALING(POS_ETA, ETA_RANGE_MIN, PHI_MIN,
     &      EM_TOWER),
     &      IW(LBANK+LSO_BANK_DLENGTH))
C
C       ADC_CNT_VS_RAW_E
C
        ELSEIF (IW(LBANK+LSO_BANK_ID) .EQ. BANK_LSAC) THEN
          CALL LSO_COPYWORDS_r(IW(LBANK+LSO_HEADER_SIZE+1),
     &      ADC_CNT_VS_RAW_E(POS_ETA, NEG_ETA, PHI_MIN, EM_TOWER),
     &      IW(LBANK+LSO_BANK_DLENGTH))
C
C       DAC_BYTE
C
        ELSEIF (IW(LBANK+LSO_BANK_ID) .EQ. BANK_LSDB) THEN
          CALL LSO_COPYWORDS(IW(LBANK+LSO_HEADER_SIZE+1),
     &      DAC_BYTE(POS_ETA, ETA_RANGE_MIN, PHI_MIN, EM_TOWER),
     &      IW(LBANK+LSO_BANK_DLENGTH))
C
C       ADC_ZERESP
C
        ELSEIF (IW(LBANK+LSO_BANK_ID) .EQ. BANK_LSAZ) THEN
          CALL LSO_COPYWORDS(IW(LBANK+LSO_HEADER_SIZE+1),
     &      ADC_ZERESP(POS_ETA, ETA_RANGE_MIN, PHI_MIN, EM_TOWER),
     &      IW(LBANK+LSO_BANK_DLENGTH))
C
C       LUQ_LOCAL_RESCALING
C
        ELSEIF (IW(LBANK+LSO_BANK_ID) .EQ. BANK_LSLR) THEN
          CALL LSO_COPYWORDS(IW(LBANK+LSO_HEADER_SIZE+1),
     &      LUQ_LOCAL_RESCALING(POS_ETA, ETA_RANGE_MIN, EM_ET_QUANT),
     &      IW(LBANK+LSO_BANK_DLENGTH))
C
C       LOOKUP_ZERESP
C
        ELSEIF (IW(LBANK+LSO_BANK_ID) .EQ. BANK_LSLZ) THEN
          CALL LSO_COPYWORDS(IW(LBANK+LSO_HEADER_SIZE+1),
     &      LOOKUP_ZERESP(POS_ETA, ETA_RANGE_MIN, PHI_MIN, EM_ET_QUANT),
     &      IW(LBANK+LSO_BANK_DLENGTH))
C
C       PROM_CUT
C
        ELSEIF (IW(LBANK+LSO_BANK_ID) .EQ. BANK_LSPC) THEN
          DO INDEX = PAGE_INDEX_MIN, PAGE_INDEX_MAX
            CALL LSO_COPYWORDS(IW(LBANK+LSO_HEADER_SIZE+1
     &          + (INDEX-PAGE_INDEX_MIN)
     &            *(IW(LBANK+LSO_BANK_DLENGTH)/
     &              (PAGE_INDEX_MAX-PAGE_INDEX_MIN+1))),
     &        PROM_CUT(POS_ETA, ETA_RANGE_MIN, PHI_MIN,
     &        IW(LBANK+LSO_BANK_SUBID), INDEX),
     &        IW(LBANK+LSO_BANK_DLENGTH)/
     &          (PAGE_INDEX_MAX-PAGE_INDEX_MIN+1))
          END DO
C
C       PROM_SLOPE
C
        ELSEIF (IW(LBANK+LSO_BANK_ID) .EQ. BANK_LSPS) THEN
          DO INDEX = PAGE_INDEX_MIN, PAGE_INDEX_MAX
            CALL LSO_COPYWORDS_r(IW(LBANK+LSO_HEADER_SIZE+1
     &          + (INDEX-PAGE_INDEX_MIN)
     &            *(IW(LBANK+LSO_BANK_DLENGTH)/
     &              (PAGE_INDEX_MAX-PAGE_INDEX_MIN+1))),
     &        PROM_SLOPE(POS_ETA, ETA_RANGE_MIN, PHI_MIN,
     &        IW(LBANK+LSO_BANK_SUBID), INDEX),
     &        IW(LBANK+LSO_BANK_DLENGTH)/
     &          (PAGE_INDEX_MAX-PAGE_INDEX_MIN+1))
          END DO
C
C       TREE_OFFSET
C
        ELSEIF (IW(LBANK+LSO_BANK_ID) .EQ. BANK_LSTO) THEN
          CALL LSO_COPYWORDS(IW(LBANK+LSO_HEADER_SIZE+1),
     &      TREE_OFFSET(EM_ET_QUANT), 
     &      IW(LBANK+LSO_BANK_DLENGTH))
C
C       PAGE_Z_NOM
C
        ELSEIF (IW(LBANK+LSO_BANK_ID) .EQ. BANK_LSZN) THEN
C
C       Do nothing
C
C
C       ELEC_NOISE_CUT_FACT
C
        ELSEIF (IW(LBANK+LSO_BANK_ID) .EQ. BANK_LSEC) THEN
C
C       Do nothing
C
C
C       TOWER_RZ_COORD
C
        ELSEIF (IW(LBANK+LSO_BANK_ID) .EQ. BANK_LSRZ) THEN
C
C       Do nothing
C
C
C       TOWER_PHI_COORD
C
        ELSEIF (IW(LBANK+LSO_BANK_ID) .EQ. BANK_LSPH) THEN
C
C       Do nothing
C
C
C       FINAL_FITTING
C
        ELSEIF (IW(LBANK+LSO_BANK_ID) .EQ. BANK_LSFF) THEN
C
C       Do nothing
C
C
C       TRANSV_ENERGY_CUT
C
        ELSEIF (IW(LBANK+LSO_BANK_ID) .EQ. BANK_LSTE) THEN
C
C       Do nothing
C
        ENDIF
C
        CALL MZDROP(IDVWRK, LBANK, ' ')
C
      END DO
C
  400 CONTINUE
      IF (IQUEST(1) .LT. 0) THEN
        ERROR = -1
      ELSE
        ERROR = 0
      ENDIF
      CALL FZENDI(UNIT_NUM,'T')
      CLOSE (UNIT_NUM)
C----------------------------------------------------------------------
  999 RETURN
      END
