      SUBROUTINE L0_GET_ADC_DATA(RAW_TIME,BUNCH_ID,RAW_CHARGE,
     &                      CORRECT_TIME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fetch the Level 0 ADC data words from the
C-                         TRGR bank
C-
C-   Inputs  : none
C-   Outputs : RAW_TIME(bunch#1-6,channel#1-80)
C-             BUNCH_ID(bunch#1-6,channel#1-80)
C-             RAW_CHARGE(bunch#1-6,channel#1-80)
C-             CORRECT_TIME(bunch#1-6,channel#1-80)
C-   Controls: none
C-
C-   Created   1-JUN-1992   Jeffrey Bantly
C-   Updated  26-JAN-1994   Jeffrey Bantly  make Run 1a and Run 1b compatible
C-   Updated  12-JUL-1995   H. Greenlee - Changed initialization to execute
C-                                        for each event.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER FIRST_BUNCH,LAST_BUNCH
      INTEGER FIRST_CHAN,LAST_CHAN
      INTEGER BLOCK_WORD
      INTEGER BLOCK_TYPE
      INTEGER BLOCK_LENGTH
      INTEGER L0_ADC_DATA_LENGTH
      INTEGER IDATA,IADC
      INTEGER IBUNCH,ICHAN
      INTEGER ADC_LENGTH
      INTEGER ADC_WORD
      INTEGER RAW_TIME(6,80)
      INTEGER BUNCH_ID(6,80)
      INTEGER RAW_CHARGE(6,80)
      INTEGER CORRECT_TIME(6,80)
      INTEGER DATA_WORD(1000)
      INTEGER NDATA
      INTEGER DESCRIPTOR
      INTEGER IBITS
      INTEGER ERR
      INTEGER L0_MAX_NDATA
      INTEGER VERSION
C
      REAL    FORMAT
C
      CHARACTER*80 MESSG
C
      LOGICAL FIRST, MCDATA
      LOGICAL PRODUC, PRODFL, EZERROR
      EXTERNAL PRODUC, EZERROR
C
      SAVE FIRST,VERSION
      DATA FIRST/.TRUE./
      DATA L0_MAX_NDATA/1000/
C
C----------------------------------------------------------------------
C
C- Execute initialization block for each event for data - HBG
C
      FIRST = .TRUE.
      IF ( FIRST ) THEN
        FIRST=.FALSE.
        PRODFL = PRODUC()
        MCDATA = IQ(LHEAD+1) .GT. 1000
        CALL EZPICK('LEVEL0_RCP')
        IF ( EZERROR(ERR) ) THEN
          CALL ERRMSG('LEVEL0-no-rcp','L0EXPD',
     &                                 'LEVEL0_RCP not found.','F')
        ELSE
          CALL L0_GET_FORMAT(FORMAT)
          IF ( FORMAT.EQ.1.0.OR.FORMAT.EQ.1.5 ) THEN
            CALL EZGET('L0_ADC_1ADATA_LENGTH', L0_ADC_DATA_LENGTH, ERR)
            IF ( ERR.NE.0 ) L0_ADC_DATA_LENGTH=782
          ELSEIF ( FORMAT.EQ.2.0 ) THEN
            CALL EZGET('L0_ADC_1BDATA_LENGTH', L0_ADC_DATA_LENGTH, ERR)
            IF ( ERR.NE.0 ) L0_ADC_DATA_LENGTH=838
          ELSE
            L0_ADC_DATA_LENGTH=782
          ENDIF
          IF ( MCDATA ) THEN
            CALL L0_GET_VERSION(VERSION)
            IF ( VERSION.EQ.1 ) THEN
              CALL EZGET('L0_ADC_MC1ADATA_LENGTH', L0_ADC_DATA_LENGTH,
     &          ERR)
              IF (ERR.NE.0) L0_ADC_DATA_LENGTH=98
            ELSEIF ( VERSION.EQ.2 ) THEN
              CALL EZGET('L0_ADC_MC1BDATA_LENGTH', L0_ADC_DATA_LENGTH,
     &          ERR)
              IF (ERR.NE.0) L0_ADC_DATA_LENGTH=98
            ELSE
              L0_ADC_DATA_LENGTH=98
            ENDIF
          ENDIF
          CALL EZRSET
        ENDIF
      ENDIF
C
C  End of (re-)initialization.
C
      CALL VZERO(RAW_TIME,6*80)
      CALL VZERO(BUNCH_ID,6*80)
      CALL VZERO(RAW_CHARGE,6*80)
      CALL VZERO(CORRECT_TIME,6*80)
C
C  Fetch the Level 0 Crate of Data.
C
      CALL L0EXPD(1,NDATA,DATA_WORD)
      IF (NDATA.LE.0 .OR. NDATA.GT.L0_MAX_NDATA) GOTO 997
      IF ( NDATA .NE. L0_ADC_DATA_LENGTH ) GOTO 998
C
C  Search for ADC data blocks (type=1) only.
C
      IDATA = 1
   10 CONTINUE
      BLOCK_WORD = DATA_WORD(IDATA)
      BLOCK_TYPE   = IBITS(BLOCK_WORD,16,16)
      BLOCK_LENGTH = IBITS(BLOCK_WORD, 0,16)
      IF (BLOCK_TYPE.NE.1) GOTO 100
C
C  Decode ADC descriptor word for data block.
C
      DESCRIPTOR = DATA_WORD(IDATA+1)
      LAST_BUNCH  = IBITS(DESCRIPTOR,24,8)
      FIRST_BUNCH = IBITS(DESCRIPTOR,16,8)
      LAST_CHAN   = IBITS(DESCRIPTOR, 8,8)
      FIRST_CHAN  = IBITS(DESCRIPTOR, 0,8)
      IF ( FIRST_CHAN.GE.100 ) THEN
        FIRST_CHAN = FIRST_CHAN - 27
        LAST_CHAN = LAST_CHAN - 27
      ENDIF
      ADC_LENGTH = (LAST_BUNCH-FIRST_BUNCH+1)*(LAST_CHAN-FIRST_CHAN+1)
      IF ( ADC_LENGTH+IDATA .GT. NDATA ) GOTO 998
      IF ( ADC_LENGTH+2 .NE. BLOCK_LENGTH ) GOTO 996
C
C  Decode individual ADC data words in data block.
C
      IADC = IDATA + 1
      DO 101 IBUNCH = FIRST_BUNCH, LAST_BUNCH
        DO 102 ICHAN = FIRST_CHAN, LAST_CHAN
          IADC = IADC + 1
          ADC_WORD = DATA_WORD(IADC)
          CORRECT_TIME(IBUNCH,ICHAN)= IBITS(ADC_WORD,24, 8)
          RAW_CHARGE(IBUNCH,ICHAN)  = IBITS(ADC_WORD,14,10)
          BUNCH_ID(IBUNCH,ICHAN)    = IBITS(ADC_WORD,10, 3)
          RAW_TIME(IBUNCH,ICHAN)    = IBITS(ADC_WORD, 0,10)
          IF ( (BUNCH_ID(IBUNCH,ICHAN)+1).NE.IBUNCH ) GOTO 991
  102   CONTINUE
  101 CONTINUE
C
C   Skip to next data block if any.
C
  100 CONTINUE
      IDATA = IDATA + BLOCK_LENGTH
      IF ( IDATA.LE.NDATA ) GOTO 10
C
C   Done.
C
      GOTO 999
C
C   Error handling.
C
  990 CONTINUE
      WRITE(MESSG,*) 'Scaler bunch outside normal range =',FIRST_BUNCH,
     &  LAST_BUNCH
      IF (.NOT.PRODFL) CALL ERRMSG('LEVEL0-bad-scaler-bunch',
     &  'L0_GET_ADC_DATA',MESSG,'W')
      GOTO 999
  991 CONTINUE
      WRITE(MESSG,*) 'ADC Bunch_id not equal to descriptor bunch =',
     &  BUNCH_ID(IBUNCH,ICHAN)+1,IBUNCH
      IF (.NOT.PRODFL) CALL ERRMSG('LEVEL0-bunch-id-mismatch1',
     &  'L0_GET_ADC_DATA',MESSG,'W')
      GOTO 999
  996 CONTINUE
      WRITE(MESSG,*) 'ADC data block length doesnt match data length =',
     &  ADC_LENGTH+IDATA,BLOCK_LENGTH
      IF (.NOT.PRODFL) CALL ERRMSG('LEVEL0-adc-bad-blocklen',
     &  'L0_GET_ADC_DATA',MESSG,'W')
      GOTO 999
  997 CONTINUE
      WRITE(MESSG,*) 'Number data words exceeds maximum expected =',
     &  L0_MAX_NDATA,NDATA
      IF (.NOT.PRODFL) CALL ERRMSG('LEVEL0-more-than-max-data',
     &  'L0_GET_ADC_DATA',MESSG,'W')
      GOTO 999
  998 CONTINUE
      WRITE(MESSG,*) 'Number data words exceeds maximum expected =',
     &  L0_ADC_DATA_LENGTH,NDATA
      IF (.NOT.PRODFL) CALL ERRMSG('LEVEL0-more-than-adc-data',
     &  'L0_GET_ADC_DATA',MESSG,'W')
      GOTO 999
C
C----------------------------------------------------------------------
  999 RETURN
      END
