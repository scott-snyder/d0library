      FUNCTION LV0REC()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Books and fills Level 0 Zebra banks
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  15-JUL-1992   Freedy Nang
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL LV0REC
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:LV0CON.INC/LIST'
C
      INTEGER LLV0H
      INTEGER CBUNCH
      INTEGER LKL0AD, LKL0SC, LKL0VX, LKPLV0
      INTEGER RAW_TIME(6,80)
      INTEGER BUNCH_ID(6,80)
      INTEGER RAW_CHARGE(6,80)
      INTEGER CORRECT_TIME(6,80)
      INTEGER GOODZ_SCALER(6,0:NL0_SCALERS)
      INTEGER NGOODZ_SCALER(6,0:NL0_SCALERS)
      INTEGER IBUNCH
      INTEGER FASTZ_ID(2),VERTEX_BOARD(2)
      INTEGER VERTEX_INFO(2,6,6,8)
      INTEGER IER
      INTEGER GZLV0H
      EXTERNAL GZLV0H
C
      LOGICAL FIRST
      LOGICAL DBG_LV0REC
      LOGICAL EZERROR
      LOGICAL PRODUC, PRODFL
      EXTERNAL EZERROR, PRODUC
C
      SAVE FIRST,DBG_LV0REC,PRODFL
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      LV0REC = .TRUE.
C
      IF(FIRST) THEN
        CALL EZPICK('LEVEL0_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('LEVEL0-no-rcp','LV0EVT',
     &                                 'LEVEL0_RCP not found.','W')
          GOTO 999
        ELSE
          CALL EZGET('DBG_LV0REC',DBG_LV0REC,IER)
          CALL EZRSET
        ENDIF
        PRODFL=PRODUC()
        IF ( PRODFL ) DBG_LV0REC=.FALSE.
        FIRST=.FALSE.
      ENDIF
C
C ****  Book LV0H bank if needed
C
      LLV0H=GZLV0H(0)
      IF ( LLV0H.LE.0 ) CALL BKLV0H(LLV0H)
      IF ( LLV0H.LE.0 ) GOTO 999
      CALL L0_COR_BUNCH(CBUNCH)
      IF ( CBUNCH.LT.1 .OR. CBUNCH.GT.6 ) THEN
        CALL ERRMSG('LEVEL0-skip-processing','LV0REC',
     &           'LEVEL0 data bad - skip processing this event','W')
        LV0REC=.FALSE.
        GOTO 999
      ENDIF
      IQ(LLV0H+1)=CBUNCH
C
C ****  Book and fill banks
C
      CALL L0_GET_ADC_DATA(RAW_TIME,BUNCH_ID,RAW_CHARGE,CORRECT_TIME)
      DO IBUNCH = 1 , 6        ! There are six bunches, hence six banks
C
C ****  Create new banks for every bunch
C
        IQ(LLV0H+2)=IQ(LLV0H+2)+1
        CALL BKL0AD(LKL0AD,IBUNCH)
        CALL FILL_L0AD(IBUNCH,RAW_TIME,BUNCH_ID,RAW_CHARGE,CORRECT_TIME)
      ENDDO
C
C ****  Book and fill L0SC banks
C
      CALL L0_GET_SCALER_DATA(GOODZ_SCALER,NGOODZ_SCALER)
      DO IBUNCH = 1 , 6        ! There are six bunches, hence six banks
        CALL BKL0SC(LKL0SC,IBUNCH)
        CALL FILL_L0SC(IBUNCH,GOODZ_SCALER,NGOODZ_SCALER)
      ENDDO
C
C ****  Book and fill L0VX banks
C
      CALL L0_GET_VERTEX_DATA(FASTZ_ID,VERTEX_BOARD,VERTEX_INFO)
      DO IBUNCH = 1 , 6        ! There are six bunches, hence six banks
        CALL BKL0VX(LKL0VX,IBUNCH)
        CALL FILL_L0VX(IBUNCH,FASTZ_ID,VERTEX_BOARD,VERTEX_INFO)
      ENDDO
C
C ****  Book and fill PLV0 bank
C
      CALL BKPLV0(LKPLV0)
      CALL FILL_PLV0()
C
C ****  Done
C
C      IF ( .NOT.PRODFL ) THEN
C        IF ( DBG_LV0REC ) THEN
CC
C          CALL PRLV0H(61,0,0,'ALL',3)
CC
C          CALL PRL0AD(61,0,CBUNCH,'ONE',3)
C          CALL PRL0SC(61,0,CBUNCH,'ONE',3)
C          CALL PRL0VX(61,0,CBUNCH,'ONE',3)
CC
C          CALL PRPLV0(61,0,0,'ALL',3)
CC
C        ENDIF
C      ENDIF
C
      LV0REC = .TRUE.
C----------------------------------------------------------------------
  999 RETURN
      END
