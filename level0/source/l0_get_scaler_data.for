      SUBROUTINE L0_GET_SCALER_DATA(GOODZ_SCALER,NGOODZ_SCALER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fetch the Level 0 SCALER data words from the
C-                         TRGR bank
C-
C-   Inputs  : none
C-   Outputs : GOODZ_SCALER(bunch#1-6,scaler grp#0-14)
C-             NGOODZ_SCALER(bunch#1-6,scaler grp#0-14)
C-   Controls: none
C-
C-   Created   1-JUN-1992   Jeffrey Bantly
C-   Updated  26-JAN-1994   Jeffrey Bantly  make Run 1a and Run 1b compatible 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:LV0CON.INC'
C
      INTEGER FIRST_BUNCH,LAST_BUNCH
      INTEGER FIRST_GROUP,LAST_GROUP
      INTEGER BLOCK_WORD
      INTEGER BLOCK_TYPE
      INTEGER BLOCK_LENGTH
      INTEGER DESCRIPTOR
      INTEGER SCALER_WORD
      INTEGER GOODZ_SCALER(6,0:NL0_SCALERS)
      INTEGER NGOODZ_SCALER(6,0:NL0_SCALERS)
      INTEGER NDATA
      INTEGER DATA_WORD(1000)
      INTEGER IDATA
      INTEGER IGOOD,IBUNCH,IGROUP,ISCALER
      INTEGER IBITS
      INTEGER ERR
      INTEGER L0_MAX_NDATA
C
      CHARACTER*80 MESSG
C
      LOGICAL FIRST, MCDATA
      LOGICAL PRODUC, PRODFL, EZERROR
      EXTERNAL PRODUC, EZERROR
C
      SAVE FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        CALL EZPICK('LEVEL0_RCP')
        IF ( EZERROR(ERR) ) THEN
          CALL ERRMSG('LEVEL0-no-rcp','L0EXPD',
     &                                 'LEVEL0_RCP not found.','W')
        ELSE
          CALL EZGET('L0_MAX_NDATA', L0_MAX_NDATA, ERR)
          IF ( ERR.NE.0 ) L0_MAX_NDATA=2000
          CALL EZRSET
        ENDIF
        MCDATA = IQ(LHEAD+1) .GT. 1000
        PRODFL = PRODUC()
        FIRST=.FALSE.
      ENDIF
C
C  Fetch the Level 0 Crate of Data.
C
      CALL L0EXPD(2,NDATA,DATA_WORD)
      IF (NDATA.LE.0 .OR. NDATA.GT.L0_MAX_NDATA) GOTO 997
C
C  Search for Scaler data blocks (type=2) only.
C
      IDATA = 1
   10 CONTINUE
      BLOCK_WORD = DATA_WORD(IDATA)
      BLOCK_TYPE   = IBITS(BLOCK_WORD,16,16)
      BLOCK_LENGTH = IBITS(BLOCK_WORD, 0,16)
      IF (BLOCK_TYPE.NE.2) GOTO 100
C
C  Decode Scaler descriptor word for data block.
C
      DESCRIPTOR = DATA_WORD(IDATA+1)
      LAST_BUNCH  = IBITS(DESCRIPTOR,24,8)
      FIRST_BUNCH = IBITS(DESCRIPTOR,16,8)
      LAST_GROUP  = IBITS(DESCRIPTOR, 8,8)
      FIRST_GROUP = IBITS(DESCRIPTOR, 0,8)
      IF ( FIRST_BUNCH.LT.1 ) GOTO 990
      IF ( LAST_BUNCH.GT.6 ) GOTO 990
      IF ( FIRST_GROUP.LT.0 ) GOTO 995
      IF ( LAST_GROUP.GT.NL0_SCALERS ) GOTO 995
C
C  Decode individual scaler data words in data block.
C
      ISCALER = IDATA + 1
      DO 101 IGOOD = 1, 2
        DO 102 IBUNCH = FIRST_BUNCH, LAST_BUNCH
          DO 103 IGROUP = FIRST_GROUP, LAST_GROUP
            ISCALER = ISCALER + 1
            IF (ISCALER.LE.NDATA) THEN
              SCALER_WORD = DATA_WORD(ISCALER)
            ELSE
              GOTO 998
            ENDIF
            IF ( IGOOD.EQ.1 ) THEN
              GOODZ_SCALER(IBUNCH,IGROUP)=SCALER_WORD
            ELSEIF ( IGOOD.EQ.2 ) THEN
              NGOODZ_SCALER(IBUNCH,IGROUP)=SCALER_WORD
            ENDIF
  103     CONTINUE
  102     CONTINUE
  101   CONTINUE
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
      IF (.NOT.PRODFL ) CALL ERRMSG('LEVEL0-bad-scaler-bunch',
     &  'L0_GET_SCALER_DATA',MESSG,'W')
      GOTO 999
  995 CONTINUE
      WRITE(MESSG,*) 'Scaler group outside normal range =',FIRST_GROUP,
     &  LAST_GROUP 
      IF (.NOT.PRODFL ) CALL ERRMSG('LEVEL0-bad-scaler-group',
     &  'L0_GET_SCALER_DATA',MESSG,'W')
      GOTO 999
  997 CONTINUE
      WRITE(MESSG,*) 'Number data words exceeds maximum expected =',
     &  L0_MAX_NDATA,NDATA
      IF (.NOT.PRODFL ) CALL ERRMSG('LEVEL0-more-than-max-data',
     &  'L0_GET_SCALER_DATA',MESSG,'W')
      GOTO 999
  998 CONTINUE
      WRITE(MESSG,*) 'Number data words exceeds maximum expected =',
     &  ISCALER,NDATA
      IF (.NOT.PRODFL ) CALL ERRMSG('LEVEL0-more-than-sclr-data',
     &  'L0_GET_SCALER_DATA',MESSG,'W')
      GOTO 999
C
C----------------------------------------------------------------------
  999 RETURN
      END
