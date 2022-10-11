      SUBROUTINE MU_SUP_L1(TREG,CCT_LATCH,MU_RAW_CCT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate the muon trigger information for
C-                         level 1
C
C-
C-   Inputs  : TREG Flag regions to process
C-   Outputs : CCT_LATCH -  cct_latch word (see d0note 1587)
C-             MU_RAW_CCT - CCT bits to TRIG_MON. Two bits/region
C-
C-   Controls: None
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C--   I/O
      LOGICAL MU_RAW_CCT(16)
      LOGICAL TREG(7)
      INTEGER CCT_LATCH(7)
C--   Auxiliar variables
      LOGICAL JBCCT(2)
C--   Counters
      INTEGER TRIG_REG,IBIT,NEVENT
C--   D0 run and event number
      INTEGER NUMRUN,D0_EVENT_NUM
C--   Variables to choose specific trigger bit to process
      INTEGER ST_BIT(0:31),SPEC_TRIG
C--   skip event variables variables
      LOGICAL SHOULD_SKIP,SKIP
C--   Initialization
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      INTEGER IER
      CHARACTER*80 STRING
C-- To process events listed in ERR_EVT.DAT
      LOGICAL RDEVENT
      INTEGER NRUN,NEVT
C-- From MUSIM.RCP
      LOGICAL DOL1,IPRL1,IPRLATCH,IPRSTAT,IPRWCCT,IPRSWCCT,IPRSCCT
      LOGICAL IPRSWTRPL,IPRSTRPL,IPRCC,IPRISA,IPRMULT,EVTLIST
      INTEGER TRIGBIT
      DATA DOL1 /.FALSE./
      LOGICAL PRINTANY
      DATA PRINTANY /.FALSE./
C
C-------------------------------------------------------------------------------
C-- Initialization
C
      IF (FIRST) THEN
        FIRST=.FALSE.
C--     Read the to do flags
        CALL EZPICK('MUSIM_RCP')
        CALL EZERR(IER)     ! Check if error
        IF(IER.NE.0) THEN
          CALL EZGET_ERROR_TEXT(IER,STRING)
          CALL ERRMSG(' EZPICK ERR','MU_SUP_L1',STRING,'S')
          GOTO 999
        ENDIF
        CALL EZGET_l('DO_L1',DOL1,IER)   ! Flag to skip or not the L1 processing
        CALL EZGET_l('IPR_L1',IPRL1,IER)
        CALL EZGET_l('IPR_LATCH',IPRLATCH,IER)
        CALL EZGET_l('IPR_STAT',IPRSTAT,IER)
        CALL EZGET_l('IPR_WCCT',IPRWCCT,IER)
        CALL EZGET_l('IPR_SWCCT',IPRSWCCT,IER)
        CALL EZGET_l('IPR_SCCT',IPRSCCT,IER)
        CALL EZGET_l('IPR_SWTRPL',IPRSWTRPL,IER)
        CALL EZGET_l('IPR_STRPL',IPRSTRPL,IER)
        CALL EZGET_l('IPR_CC',IPRCC,IER)
        CALL EZGET_l('IPR_ISA',IPRISA,IER)
        CALL EZGET_l('IPR_MULT',IPRMULT,IER)
        CALL EZGET('TRIG_BIT',TRIGBIT,IER) ! specific trigger bit (1:31) to be
                                           ! processed
        CALL EZGET_l('EVT_LIST',EVTLIST,IER) ! To process only events listed in
                                           ! ERR_EVT.DAT
        CALL EZRSET()
        PRINTANY = IPRL1.OR.IPRLATCH.OR.IPRSTAT.OR.IPRWCCT.OR.
     &             IPRSWCCT.OR.IPRSCCT.OR.IPRSWTRPL.OR.IPRSTRPL.OR.
     &             IPRCC.OR.IPRISA.OR.IPRMULT

        IF(EVTLIST) THEN
          OPEN (50,FILE='ERR_EVT.DAT',STATUS='OLD')
          RDEVENT = .TRUE.
        ENDIF

        NEVENT = 0
      ENDIF

C
C-- Go to MUSIM directory in memory

      CALL HCDIR('//PAWC/MUSIM_L1',' ')

C--   Return if DOL1 is false
      IF(.NOT.DOL1) GOTO 999

C-- Initialize the SKIP event flag
      SHOULD_SKIP = .FALSE.
C-- Isajet events will be skipped dependisn on cuts set in MUSIM.RCP
C   Also fill in isajet generated events histos
      CALL ISAINFO(SHOULD_SKIP)
      IF (SHOULD_SKIP) GO TO 999

C-- Process specific bits, if trigbit = -1 do it all
      IF (TRIGBIT.NE.-1) THEN

C-- Choose a particular bit
        IF(LHEAD.NE.0) THEN
          SPEC_TRIG=IQ(LHEAD+11)
        ELSE
          RETURN
        ENDIF

        DO IBIT=31,0,-1
          SPEC_TRIG = ISHFTC( SPEC_TRIG , 1 , 32)
          ST_BIT(IBIT) = IAND  ( SPEC_TRIG , 1 )
        ENDDO
C--  Check the bit
        IF(ST_BIT(TRIGBIT).EQ.0)THEN
          SHOULD_SKIP = .TRUE.
          GO TO 999
        ENDIF

      ENDIF

      CALL EVNTID(NUMRUN,D0_EVENT_NUM)

C--  To process events form ERR_EVT.DAT
      IF (EVTLIST) THEN
        IF(RDEVENT) THEN
          READ(50, *,END=999)NRUN,NEVT
          RDEVENT=.FALSE.
        ENDIF
        IF(NUMRUN.NE.NRUN.OR.D0_EVENT_NUM.NE.NEVT) THEN
          SHOULD_SKIP = .TRUE.
          GO TO 999
        ENDIF

        RDEVENT=.TRUE.
      ENDIF

      NEVENT=NEVENT+1
      IF(PRINTANY)PRINT *,
     &  '  EVENT # : ',  NEVENT ,' D0 EVENT #: ',D0_EVENT_NUM

        DO IBIT = 1, 16
        MU_RAW_CCT(IBIT) = .FALSE.
      ENDDO

C-- Compute the muon triggers for all trigger regions.
      DO TRIG_REG = 1, 7
        CCT_LATCH(TRIG_REG) = 0
        DO IBIT = 1, 2
          JBCCT(IBIT) = .FALSE.
        ENDDO

        IF(TREG(TRIG_REG))THEN
          CALL MU_TRIG_CRATE_L1(TRIG_REG,JBCCT,CCT_LATCH(TRIG_REG))
C
C  Pack CCT bits to the TRIGMON. With the 7 trigger region scheme there
C   are 2 bits per region.

          DO IBIT=1,2
            MU_RAW_CCT((TRIG_REG-1)*2+IBIT+1) = JBCCT(IBIT)
          ENDDO

        ENDIF
      ENDDO                             !End loop over TRIG_REG = 1,7


  999 CONTINUE

      CALL HCDIR('//PAWC',' ')

      RETURN
C-------------------------------------------------------------------------
C-- Entry point to further skip histograming l1 stuff
      ENTRY SKIP_EVENT (SKIP)
      SKIP = SHOULD_SKIP
      RETURN

      END

