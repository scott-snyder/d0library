c-----------------------------------------------------------------------------
c
c        This include file contains the calib_taker state data structure and
c        is used by the calib_taker task
c
c        14-March-1989   B. Gibbard
c
c-----------------------------------------------------------------------------

      INTEGER MAX_STATE
      PARAMETER (MAX_STATE = 8)

      INTEGER INITIALIZE,SET_TRIGGERS,READY_TO_BEGIN,AWAITING_STEP,
     1 DELETE_TRIGGERS,MODIFY_RUN,PROCESSING_END,READY_TO_TRIGGER,
     2 UNDEFINED
      PARAMETER (INITIALIZE = 1)
      PARAMETER (SET_TRIGGERS = 2)
      PARAMETER (READY_TO_BEGIN = 3)
      PARAMETER (AWAITING_STEP = 4)
      PARAMETER (DELETE_TRIGGERS = 5)
      PARAMETER (MODIFY_RUN = 6)
      PARAMETER (PROCESSING_END = 7)
      PARAMETER (UNDEFINED = MAX_STATE)

      INTEGER TAKER_STATE
      CHARACTER COMMAND(MAX_STATE)*16,ASYN_CMND*32
      LOGICAL RECORDING_ON,TAPE_LOCK,QUERY_BYPASS,
     1        LOGGER_END_REQUEST,FORCE_RDB_ENTRY
      COMMON /TAKE_CNTRL/TAKER_STATE,COMMAND,ASYN_CMND,
     1                   RECORDING_ON,TAPE_LOCK,QUERY_BYPASS,
     2                   LOGGER_END_REQUEST,FORCE_RDB_ENTRY

      DATA TAKER_STATE/1/,TAPE_LOCK/.TRUE./,QUERY_BYPASS/.FALSE./
      DATA FORCE_RDB_ENTRY/.FALSE./
      DATA
     1     COMMAND(INITIALIZE)      /'INITIALIZE      '/,
     2     COMMAND(SET_TRIGGERS)    /'SET TRIGGERS    '/,
     3     COMMAND(READY_TO_BEGIN)  /'READY TO BEGIN  '/,
     4     COMMAND(AWAITING_STEP)   /'AWAITING STEP   '/,
     5     COMMAND(DELETE_TRIGGERS) /'DELETE TRIGGERS '/,
     6     COMMAND(MODIFY_RUN)      /'MODIFY RUN      '/,
     7     COMMAND(PROCESSING_END)  /'PROCESSING END  '/,
     *     COMMAND(UNDEFINED)       /'UNDEFINED       '/
