c-----------------------------------------------------------------------------
c
c        This include file contains the taker state data structure and
c        is used by version 2 of the taker task
c
c        13-Sept-1988   B. Gibbard
c
c-----------------------------------------------------------------------------

      INTEGER MAX_STATE
      PARAMETER (MAX_STATE = 8)

      INTEGER INITIALIZE,SET_TRIGGERS,READY_TO_BEGIN,AWAITING_END,
     1 DELETE_TRIGGERS,MODIFY_RUN,MODIFY_PARAMS,UNDEFINED
      PARAMETER (INITIALIZE = 1)
      PARAMETER (SET_TRIGGERS = 2)
      PARAMETER (READY_TO_BEGIN = 3)
      PARAMETER (AWAITING_END = 4)
      PARAMETER (DELETE_TRIGGERS = 5)
      PARAMETER (MODIFY_RUN = 6)
      PARAMETER (MODIFY_PARAMS = 7)
      PARAMETER (UNDEFINED = MAX_STATE)

      INTEGER TAKER_STATE
      CHARACTER COMMAND(MAX_STATE)*16,ASYN_CMND*32
      COMMON /TAKE_CNTRL/TAKER_STATE,COMMAND,ASYN_CMND

      DATA TAKER_STATE/1/
      DATA
     1     COMMAND(INITIALIZE)      /'INITIALIZE      '/,
     2     COMMAND(SET_TRIGGERS)    /'SET TRIGGERS    '/,
     3     COMMAND(READY_TO_BEGIN)  /'READY TO BEGIN  '/,
     4     COMMAND(AWAITING_END)    /'AWAITING END    '/,
     5     COMMAND(DELETE_TRIGGERS) /'DELETE TRIGGERS '/,
     6     COMMAND(MODIFY_RUN)      /'MODIFY RUN      '/,
     7     COMMAND(MODIFY_PARAMS)   /'MODIFY PARAMS   '/,
     *     COMMAND(UNDEFINED)       /'UNDEFINED       '/
