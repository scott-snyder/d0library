$!========================================================================
$!
$! Name      : CHANGE_FILTERS
$!
$! Purpose   : To switch TOOL configuration and FILTER mapping
$!             from one to another available default trigger sets
$!                          GRANNIS ---  DZERO
$!                               \       /
$!                                 BLAND
$!
$! Arguments : Specifies the Trigger Definition Set
$!             p1 = G(RANNIS), D(ZERO), or B(LAND)
$!           ( p2 = NOSKIP some dialog if called by SETUP_FILTER )
$!           ( p3 = destination directory when called by SETUP )
$! Created  16-OCT-1991   Daniel R. Claes
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$  SAY :== WRITE SYS$OUTPUT
$  IF p3.EQS.""
$    THEN DEF_DIR = F$ENVIRONMENT( "DEFAULT" )
$  ELSE
$    DEF_DIR = p3
$  ENDIF
$  IF p1.EQS."" THEN INQUIRE/NOPUNCT p1 "Trigger Definition Set?"
$START:
$  IF (p1.EQS."G") .OR. (P1.EQS."GRANNIS")
$      THEN P1 = "GRANNIS"
$      GOTO OK
$  ENDIF
$  IF (P1.EQS."D") .OR. (P1.EQS."D0") .OR. (P1.EQS."DZERO") 
$      THEN P1 = "DZERO"
$      GOTO OK
$  ENDIF
$  IF (P1.EQS."B") .OR. (P1.EQS. "BLAND" ) 
$      THEN P1 = "BLAND"
$      GOTO OK
$  ENDIF
$  SAY "Available sets:"
$  SAY "G(RANNIS), D(ZERO), B(LAND)"
$  INQUIRE/NOPUNCT P1 "Trigger Definition Set?"
$  GOTO START
$OK:
$  SAY " "
$!
$! Will copy new files unconditionally whenever user runs routine
$  IF p2.EQS."" THEN $GOTO NOfilt
$! But will ask the following from within SETUP_FILTER
$!
$  TEST = F$SEARCH("RUN_FILTER.DAT")
$  IF TEST.EQS."" 
$    THEN GOTO NOfilt
$  ENDIF
$  SAY "Found old RUN_FILTER.DAT in this directory."
$  SAY "This file defines the TRIGGER SET used previously."
$  ans = "N"
$  INQUIRE/NOPUNCT ans "Copy over new TRIGGER SET DEFINITION? [N]"
$  IF (ans.EQS."Y") .OR. (ans.EQS."y")
$    THEN GOTO NOfilt
$  ENDIF
$  GOTO FOUNDfilt
$NOfilt:
$  SAY "    Copying RUN_FILTER.DAT for the ''p1' Trigger Set from D0$LEVEL2"
$  COPY D0$LEVEL2$L2SIM:FILTER_'p1'.DAT         'DEF_DIR'RUN_FILTER.DAT
$  COPY D0$LEVEL2$L2SIM:RCP_'p1'.DAT            'DEF_DIR'RCP_0000001.DAT
$  COPY D0$LEVEL2$L2SIM:RCP_'p1'.ZDAT           'DEF_DIR'RCP_0000001.ZDAT
$!
$  COPY D0$LEVEL2$L2SIM:'p1'_MU_TRIGGER.INFO    'DEF_DIR'MU_TRIGGER.INFO
$  COPY D0$LEVEL2$L2SIM:'p1'_TRIGGER.INFO       'DEF_DIR'TRIGGER.INFO
$  COPY D0$LEVEL2$L2SIM:'p1'_TRIG_FILT.INFO     'DEF_DIR'TRIG_FILT_RUN.INFO
$!  PURGE MU_TRIGGER.INFO
$!  PURGE TRIGGER.INFO
$!  PURGE TRIG_FILT_RUN.INFO
$!  PURGE RCP_0000001.DAT
$!  PURGE RCP_0000001.ZDAT
$!  PURGE RUN_FILTER.DAT
$FOUNDfilt:
$!
$EXIT:
$   EXIT
