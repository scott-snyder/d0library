$!========================================================================
$!
$! Name      : UPDATE_COOR_INFO
$!
$! Purpose   : RENAME ALL Coor_Sim output (L2SIM input files) to the
$!             D0$LEVEL2$ROOT:[L2SIM] area where CHANGE_FILTER
$!             copies it to the user from.
$!
$! Arguments : Specifies the Trigger Definition Set
$!             p1 = G(RANNIS), D(ZERO), or B(LAND)
$!
$! Created  03-DEC-1991   Daniel R. Claes
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$  SAY :== WRITE SYS$OUTPUT
$!                                              ! Check if COM file is being
$   RELtest = F$TRNLNM("D0$RELEASE")            ! run by release procedure
$   IF RELtest.EQS.""
$     THEN L2ROOT = F$TRNLNM("D0$LEVEL2$ROOT") - "LEVEL2.]" + "]"
$     DEFINE/NOLOG D0$RELEASE/TRANS=(CONCEALED,TERMINAL) 'L2ROOT'
$   ENDIF
$!
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
$  SAY "G(RANNIS),D(ZERO),B(LAND)"
$  INQUIRE/NOPUNCT P1 "Trigger Definition Set?"
$  GOTO START
$OK:
$!  RENAME LEVEL_2.INFO            D0$RELEASE:[LEVEL2.L2SIM]'p1'_LEVEL_2.INFO
$  RENAME LEVEL_2.INFO            D0$RELEASE:[LEVEL2.L2SIM]'p1'_LEVEL_2.INFO
$  RENAME MU_TRIGGER.INFO         D0$RELEASE:[LEVEL2.L2SIM]'p1'_MU_TRIGGER.INFO
$  RENAME NODE_CONFIG_CURRENT.INFO  -
       D0$RELEASE:[LEVEL2.L2SIM]NODE_CONFIG_'p1'.INFO
$  RENAME RCP_0000001.DAT         D0$RELEASE:[LEVEL2.L2SIM]RCP_'p1'.DAT
$  RENAME RCP_0000001.ZDAT        D0$RELEASE:[LEVEL2.L2SIM]RCP_'p1'.ZDAT
$  RENAME RCP_FILTERS_0000001.DAT D0$RELEASE:[LEVEL2.L2SIM]RCP_FILTERS_'p1'.DAT
$  RENAME RUN_FILTER.DAT          D0$RELEASE:[LEVEL2.L2SIM]FILTER_'p1'.DAT
$  RENAME TOKEN_RING.INFO         D0$RELEASE:[LEVEL2.L2SIM]'p1'_TOKEN_RING.INFO
$  RENAME TRIGGER.INFO            D0$RELEASE:[LEVEL2.L2SIM]'p1'_TRIGGER.INFO
$  RENAME TRIGGER.RES             D0$RELEASE:[LEVEL2.L2SIM]'p1'_TRIGGER.RES
$  RENAME TRIG_FILT_RUN.INFO      D0$RELEASE:[LEVEL2.L2SIM]'p1'_TRIG_FILT.INFO
$EXIT:
$  EXIT
