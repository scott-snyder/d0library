$!========================================================================
$!
$! Name      : GM_SHOW_SUMMARY
$!
$! Purpose   : Type file COPYCFG$ARCHIVE:RUN_xxxxxx.GM_SUM
$!
$! Arguments : P1   RunNumber
$!
$! Created   3-APR-1993   Harrison B. Prosper
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   
$!================================================
$!   Get run number
$!================================================
$   
$   IF P1 .EQS. ""
$   THEN
$       DIRECTORY/SINCE="TODAY-7-00:00:00.00" COPYCFG$ARCHIVE:RUN_*.GM_SUM
$       INQUIRE P1 "Use mouse to SELECT a File "
$       File = "COPYCFG$ARCHIVE:''P1'"
$   ELSE
$       RunNumber = P1
$       IF RunNumber .EQS. "" THEN GOTO EXIT
$       File    = F$SEARCH("COPYCFG$ARCHIVE:RUN_*''RunNumber'.GM_SUM")
$   ENDIF
$   
$   IF File .NES. ""
$   THEN
$       WRITE SYS$OUTPUT "  "
$       TYPE/NOPAGE 'File'
$       WRITE SYS$OUTPUT "  "
$   ELSE
$       WRITE SYS$OUTPUT "SHOWSUM: GM_SUM file NOT found"
$   ENDIF
$EXIT:
$   EXIT
