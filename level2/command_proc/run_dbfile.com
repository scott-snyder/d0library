$!========================================================================
$!
$! Name      : RUN_DBFILE
$!
$! Purpose   : Run DBFILE or MAP, depending on wheter you want to look at event
$! data or some other FZ file
$!
$! Arguments : 
$!
$! Created  26-APR-1992   James T. Linnemann
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$ SAY  :== WRITE SYS$OUTPUT
$  INQUIRE/NOPUNCT event " Do you want to look at event data? Y/N"
$ IF (event.EQS."Y").OR.(event.EQS."y")
$ THEN
$  SAY "running d0$util:MAP.EXE"
$  DEFINE/USER_MODE SYS$INPUT SYS$COMMAND
$  RNMP := $D0$UTIL:MAP.EXE
$  RNMP 
$ GOTO EXIT
$ ENDIF
$  SAY "running d0$util:DBFILE.EXE"
$  DEFINE/USER_MODE SYS$INPUT SYS$COMMAND
$  RUN D0$UTIL:DBFILE.EXE
$EXIT:
$   EXIT
