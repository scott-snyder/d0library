$!========================================================================
$!
$! Name      : ALLOCATE_TAPE
$!
$! Purpose   : Allocate tape drive, if appropriate
$!
$! Arguments : P1 - Device name to allocate
$!                  Txxx => Generic tape device
$!                  DISK => IO to/from DISK
$!                  NL   => IO to/from NULL device
$!                  NONE => Not using, exit
$!                  other=> using tape drive
$!             P2 - IN/OUT - specify direction
$!                  Determines logical and symbol return
$!                  IN  -> TAPE_INPUT  (logical)/IN_TAPE (symbol)
$!                  OUT -> TAPE_OUTPUT (logical)/OUT_TAPE (symbol)
$!             P3 - If P1="DISK" then this is the directory spec
$!
$! Created  25-SEP-1991   A.M.Jonckheere
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!
$   IF p1 .eqs. "NONE" .or. p1 .eqs ""
$   THEN
$       IF p2 .eqs. "" then p2 = "IN"
$       WRITE SYS$OUTPUT "***** No ''p2'PUT device required"
$       goto EXIT
$   ENDIF

$   IF p1 .nes. "DISK" .and. p1 .nes. "NL"
$   THEN
$! Allocate tape
$       show time
$       WRITE SYS$OUTPUT "***** Allocate TAPE_''P2'PUT drive"
$allocate_loop:
$       gener = ""
$       IF f$extract(0,1,p1) .eqs. "T" then gener = "/generic"
$       allocate'gener' 'p1' tape_'p2'put
$       IF .NOT.$STATUS
$       THEN
$           wait 00:01:00
$           goto allocate_loop
$       ENDIF
$       'p2'_tape == f$trnlnm("tape_''p2'put")
$       a_tape = 'p2'_tape
$       WRITE SYS$OUTPUT "***** Tape ''a_tape' allocated"
$   ELSE
$       IF p1 .nes. "NL"
$       THEN
$           'p2'_tape == "''p3'"
$       ELSE
$           'p2'_tape == "NL:"
$       ENDIF
$       a_tape = 'p2'_tape
$       IF p2 .eqs. "IN"
$       THEN
$           tofrom = "from"
$       ELSE
$           tofrom = "to"
$       ENDIF
$       WRITE SYS$OUTPUT "***** ''P2'PUT ''tofrom' ''a_tape'"
$   ENDIF
$!
$EXIT:
$   EXIT
