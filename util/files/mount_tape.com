$!========================================================================
$!
$! Name      : MOUNT_TAPE
$!
$! Purpose   : Allocate and mount input or output tapes
$!
$! Arguments : P1 - IN/OUTDEV    = device name, can be a logical name,
$!                                  or a real *physical* drive.
$!                                  DISK, NL or "" causes an exit
$!             P2 - IN/OUT       = direction of IO
$!             P3 - IN/OUTVSN    = tape visual label
$!             P4 - IN/OUTLABEL  = tape label or disk directory 
$!             P5 - INIT_OUT     = init flag for tapes
$!
$! NOTE: If Logical device TAPE_INPUT or TAPE_OUTPUT is not define 
$!       (via ALLOCATE for example), it is *assumed* that P1 is a real 
$!       physical device that can be mounted/inited etc.
$!
$! Created  24-SEP-1991   A.M.Jonckheere
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!
$   IF p2 .eqs. "OUT" 
$   THEN
$       r_w = "WRITE"
$       swit = "/WRITE/BLOCK=32768"
$   ELSE
$       r_w = "READ"
$       swit = "/nowrite"
$   ENDIF
$!
$   IF p1 .eqs. "DISK" .or. p1 .eqs. "NL" .or. p1 .eqs. "" then goto EXIT
$!
$   if f$locate(":",p1) .eq. f$length(p1) then p1 = p1 + ":"
$   sh sym p1
$!
$   CMNT = "Please mount VSN "+"''P3'"+" for ''r_w'"
$   WRITE SYS$OUTPUT "***** Mount ''P2'PUT tape ''P3'/''p4'"
$   MOUNT/FOR/NOAUTO'swit'/COMMENT="''CMNT'" 'p1'
$   DISMOUNT/NOUNLOAD 'p1'
$   IF p2 .eqs. "OUT" .and. p5 .eqs. "INIT"
$   THEN
$       WRITE SYS$OUTPUT "***** INIT tape ''P4'"
$       INITIALIZE/override=expiration 'p1' 'P4'
$       IF f$locate("MKB","''p1'") .lt. f$length(p1)
$       THEN
$           OPEN/WRITE DUMMY DUMMY.DUM
$           WRITE DUMMY -
                  " This is a dummy file required on a 3100"
$           WRITE DUMMY -
                  " to provide a *LONG* file mark."
$           CLOSE DUMMY
$           BACKUP DUMMY.DUM 'p1'DUMMY.BCK/LABEL='P4'
$           DELETE/NOCONF/NOLOG DUMMY.DUM;*
$           dismount/nounload 'p1'
$           WRITE SYS$OUTPUT "***** Init complete"
$       ENDIF
$   ENDIF
$   MOUNT/NOASSIST/NOAUTO'swit' 'p1' 'P4'
$   WRITE SYS$OUTPUT "***** Mount complete"
$!
$EXIT:
$   EXIT
