$!========================================================================
$!
$! Name      : SETUP_FZSORT
$!
$! Purpose   : Setup FZSORT
$!
$! Arguments : None
$!
$! Created  11-SEP-1992   Herbert Greenlee
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
 FZSO*RT :== $D0$UTIL:FZSORT
 FZSC*RAMBLE :== $D0$UTIL:FZSCRAMBLE
$EXIT:
$   EXIT
