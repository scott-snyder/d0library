$!========================================================================
$!
$! Name      : SETUP_FZDIFF
$!
$! Purpose   : Setup FZDIFF
$!
$! Arguments : None
$!
$! Created  11-SEP-1992   Herbert Greenlee
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$ DEFINE FZDIFF_RCP D0$UTIL$FZDIFF:FZDIFF.RCP
$ FZD*IFF :== $D0$UTIL:FZDIFF
$EXIT:
$   EXIT
