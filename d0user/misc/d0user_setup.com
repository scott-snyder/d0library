$!========================================================================
$!
$! Name      : D0USER_SETUP
$!
$! Purpose   : basic setup for D0USER program
$!
$! Arguments : none
$!
$! Created  19-APR-1991   Serban D. Protopopescu
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!
$ SAY :== write sys$output
$ @D0$D0USER:D0USER_MENUS
$ SAY "To run an event display you must @D0$D0USER:EVENT_DISPLAY"
$ SAY "before running D0USER"
$ D0USER     :== $'P1'_D0USER
$ DEB_D0USER :== $'P1'_DEB_D0USER
$!
$EXIT:
$   EXIT
