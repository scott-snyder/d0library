$!========================================================================
$!
$! Name      : SETUP_UNIX
$!
$! Purpose   : Define symbols for VMS-callable .COM files
$!
$! Arguments : 
$!
$! Created  13-JAN-1992   Herbert Greenlee
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$ check_unix == "@d0$unix:check_unix"
$ unix_node == "d0sgi5"
$ if p1.nes."" then unix_node == f$edit(p1,"lowercase")
$ unix_user == "guest"
$ if p2.nes."" then unix_user == f$edit(p2,"lowercase")
$EXIT:
$   EXIT
