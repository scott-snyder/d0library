$!========================================================================
$!
$! Name      : REMOVE
$!
$! Purpose   : REMOVE from any .OLB's in the area (top version only)
$!
$! Arguments : P1   name of module
$!
$! Created  26-APR-1992   James T. Linnemann
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$ PIPE LIB/DELETE=('P1) *.OLB NAME
$EXIT:
$   EXIT
