$!========================================================================
$!
$! Name      : INSERT
$!
$! Purpose   : INSERT an .OBJ into any .OLB's in the area (top version only)
$!
$! Arguments : P1   name of module
$!
$! Created  26-APR-1992   James T. Linnemann
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$ PIPE LIB/REPLACE *.OLB NAME 'P1'
$EXIT:
$   EXIT
