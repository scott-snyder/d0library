$!========================================================================
$!
$! Name      : MAKE_DIR
$!
$! Purpose   : make 2 LEVELS of subdirectory 
$! Arguments : P1 new subdirectory P2 new sub-subdirectory
$!
$! Created  20-JUN-1991   James T. Linnemann
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$ CREATE/DIR [.'P1']
$ SET DEFAULT [.'P1']
$ CREATE/DIR [.'P2']
$ SET DEFAULT [-]
$EXIT:
$   EXIT
