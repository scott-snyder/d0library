$!========================================================================
$!
$! Name      : DEFINE_HERE
$!
$! Purpose   : define a logical to the current default directory
$!
$! Arguments : P1   name of the logical
$!
$! Created  21-JAN-1993   James T. Linnemann
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   sdir = f$environment("DEFAULT")
$   DEFINE 'P1' 'sdir'
$EXIT:
$   EXIT
