$!========================================================================
$!
$! Name      : TYPE_SPACE.COM
$!
$! Purpose   : copy a file and type it with a space in column  1; delete it
$!
$! Arguments : P1 file name
$!
$! Created  29-APR-1992   James T. Linnemann
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   OPEN/WRITE new_version type_space.tmp
$   OPEN/READ/END=CLOS old_version 'P1
$LOOP:
$   READ/END=CLOS old_version line
$   WRITE new_version " " + line
$   GOTO LOOP
$CLOS:
$   CLOSE new_version
$   CLOSE old_version
$   TYPE/PAGE type_space.tmp
$   DELETE type_space.tmp;*
$   EXIT
$EXIT:
$   CLOSE new_version
$   CLOSE old_version
$   EXIT
