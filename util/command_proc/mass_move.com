$!========================================================================
$!
$! Name      : MASS_MOVE
$!
$! Purpose   : move a whole subdirectory tree
$!
$!              create the subdirectory structure first with make_dir.com
$! Arguments : P1 top source directory
$!             P2 top target directory
$!
$! Created  26-SEP-1991   James T. Linnemann
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$ source = "''P1'" - "]" + "...]*.*
$ target = "''P2'" - "]" + "...]*.*
$ COPY/LOG/EXCLUDE=(*.DIR) 'source' 'target'
$EXIT:
$   EXIT
