$!------------------------------------------------
$!
$! Name      : BUILD_ENTRY_POINTS
$!
$! Purpose   : To build the entry point list
$!
$! Arguments : Library
$!
$! Created   1-FEB-1990   D0 Library
$! Updated   3-MAR-1992   D0 Library
$!
$!------------------------------------------------
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$
$!?????$ libtest util
$ edit/tpu/nodisplay -
      /comm=d0$util$entry_points:build_entry_list
$EXIT:
$   EXIT
