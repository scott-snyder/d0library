$!========================================================================
$!
$! Name      : SETUP_SPYTHIA
$!
$! Purpose   : Setup logicals and symbols
$!
$! Arguments : 
$!
$! Created  15-NOV-1995   Adam L. Lyon
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$
$   say :== write sys$output
$   spythia :== "$d0$spythia:spythia.exe"
$   deb_spythia :== "$d0$spythia:deb_spythia.exe"
$   michmodel :== "$d0$spythia:michmodel.exe"
$   deb_michmodel :== "$d0$spythia:deb_michmodel.exe"
$   define spythia_rcp d0$spythia:spythia.rcp
$!
$   say "Setup SPYTHIA"
$   say "To run spythia, type SPYTHIA"
$   say "To run the Michigan model maker, type MICHMODEL"
$   say " "
$   say "Spythia is controlled by SPYTHIA_RCP"
$EXIT:
$   EXIT
