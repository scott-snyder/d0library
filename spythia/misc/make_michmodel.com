$!========================================================================
$!
$! Name      : MAKE_MICHMODEL
$!
$! Purpose   : Build the Michigan SUGRA->MSSM model program
$!
$! Arguments : 
$!
$! Created  15-NOV-1995   Adam L. Lyon
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!  Link Opt
$   link/exe=michmodel.exe -
      d0$SPYTHIA:michmodel/library/include=(michsugra), -
      d0$general:general/library, -
      'cernp'
$!
$!  Link debug
$   link/debug/exe=deb_michmodel.exe -
      d0$SPYTHIA:deb_michmodel/library/include=(michsugra), -
      d0$general:deb_general/library, -
      'cernp'
$EXIT:
$   EXIT
