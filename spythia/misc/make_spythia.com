$!========================================================================
$!
$! Name      : MAKE_SPYTHIA
$!             Usage:    @make_SPYTHIA
$! Purpose   : Make the SPYTHIA executable
$!
$! Arguments :
$!
$! Created  11-OCT-1995   Adam L. Lyon
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!
$!  Link Opt
$   link/exe=SPYTHIA.exe -
      d0$cernlib:pdflib/lib/include=(pdfset,structm),-
      d0$SPYTHIA:SPYTHIA/library/include=(run_spythia,pydata,ludata), -
      d0$isazeb:isazeb/library, -
      d0$general:general/library, -
      'cernp'
$!
$!  Link debug
$   link/debug/exe=deb_SPYTHIA.exe -
      d0$cernlib:pdflib/lib/include=(pdfset,structm),-
      d0$SPYTHIA:deb_SPYTHIA/library/include=(run_spythia,pydata,ludata), -
      d0$isazeb:deb_isazeb/library, -
      d0$physics_util:physics_util/include=(dbank,ezbank)/library,-
      d0$compack:deb_compack/library,-
      d0$general:deb_general/include=(dzsurv_zebcom)/library, -
      'cernp'
$EXIT:
$   EXIT
