$!========================================================================
$!
$! Name      : MAKE_PYTHIA
$!             Usage:    @make_PYTHIA [debug]
$! Purpose   : Make the PYTHIA executable
$!
$! Arguments :
$!
$! Created  11-OCT-1995   Adam L. Lyon and John Krane
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!
$   if p1 .nes. ""
$   then
$     cdeb = "/noopt/debug"
$     deb  = "deb_"
$     swit = "/debug"
$   else
$     cdeb = ""
$     deb  = ""
$     swit = ""
$   endif
$
$!  Link Opt
$   link/exe=PYTHIA.exe -
      d0$PYTHIA:PYTHIA/library/include=(test_PYTHIA,pydata,ludata), -
      d0$isazeb:isazeb/library, -
      d0$general:general/library, -
      d0$cernlib:pdflib/lib, -
      'cernp',-
      sys$input:/opt
      STACK=50
      ISD_MAX=350
      PSECT = PAWC,PAGE
$!
$!  Link debug
$   link/debug/exe=deb_PYTHIA.exe -
      d0$PYTHIA:deb_PYTHIA/library/include=(test_PYTHIA,pydata,ludata), -
      d0$isazeb:deb_isazeb/library, -
      d0$physics_util:physics_util/include=(dbank,ezbank)/library,-
      d0$compack:deb_compack/library,-
      d0$general:deb_general/include=(dzsurv_zebcom)/library, -
      d0$cernlib:pdflib/lib, -
      'cernp',-
      sys$input:/opt
      STACK=50
      ISD_MAX=350
      PSECT = PAWC,PAGE
$EXIT:
$   EXIT
