 $!========================================================================
$!
$! Name      : LO.COM
$!
$! Purpose   : To Link the Leading order calculation program
$!
$! Arguments : P1 = beta ==> creates exe in the beta area
$!
$! Created  18-OCT-1993   Sandor Feher and Patrick Mooney
$! Modified  3-JAN-1994   Meenakshi Narain : 
$!                          fix to pickup objs from physics_util.olb
$!                          fix to make exe in beta area
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$ DEFINE USR$EXE d0$physics_util       
$ if "''p1'".eqs."BETA" 
$ then
$ DEFINE USR$EXE [],d0$physics_util       
$ DEFINE D0$INC [],D0$INC$ROOT:[000000]
$! link/EXE=USR$EXE:lo_main_r  -
        d0$physics_util:beta_release.opt/opt,-
        d0$physics_util:physics_util.olb/l/inc=LO_MAIN_R, -
        D0$CERNLIB:PDFLIB/LIB,-
        D0$UTIL:UTIL4.OPT/OPT,-
        'CERNP'
$ link/DEB/EXE=USR$EXE:deb_LO_MAIN_R  -
      d0$physics_util:deb_beta_release.opt/opt,-
      d0$physics_util:deb_physics_util.olb/l/inc=LO_MAIN_R, -
      D0$CERNLIB:PDFLIB/LIB,-
      D0$UTIL:UTIL4.OPT/OPT,-
     'CERNP'
$ else
$ link/EXE=USR$EXE:lo_main_r  -
       d0$physics_util:physics_util.olb/l/inc=LO_MAIN_R, -
        D0$CERNLIB:PDFLIB/LIB,-
        D0$UTIL:UTIL4.OPT/OPT,-
        'CERNP'
$ link/DEB/EXE=USR$EXE:deb_LO_MAIN_R  -
      d0$physics_util:deb_physics_util.olb/l/inc=LO_MAIN_R, -
      D0$CERNLIB:PDFLIB/LIB,-
      D0$UTIL:UTIL4.OPT/OPT,-
     'CERNP'
$ endif
$EXIT:
$   EXIT    
