$!========================================================================
$!
$! Name      : BUILD_DZSUM.COM
$!
$! Purpose   : make .exe for DZSURV and DZSUM
$!
$! Arguments : none
$!
$! Created 18-JAN-1994  James T. Linnemann
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!
$   WRITE SYS$OUTPUT "BUILD_DZSURV: Building program DZSURV"
$!
$ link/NOMAP/EXE=D0$UTIL:DZSURV.EXE -
     D0$UTIL:UTIL/INCLUDE=( DZSURVEY)/LIBRARY,-
     d0$util:util4.opt/opt,'CERNP'
$   WRITE SYS$OUTPUT "BUILD_DZSURV: Done!"
$!
$!
$   WRITE SYS$OUTPUT "BUILD_DZSUM: Building program DZSUM"
$!
$ link/NOMAP/EXE=D0$UTIL:DZSUM.EXE -
     D0$UTIL:UTIL/INCLUDE=( DZSUM)/LIBRARY,-
     d0$util:util4.opt/opt,'CERNP'
$   WRITE SYS$OUTPUT "BUILD_DZSUM: Done!"
$!
$EXIT:
$   EXIT
