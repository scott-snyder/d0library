$!========================================================================
$!
$! Name      : BUILD_MEGA_AUTOCOMPARE.COM
$!
$! Purpose   : make .exe for MEGA_AUTOCOMPARE. 
$!
$! Arguments : none
$!
$! Created 18-JAN-1994   R. J. Genik II to make this exe.
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!
$   WRITE SYS$OUTPUT "BUILD_MEGA_AUTOCOMPARE: Building program MEGA_AUTOCOMPARE"
$!
$ link/NOMAP/EXE=D0$UTIL:MEGA_AUTOCOMPARE_HISTOGRAM.EXE -
     D0$UTIL:UTIL/INCLUDE=( MEGA_AUTOCOMPARE_HISTOGRAM,-
     dbinom,hdbcop,prob)/LIBRARY,-
     d0$util:util4.opt/opt,'CERNP'
$   WRITE SYS$OUTPUT "BUILD_MEGA_AUTOCOMPARE: Done!"
$!
$EXIT:
$   EXIT
