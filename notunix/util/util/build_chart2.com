$!========================================================================
$!
$! Name      : BUILD_CHART2.COM
$!
$! Purpose   : make .exe for CHART2, .exe from Robert Hatcher of MSU for making
$!                  call trees
$!
$! Arguments : none
$!
$! Created  16-OCT-1993   James T. Linnemann
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!
$   WRITE SYS$OUTPUT "BUILD_CHART2: Building program CHART2"
$!
$ link/NOMAP/EXE=D0$UTIL:CHART2.EXE -
     D0$UTIL:UTIL/INCLUDE=(chart2,chart2_TABLES)/LIB, -
     d0$util:util4.opt/opt,'CERNP'
$   WRITE SYS$OUTPUT "BUILD_CHART2: Done!"
$!
$EXIT:
$   EXIT
