$!========================================================================
$!
$! Name      : BUILD_BSTAT
$!
$! Purpose   : Build batch status information utility
$!
$! Arguments : 
$!
$! Created  20-JUL-1993   Michael Diesburg
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!
$ LINK/EXE=D0$UTIL:BSTAT.EXE - 
           D0$UTIL:BSTAT/LIB/INCLUDE=(BSTAT), -
           LNK$LIBRARY/LIB
$!
$EXIT:
$   EXIT
