$!------------------------------------------------
$!
$! Name      : BUILD_D0CHECK
$!
$! Purpose   : Build D0CHECK.EXE
$!
$! Arguments : none
$!
$! Created  13-JUN-1989   Harrison B. Prosper
$!
$!------------------------------------------------
$!   ON ERROR     Then Goto EXIT
$!   ON CONTROL_Y Then Goto EXIT
$
$   LINK/NOMAP/EXE=D0$UTIL:D0CHECK.EXE -
        D0$UTIL:UTIL/LIBRARY/INCLUDE=(D0CHECK),-
        D0$LIBRARY_UTIL:LIBRARY_UTIL/LIB,-
        D0$GENERAL:GENERAL/L,-
        'CERNP'
$
$! EXIT:
$!   EXIT
