$!------------------------------------------------
$!
$! Name      : BUILD_SFORTRAN
$!
$! Purpose   : Build SFORTRAN.EXE
$!
$! Arguments : none
$!
$! Created   7-OCT-1990   Harrison B. Prosper 
$!
$!------------------------------------------------
$!   ON ERROR     Then Goto EXIT
$!   ON CONTROL_Y Then Goto EXIT
$
$   LINK/NOMAP/EXE=D0$UTIL:SFORTRAN.EXE -
        D0$UTIL:UTIL/INCLUDE=(SFORTRAN)/LIB,-
        D0$GENERAL:GENERAL/LIB,-
        'CERNP'
$
$! EXIT:
$!   EXIT
