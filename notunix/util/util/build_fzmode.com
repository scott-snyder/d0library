$!------------------------------------------------
$!
$! Name      : BUILD_FZMODE
$!
$! Purpose   : Build FZMODE.EXE
$!
$! Arguments : none
$!
$! Created  28-JAN-1993   Harrison B. Prosper 
$!
$!------------------------------------------------
$!   ON ERROR     Then Goto EXIT
$!   ON CONTROL_Y Then Goto EXIT
$
$   WRITE SYS$OUTPUT "BUILD_FZMODE: Building program FZMODE"
$   LINK/NOMAP/EXE=D0$UTIL:FZMODE.EXE -
        D0$UTIL:UTIL/INCLUDE=(FZMODE)/LIB,-
        d0$physics_util:physics_util/lib,-
        D0$GENERAL:GENERAL/LIB,-
        'CERNP'
$   WRITE SYS$OUTPUT "BUILD_FZMODE: Done!"
$
$! EXIT:
$!   EXIT
