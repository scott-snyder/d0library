$!------------------------------------------------
$!
$! Name      : BUILD_FZTON
$!
$! Purpose   : Build the FZTON utility
$!
$! Arguments : none
$!
$! Created  11-Sep-1992   Herbert Greenlee
$!
$!------------------------------------------------
$   LINK/EXE=D0$UTIL:FZTON.EXE -
        D0$UTIL:FZDIFF/LIBRARY/INCLUDE=(FZTON), -
        D0$UTIL:UTIL4/OPT,-
        'CERNP'
