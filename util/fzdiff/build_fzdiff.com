$!------------------------------------------------
$!
$! Name      : BUILD_FZDIFF
$!
$! Purpose   : Build the FZDIFF utility.  Also fzsort, fzscramble.
$!
$! Arguments : none
$!
$! Created  11-Sep-1992   Herbert Greenlee
$!
$!------------------------------------------------
$   LINK/EXE=D0$UTIL:FZDIFF.EXE -
        D0$UTIL:FZDIFF/LIBRARY/INCLUDE=(FZDIFF), -
        d0$physics_util:physics_util/lib, -
        D0$GENERAL:GENERAL/LIB, -
        'CERNP'
$
$   LINK/EXE=D0$UTIL:FZSORT.EXE -
        D0$UTIL:FZDIFF/LIBRARY/INCLUDE=(FZSORT), -
        D0$GENERAL:GENERAL/LIB, -
        'CERNP'
$
$   LINK/EXE=D0$UTIL:FZSCRAMBLE.EXE -
        D0$UTIL:FZDIFF/LIBRARY/INCLUDE=(FZSCRAMBLE), -
        D0$GENERAL:GENERAL/LIB, -
        'CERNP'
