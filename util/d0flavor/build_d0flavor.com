$!------------------------------------------------
$!
$! Name      : BUILD_D0FLAVOR
$!
$! Purpose   : Build the D0FLAVOR utility
$!
$! Arguments : none
$!
$! Created   3-OCT-1988   Olivier Callot
$! Modified 19-MAR-1992   Harrison B. Prosper 
$!  Add GENERAL and CERNLIB
$!
$!------------------------------------------------
$   LINK/EXE=D0FLAVOR.EXE -
        D0$UTIL:D0FLAVOR/LIBRARY/INCLUDE=(D0FLAVOR), -
        D0$UTIL:UTIL/LIBRARY
