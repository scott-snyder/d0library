
$!========================================================================
$!
$! Name      : BUILD_PXBUILD
$!
$! Purpose   : Create file PXBUILD.EXE
$!
$! Arguments : None
$!
$! Created  15-SEP-1990   Harrison B. Prosper
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$
$@DI3_LINK:DI3LOAD -
D0$PIXIE:PIXIE/INCLUDE=(PXBUILD)/LIBRARY,-
D0$UTIL:UTIL4/OPT,- 
D0$D0USER:FRAME/L,-
D0$GENERAL:GENERAL/L,-
D0$CERNLIB:PACKLIB/L,KERNLIB/L SHARE /MAP=NOMAP/EXE=PXBUILD.EXE
$EXIT:
$   EXIT
