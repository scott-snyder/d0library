$!========================================================================
$!
$! Name      : BUILD_GM
$!
$! Purpose   : Create GM_CALOR_OFF.EXE
$!
$! Arguments : 
$!
$! Created  29-JAN-1994   Harrison B. Prosper
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$
$   SET VERIFY
$   
$   LINK/EXE=D0$PHYSICS_UTIL$GM:GM_CALOR_OFF.EXE  -
             D0$PHYSICS_UTIL$GM:GM.OPT/OPT,'CERNP'
$EXIT:
$   SET NOVERIFY
$   EXIT
