$!========================================================================
$!
$! Name      : GMDISP
$!
$! Purpose   : 
$!
$! Arguments : 
$!
$! Created  11-MAY-1993   Harrison B. Prosper
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   DEFINE/USER CALFRAME_RCP D0$GM:GMDISP_CALFRAME.RCP
$   RUN/NODEBUG D0$GM:DEB_GMDISP_CALOR_OFF
$EXIT:
$   EXIT
