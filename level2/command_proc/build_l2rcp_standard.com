$!========================================================================
$!
$! Name      : BUILD_L2RCP_STANDARD
$!
$! Purpose   : build the com file BUILD_L2RCP_STANDARD.COM
$!
$! Arguments :
$!
$! Created  14-JAN-1993   James T. Linnemann
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$ PBD/FRAME=CALOR_OFF/PACK=(%L2RCP)/NOCOMPILE
$ RENAME  L2RCP_CALOR_OFF.COM L2RCP_STANDARD.COM
$ DELETE *L2RCP_CALOR_OFF.*;*
$!copy to d0$level2$command_proc?
$EXIT:
$   EXIT
