$!========================================================================
$!
$! Name      : PCA_USE
$!
$! Purpose   : SEDTS UP PCA SYMBOL
$!
$! Arguments : 
$!
$! Created  27-JUN-1990   Rajendran Raja
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$DEFINE LIB$DEBUG SYS$LIBRARY:PCA$COLLECTOR.EXE      
$EXIT:
$   EXIT
