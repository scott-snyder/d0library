$!========================================================================
$!
$! Name      : SETUP_USER
$!
$! Purpose   : Sets up User defined symbols
$!
$! Arguments : 
$!
$! Created  17-APR-1990   Rajendran Raja
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!
$DEFINE DATAFILE USR$ROOT2:[RAJA.BLND05]RUN05F03_01.GEN1;1    
$DEFINE DATAFILE_LIST D0$CMS:[SHOWERLIBRARY]BLND05.LIST
$
$SHOW LOG DATAFILE
$SHOW LOG DATAFILE_LIST
$EXIT:
$   EXIT
