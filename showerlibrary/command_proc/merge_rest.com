$!========================================================================
$!
$! Name      : MERGE_REST
$!
$! Purpose   : GET REST OF IT
$!
$! Arguments : 
$!
$! Created  19-SEP-1990   Rajendran Raja
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$SET DEF SF13:
$ ARG = "SORT_BLND15_REST.DAT,"+-
        "MERGE_ALL_BLND15.DAT"
$SHO SYM ARG
$   MERGE/KEY=(POSITION:1,SIZE:4,BINARY)/STATISTICS  'ARG'-
 SF13:MERGE_ALL_REST_BLND15.DAT
$EXIT:
$   EXIT
