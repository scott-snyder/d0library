$!========================================================================
$!
$! Name      : SUB_MERGE2
$!
$! Purpose   : Submits first level merge job
$!
$! Arguments : p1 = BLNd15, BLND16 etc.
$!
$! Created  21-OCT-1990   Rajendran Raja
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$If p1 .eqs. ""  Then P1 = "BLND16"
$ SUBMIT/NOPRINTER/NOTIFY/QUEUE=D0SF11_BATCH - 
      /log_file = usr$scratch:[raja] -
      /NAME=merge_'P1'/PARAMETERS=('P1') Merge2_'p1'
$EXIT:
$   EXIT
