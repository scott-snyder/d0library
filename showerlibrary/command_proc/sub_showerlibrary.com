$!------------------------------------------------
$!
$! Name      : SUB_SHOWERLIBRARY.com
$!
$! Purpose   : SUBMITS 1 SHOWERLIBRARY generation JOB WHICH CHAINS ON ITSELF.
$!
$! Arguments : 
$!
$! Created  13-NOV-1989   Rajendran Raja
$! Modified 29-DEC-1989   Rajendran Raja 
$!
$!------------------------------------------------
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!SET VERIFY
$If p1 .eqs. ""  Then P1 = "BLND14"
$ SUBMIT/NOPRINTER/NOTIFY/QUEUE=D0SF13_BATCH - 
      /log_file = usr$scratch:[raja] -
      /NAME=SHLB_'P1'/PARAMETERS=('P1') showerlibrary
$EXIT:
$   EXIT
