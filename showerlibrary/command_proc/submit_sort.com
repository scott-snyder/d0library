$!========================================================================
$!
$! Name      : SUBMIT_SORT
$!
$! Purpose   : Submit an offline job to sort showerlibraryfiles.
$!
$! Arguments : 
$!
$! Created  17-OCT-1990   Rajendran Raja
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$Write sys$output "Running Sort Job"
$ SUBMIT/NOPRINTER/NOTIFY/QUEUE=D0SF11_BATCH/NAME=SORT_SHOWER SORT_BLND16
$EXIT:
$EXIT:
$   EXIT
