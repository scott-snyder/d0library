$!------------------------------------------------
$!
$! Name      : SETUP_USER
$!
$! Purpose   : Define your Input file and No. of Events for 
$!             the simple D0 Event Display Program.
$!
$!             SETUP_PIXIE.COM executes this file.
$!
$! Created  16-DEC-1991   Nobuaki Oshima
$!
$!------------------------------------------------
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!
$   INQUIRE   input_file   "Give your input file:"
$   DEFINE/NOLOG PIXIE$DATAFILE    'input_file'
$   DEFINE/NOLOG PIXIE$COUNT       "1000000"
$   
$   SHOW LOG PIXIE$DATAFILE
$EXIT:
$   EXIT
