$!========================================================================
$!
$! Name      : MAKEPLOTS
$!
$! Purpose   : run command file to make plots
$!
$! Arguments : p1 = run number
$!             p2 = comment line excluding run number
$!
$! Created   5-JUN-1990   John Womersley
$! MODIFIED 20-JUN-1990   Howard Gordon - to allow using tbhist: and use ln03
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   set noverify
$   if p1 .eqs. "" then $ inquire p1 "Run number (4 digits)?" 
$   if p2 .eqs. "" then $ inquire p2 "Run conditions comment?" 
$   p3 = " "" ''p2' "" "
$   IF P4 .EQS. "" THEN $ INQUIRE P4 "READ .HST4 FROM [CALDATA.EXAMINE2] ? [Y]"
$   IF P4 .EQS. "" .OR. P4 .EQS. "Y" THEN P4 = "Y"
$   IF P4 .NES. "Y" THEN $ WRITE SYS$OUTPUT "READING OLD .HST4 FROM TBHIST:
$   IF P4 .NES. "Y" THEN $ COPY TBHIST:RUN100'P1'_1.HST4 RUN100'P1'_1.HST4
$   write sys$output "Must EXIT from EXAMINE before printing file"
$   pr/que=pk022_ln03 run100'p1'_status.sum
$   copy run_display_ln03.com temp.com
$   swap temp.com xxxx 'p1'
$   swap temp.com yyyy 'p3'
$   @temp.com
$   delete/nocon/nolog temp.com;*
$   IF P4 .EQS. "Y" THEN $copy run100'p1'_1.hst4 tbhist:run100'p1'_1.hst4
$   delete run100'p1'_1.hst4;1 
$EXIT:
$   EXIT
