$!========================================================================
$!
$! Name      : RECO_FAILURE
$!
$! Purpose   : 
$!
$! Arguments : 
$!
$! Created   2-SEP-1992   Lee Lueking
$! Modified 21-SEP-1993   Dorota I. Genser - changes for NEW JOBMAN
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!Blow away the output files if the job fails
$IF DST_STREAMS THEN DELETE/NOCONFIRM/LOG 'DST_FILES'*;*
$IF STA_STREAMS THEN DELETE/NOCONFIRM/LOG 'STA_FILES'*;*
$ del/nocon/nolog PM$run:*'jobname'*.*;*
$ del/nocon/nolog 'tape_name'.*;*
$ del/nocon/nolog pm$run:For003.dat;*
$ del/nocon/nolog pm$run:for000.dat;*
$!
$EXIT:
$   EXIT
