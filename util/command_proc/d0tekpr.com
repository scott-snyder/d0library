$!------------------------------------------------
$! Name      : D0TEKPR
$!
$! Purpose   : set the Talaris printer to Tecktro mode, then reset it.
$!
$! Arguments : name of the file to be printed
$!
$! Created   4-OCT-1988   Ghita Rahal-Callot
$!
$!------------------------------------------------
$   ON ERROR     Then Goto EXIT
$   ON CONTROL_Y Then Goto EXIT
$   IF P1 .EQS. "" THEN INQUIRE P1 " Name of the file to print:"
$   IF P1 .EQS. "" THEN GOTO EXIT
$   INPUT  = F$PARSE(P1,".LOG")
$   OUTPT  = F$PARSE(".QMS",P1)
$   OPEN/READ  RDFILE 'INPUT'
$   OPEN/WRITE SAVFIL 'OUTPT'
$! Set the printer in Tecktro mode
$   WRITE  SAVFIL  "^PY^-"
$   WRITE  SAVFIL  "^IGT"
$   WRITE  SAVFIL  "^-^PN-"
$! Here is the file to print
$COPYLOOP:
$   READ RDFILE/END=ENDCOPY INSTRG
$   WRITE SAVFIL INSTRG
$   GOTO COPYLOOP
$ENDCOPY:
$   CLOSE RDFILE
$! reset the printer
$   WRITE  SAVFIL  "ReSeTrEsEtReSeT"
$   CLOSE  SAVFIL
$   PRINT/NOFEED/DELETE 'OUTPT'
$EXIT:
$   EXIT
