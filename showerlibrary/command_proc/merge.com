$!------------------------------------------------
$!
$! Name      : MERGE
$!
$! Purpose   : Merges a list of files
$!
$! Arguments : P1 = File name prefix.
$!             P2 = beginning serial number of input file
$!             P3 = end serial number of input file
$!             P4 = Output file name
$!
$! Created  27-SEP-1988   Rajendran Raja
$!
$!------------------------------------------------
$
$DEFINE SYS$SCRATCH UF13:
$SET DEF UF13:
$ P5 = P2
$ PRE = "''P1'"
$ ARG = ""
$MORE:
$ FILE = PRE + "''P5'"
$ ARG = ARG + FILE
$ P5 = P5 + 1
$ IF P5 .GT. P3 THEN GOTO DONE
$ ARG = ARG + ","
$ GOTO MORE
$DONE:
$IF P4 .EQS. "" THEN P4 = "MERGE"+"''P1'" + "''P2'" + "''P3'"
$SHO SYM ARG
$SHOW SYM P4
$   MERGE/KEY=(POSITION:1,SIZE:4,BINARY)/STATISTICS  'ARG' UF13:'P4'
$   ON ERROR     Then Goto EXIT
$   ON CONTROL_Y Then Goto EXIT
$EXIT:
$   EXIT
