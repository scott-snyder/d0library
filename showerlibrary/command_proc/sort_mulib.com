$!------------------------------------------------
$!
$! Name      : SORT
$!
$! Purpose   : Sorts a file Based on Key position and size
$!
$! Arguments : P1 = File name. Input name = Output name
$!             P2 = Output file name
$!
$! Created  27-SEP-1988   Rajendran Raja
$!
$!------------------------------------------------
$
$   IF P2 .eqs. "" THEN   P2 = "SORT_"+"''P1'"
$   IF P3 .eqs. "" THEN   P3 = 3500
$!DEFINE SYS$SCRATCH UF13:
$!DEFINE UF13 USR$SCRATCH:[SINGHJB] 
$DEFINE UF13 USR$ROOT:[SINGHJB.SHOWER] 
$ WRITE SYS$OUTPUT "Start - " + F$TIME()                             
$ SORT/KEY=(POSITION:1,SIZE:4,BINARY,UNSIGNED) -
      /STATISTICS -
 UF13:'P1' UF13:'P2'
$ WRITE SYS$OUTPUT "End - " + F$TIME()                               
$   ON ERROR     Then Goto EXIT
$   ON CONTROL_Y Then Goto EXIT
$EXIT:
$   EXIT
