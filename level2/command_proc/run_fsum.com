$!========================================================================
$!
$! Name      : FSUM
$!
$! Purpose   : run the GRAND_FSUM package
$!
$! Arguments : none
$!
$! Created  26-APR-1992   James T. Linnemann
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$  SAY :== WRITE SYS$OUTPUT
$ DEFINE GRAND_FSUM_RCP GRAND_FSUM.RCP
$ DEFINE DUMMY_FILE PRJ$ROOT211:[TRIGGER_FEST]*40TO80_01.*
$  TEST = F$SEARCH("GRAND_FSUM_RCP")
$  IF (TEST .EQS. "") 
$    THEN
$    COPY D0$LEVEL2$SOURCE:GRAND_FSUM.RCP GRAND_FSUM.RCP 
$    SAY "Please edit GRAND_FSUM.RCP before prceeding"
$    GOTO EXIT
$  ENDIF
$ DEFINE WHERE FILTER_DEFAULT,D0$LEVEL2
$ FS := $WHERE:GRAND_FSUM_D0USER.EXE
$ FS/nosmg/command=d0$level2$l2sim:fsum.inp
$ DEASSIGN WHERE
$ SAY "look at GRAND_FSUM.OUT or GRAND_FDIFF.OUT"
$EXIT:
$   EXIT
