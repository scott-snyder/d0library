$!========================================================================
$!
$! Name      : SETUP
$!
$! Purpose   : Setup some RCP command symbols
$!
$! Arguments :
$!
$! Created  27-SEP-1990   Harrison B. Prosper
$! Modified 15-FEB-1991   Harrison B. Prosper
$!      Fixed typo
$! Modified 23-APR-1991   Harrison B. Prosper
$!      Added RCPCHECK
$! Modified 27-JUN-1991   Harrison B. Prosper
$!      Added SETUP_DISPATCH_BUILDER
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   WR  :== WRITE SYS$OUTPUT
$
$   RCP_TO_FZ   :== $D0$SRCP_UTIL:RCP_TO_FZ
$   RCPS*IZE    :== $D0$SRCP_UTIL:RCPSIZE
$   RCPT*EST    :== $D0$SRCP_UTIL:RCPTEST
$   RCPL*IST    :== $D0$SRCP_UTIL:RCPLIST
$   RCPC*HECK   :== $D0$SRCP_UTIL:RCPCHECK
$   WR " "
$   WR "        RCP commands"
$   WR "  "
$   WR " RCP_TO_FZ      Convert RCP file to an FZ file"
$   WR " RCPSIZE        Add/update \SIZE parameter in an RCP file"
$   WR " RCPCHECK       Check RCP file by calling INRCP"
$   WR " RCPTEST        Run some tests on RCP files/SRCP banks"
$   WR " RCPLIST        Produce a list of parameters in an RCP file"
$   WR "  "
$
$   @D0$SRCP_UTIL:SETUP_DISPATCH_BUILDER
$
$EXIT:
$   EXIT
