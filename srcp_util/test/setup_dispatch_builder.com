$!========================================================================
$!
$! Name      : SETUP_DISPATCH_BUILDER.COM
$!
$! Purpose   : Setup symbols/logicals for DISPATCH_BUILDER
$!
$! Arguments : None
$!
$! Created  25-JUN-1991   Harrison B. Prosper
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$
$   wr          :== WRITE SYS$OUTPUT
$   your_name   = "EZ_DISPATCH_BUILDER"
$
$!================================================
$!   Get your name from MY_EDITOR.TPU
$!================================================
$
$   IF F$SEARCH("SYS$LOGIN:MY_EDITOR.TPU") .EQS. "" THEN GOTO DEFINE_COMMAND
$
$   OPEN/READ MYFILE SYS$LOGIN:MY_EDITOR.TPU
$
$READ_EDITOR_TPU:
$
$   READ/END_OF_FILE=CLOSE_MY_FILE myfile your_name
$   L           = F$LENGTH(your_name)
$   line        = F$EDIT(your_name,"UPCASE")
$   IF F$LOCATE("$AUTHOR_NAME",line) .GE. L THEN GOTO READ_EDITOR_TPU
$
$   I           = F$LOCATE("""",your_name)
$   your_name   = F$EXTRACT(I+1,L-I,your_name)
$   I           = F$LOCATE("""",your_name)
$   your_name   = F$EXTRACT(0,I,your_name)
$
$CLOSE_MY_FILE:
$
$   CLOSE myfile
$
$   DBD         :== $D0$SRCP_UTIL:EZ_DISPATCH_BUILDER
$   DEFINE/NOLOG AUTHOR                 "''your_name'"
$   DEFINE/NOLOG DISPATCH_BUILDER_RCP     D0$SRCP_UTIL:DISPATCH_BUILDER.RCP
$
$   WR " "
$   WR "        DISPATCH BUILDER command"
$   WR "  "
$   WR " DBD            Run EZ_DISPATCH_BUILDER"
$   WR " Author         ''your_name'"
$   WR " "
$EXIT:
$   EXIT
