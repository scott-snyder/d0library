$!------------------------------------------------
$!
$! Name      : SETUP
$!
$! Purpose   : Setup some D0USER commands
$!
$! Arguments : 
$!
$! Created   6-FEB-1990   Harrison B. Prosper
$!
$!------------------------------------------------
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   
$   DEFINE/NOLOG D0$D0USER      [], -
                                        D0$D0USER$ROOT:[000000]
$   
$   MAKE_P*BD           :== @D0$D0USER:D0USER_PBD
$   MAKE_H*OOKS         :== @D0$D0USER:D0USER_HOOKS
$   
$   WRITE SYS$OUTPUT "  "
$   WRITE SYS$OUTPUT " D0USER Program-Builder commands "
$   WRITE SYS$OUTPUT "  "
$   WRITE SYS$OUTPUT " MAKE_PBD         Create a D0USER .PBD file"
$   WRITE SYS$OUTPUT " MAKE_HOOKS       Create a D0USER .FOR Hooks file "
$   WRITE SYS$OUTPUT "  "
$   
$EXIT:
$   EXIT
