$!========================================================================
$!
$! Name      : SETUP_USER
$!
$! Purpose   : User Setup for ISARCP. Define logicals:
$! 
$!                      COMMAND_FILE
$!                      PRINT_FILE
$!                      OUTPUT_FILE
$! Arguments : P1 = command file
$!             P2 = print file
$!             p3 = output file
$!
$! Created  21-DEC-1990   Harrison B. Prosper
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   
$   
$   DEFINE/NOLOG COMMAND_FILE   'p1'
$   DEFINE/NOLOG PRINT_FILE     'p2'
$   DEFINE/NOLOG OUTPUT_FILE    'p3'
$   
$   SHOW LOGICAL COMMAND_FILE
$   SHOW LOGICAL PRINT_FILE
$   SHOW LOGICAL OUTPUT_FILE
$EXIT:
$   EXIT
