$!------------------------------------------------
$!
$! Name      : MAKE_RCP_MANUAL
$!
$! Purpose   : Invoke RUNOFF with appropriate 
$!             switches to create .MEM file
$!             for RCP_MANUAL.RNO
$!
$! Arguments : None
$!
$! Created   20-JAN-1989   Harrison B. Prosper
$! Modified 11-JUL-1991   Harrison B. Prosper 
$!      Can now remove delete
$!
$!------------------------------------------------
$   ON ERROR     Then CONTINUE
$   ON CONTROL_Y Then Goto EXIT
$!
$   SET NOON
$   RUNOFF/INTERMEDIATE/NOOUTPUT RCP_MANUAL2
$   RUNOFF/INTERMEDIATE/NOOUTPUT RCP_MANUAL
$   SET ON
$   
$   RUNOFF/CONTENTS RCP_MANUAL2.BRN
$   RUNOFF/CONTENTS RCP_MANUAL.BRN
$   
$   RUNOFF RCP_MANUAL
$   RUNOFF RCP_MANUAL2
$   
$   DELETE/NOCONFIRM/LOG RCP_MANUAL*.BRN;*
$   DELETE/NOCONFIRM/LOG RCP_MANUAL*.RNT;*
$!   No longer needed! 11-July-1991 HBP
$!   DELETE/NOCONFIRM/LOG SRCP_*.MEM;*
$EXIT:
$   EXIT
