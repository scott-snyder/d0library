$!------------------------------------------------
$!
$! Name      : MAKE_PIXIE_GUIDE
$!
$! Purpose   : Invoke RUNOFF with appropriate 
$!             switches to create .MEM file
$!             for PIXIE_GUIDE.RNO
$!
$! Arguments : None
$!
$! Created 18-MAR-1991   Harrison B. Prosper 
$! Modified 26-MAR-1991   Harrison B. Prosper 
$!      Bug fix
$!
$!------------------------------------------------
$   ON ERROR     Then CONTINUE
$   ON CONTROL_Y Then Goto EXIT
$
$   SET NOON
$   RUNOFF/INTERMEDIATE/NOOUTPUT D0$PIXIE$SOURCE:PIXIE_GUIDE.RNO
$   SET ON
$   RUNOFF/CONTENTS PIXIE_GUIDE.BRN
$   RUNOFF D0$PIXIE$SOURCE:PIXIE_GUIDE.RNO
$   
$   SET NOON
$   DELETE/NOCONFIRM/NOLOG PIXIE_GUIDE*.BRN;*
$   DELETE/NOCONFIRM/NOLOG PIXIE_GUIDE*.RNT;*
$   SET ON
$EXIT:
$   EXIT
