$!========================================================================
$!
$! Name      : RUN_DISPLAY
$!
$! Purpose   : command file run by makeplots.com;
$!             don't execute this file by itself
$!
$! Created   5-JUN-1990   Tony Spadafora and John Womersley
$! Modified 20-JUN-1990   Howard Gordon - to use LN03
$! Modified 21-JUL-1990   Paul Draper - add lego plots
$! Modified 25-JUL-1990   Sarah Durston - took away lego plots for time being
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   DISPLAY5
ST
RUN100xxxx_1.HST4





DATE Y Y
OPT NBOX
OPT **P
GTIT 
RUN xxxx yyyy
W 2 2
H 1001
 T STAT NT STAT NT STAT NT STAT 
NT STAT NT STAT NT STAT NT STAT 
NT STAT NT STAT NT STAT NT STAT
NT STAT NT STAT NT STAT NT STAT 
NT STAT NT STAT NT STAT NT STAT 
NT STAT NT STAT NT STAT NT STAT
NT STAT NT STAT NT STAT NT STAT 
NT STAT NT STAT NT STAT NT STAT 
NT STAT NT STAT NT STAT NT STAT
NT STAT NT STAT NT STAT NT STAT 
NT STAT NT STAT NT STAT NT STAT 
NT STAT NT STAT NT STAT 
NT NT 
N 
N
N
N
N  
N 
N  
N  
N  
N  
W 1 1
W 2 2
H 1
 T STAT NT STAT NT STAT NT STAT 
NT STAT NT STAT NT STAT NT STAT 
NT STAT NT STAT NT STAT 
W 4 3 
NT NT NT NT NT NT NT NT NT NT NT 
W 2 2
H 62
 T STAT NT STAT NT STAT NT STAT 
EXIT
$tprint
$del/NOCON/NOLOG seq4010.ugs;
$!del/NOCON/NOLOG seq4010.ln03; 
$   
$EXIT:
$   EXIT
