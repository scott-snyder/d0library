$!========================================================================
$!
$! Name      : COPY_COM_SHIFTS
$!
$! Purpose   : Copy COM_SHIFTS.TXT from D0$BETA:[OPERATIONS] to 
$!              default directory.
$!
$! Arguments : 
$!
$! Created   1-DEC-1992   Harrison B. Prosper
$! Modified 28-JAN-1993   Harrison B. Prosper 
$!      Fix typo
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   
$   Cluster = F$TRNLNM("SYS$CLUSTER_NODE")
$   IF Cluster .EQS. "D0SFT::"
$   THEN
$       Node = ""
$   ELSE
$       Node = "D0SFA::"
$   ENDIF
$   
$   WRITE SYS$OUTPUT "Copying COM_SHIFTS.TXT to your area..."
$   COPY/NOLOG/NOCONF 'Node'D0CMS_PRJ$HROOT:[OPERATIONS]COM_SHIFTS.TXT []
$   WRITE SYS$OUTPUT "Done!"
$   
$EXIT:
$   EXIT
