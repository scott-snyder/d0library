$!------------------------------------------------
$!
$! Name      : CALOR_OFF_MMS
$!
$! Purpose   : Make MMS.COM, DEFINE.COM and DESCRIP.MMS
$!              for CALOR_OFF frame.
$!
$! Arguments : 
$!
$! Created  13-SEP-1990   Harrison B. Prosper
$!
$!------------------------------------------------
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   
$   WR          :== WRITE SYS$OUTPUT
$   
$   Combined_package    = p1
$   
$   IF Combined_package .EQS. ""
$   THEN
$       WR "  "
$       WR " CALOR_OFF - create files MMS.COM, DEFINE.COM and DESCRIP.MMS"
$       WR "  "
$       WR "  "
$       WR " Note: The file containing the PBD hooks will be called"
$       WR "  "
$       WR "       XXXXX_CALOR_OFF.FOR" 
$       WR "  "
$       WR "       where XXXXX is the name of your combined package."
$       WR "  "
$       INQUIRE Combined_package " Enter name of your COMBINED-package "
$   ENDIF
$   IF Combined_package .EQS. "" THEN GOTO EXIT
$   
$   INQUIRE answer " Do you want to link with DI3000 [N] "
$   IF F$EXTRACT(0,1,answer) .EQS. "Y" 
$   THEN
$       di3000 = "/DI3000"
$   ELSE
$       di3000 = ""
$   ENDIF
$   
$   CMMS        CALOR_OFF 'Combined_package'
$   CDESCRIP    CALOR_OFF'di3000'
$EXIT:
$   EXIT
