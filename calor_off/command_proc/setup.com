$!------------------------------------------------
$!
$! Name      : SETUP
$!
$! Purpose   : Setup some CALOR_OFF commands
$!
$! Arguments : 
$!
$! Created   6-FEB-1990   Harrison B. Prosper
$! Modified 15-NOV-1991   Boaz Klima 
$!    Delete MAKE_PBD to be compatible with new PBD
$! Modified  4-FEB-1992   Stan M. Krzywdzinski, Harrison B. Prosper
$!    Deassign D0$PBD$ROOT
$!
$!------------------------------------------------
$   ON ERROR     THEN $ GOTO EXIT
$   ON WARNING   THEN CONTINUE
$   ON CONTROL_Y THEN $ GOTO EXIT
$   
$!================================================
$   file = F$TRNLNM("D0CMS_PRJ$HROOT")
$   IF file .NES. ""
$   THEN
$      DEFINE/NOLOG D0$CALOR_OFF   [], -
                                D0$BETA:[CALOR_OFF], -
                                D0$BETA:[CALOR_OFF.PBD], -
                                D0$CALOR_OFF$ROOT:[000000]
$   ELSE
$      DEFINE/NOLOG D0$CALOR_OFF   [],D0$CALOR_OFF$ROOT:[000000]
$   ENDIF
$   
$   MAKE_M*MS           :== @D0$CALOR_OFF:CALOR_OFF_MMS
$   MAKE_H*OOKS         :== @D0$CALOR_OFF:CALOR_OFF_HOOKS
$   SETUP_CAL*OR_OFF    :== @D0$CALOR_OFF:SETUP_CALOR_OFF 
$   
$   WRITE SYS$OUTPUT "  "
$   WRITE SYS$OUTPUT " Calor_off commands "
$   WRITE SYS$OUTPUT "  "
$   WRITE SYS$OUTPUT " MAKE_MMS         Create MMS.COM etc. for Calor_off"
$   WRITE SYS$OUTPUT " MAKE_HOOKS       Create hooks for Calor_off"
$   WRITE SYS$OUTPUT " SETUP_CALOR_OFF  @d0$calor_off:setup_calor_off"
$   WRITE SYS$OUTPUT "  "
$   
$!================================================
$!   Deassign D0$PBD$ROOT
$!================================================
$   
$   SET MESSAGE/NOFACILITY/NOIDENT/NOSEVERITY/NOTEXT
$   DEASSIGN D0$PBD$ROOT
$   SET MESSAGE/FACILITY/IDENT/SEVERITY/TEXT
$   
$EXIT:
$   EXIT
