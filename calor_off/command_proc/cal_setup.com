$!------------------------------------------------
$!
$! Name      : CAL_SETUP.COM
$!
$! Purpose   : Define logical names used in CALOFF
$!
$! Arguments : None
$!
$! Created  27-FEB-1989   Harrison B. Prosper, John Womersley
$!
$!------------------------------------------------
$   ON ERROR     Then Goto EXIT
$   ON CONTROL_Y Then Goto EXIT
$!------------------------------------------------
$!   Define input file logical names
$!------------------------------------------------
$!
$@define        !Setup logicals
$!
$   DIR = "D0CMS_PRJ$HROOT:[CALOR_OFF]"
$   DEFINE/NOLOG D0$CAL       'DIR'
$   SHOW LOGICAL D0$CAL
$!
$   caloffn      :== RU 'DIR'TESTCALOR_OFF
$   caloffd      :== RU 'DIR'TESTDEB_CALOR_OFF
$   caloffnd     :== RU 'DIR'TESTDEB_CALOR_OFF/NODEB
$!
$   DEFINE/KEY/NOLOG PF4 "CALOFFD" /TERMINATE/ECHO
$   DEFINE/KEY/NOLOG PF1 "CALOFFND" /TERMINATE/ECHO
$!
$   WRITE SYS$OUTPUT "   "
$   WRITE SYS$OUTPUT " Hit PF4 to run CALOFFD   (DEBUG)"
$   WRITE SYS$OUTPUT " Hit PF1 to run CALOFFND  (NODEBUG)"
$   WRITE SYS$OUTPUT "   "
$!
$   DEFINE DATAFILE             D0DATA_PRJ$HROOT:[CALOR_OFF]GEANT_E15.DAT
$   DEFINE CALFRAME_RCP         D0$CAL:CALFRAME.RCP
$   DEFINE CALEVT_RCP           D0$CAL:CALEVT.RCP
$   DEFINE CAL_STPFILE          D0$CAL:CAL_STPFILE.DAT
$   DEFINE CALTOWER_STP         D0$STP:CALTOWER_STP.DAT
$!
$   DEFINE CAHITS_RCP           D0$CAL:CAHITS.RCP
$   DEFINE CAJETS_RCP           D0$CAL:CAJETS.RCP
$   DEFINE CAPHEL_RCP           D0$CAL:CAPHEL.RCP
$EXIT:
$   EXIT
