$!------------------------------------------------
$!
$! Name      : SETUP_NOISY.COM
$!
$! Purpose   : Setup some CALOR_OFF commands
$!
$! Arguments :
$!
$!------------------------------------------------
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!------------------------------------------------
$!   Define RCP files 
$!------------------------------------------------
$   DEFINE NOISY_RCP            D0$CAL:NOISY.RCP
$   DEFINE NOIANL_RCP           D0$CAL:NOIANL.RCP
$!------------------------------------------------
$!   Define CC pedestal file
$!------------------------------------------------
$   DEFINE PEDS_HIS_FILE       D0$STP:CCPEDS.HST
$!------------------------------------------------
$!   Define electronics SPICE file
$!------------------------------------------------
$   DEFINE SPICE_DAT           D0$STP:D0ALPHA.SPICE
$!------------------------------------------------
$!   Define pileup event stream
$!------------------------------------------------
$   DEFINE PILEFILE_LIST       D0$BETA:[NOISY]PILEUP.LIST
$!------------------------------------------------
$EXIT:
$   EXIT
