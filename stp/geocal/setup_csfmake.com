$! DEC/CMS REPLACEMENT HISTORY, Element SETUP_CSFMAKE.COM
$! *2    15-APR-1992 18:07:18 STEWART "csfmake setup"
$! *1    15-APR-1992 17:59:48 STEWART "setup file for csf_stpfile"
$! DEC/CMS REPLACEMENT HISTORY, Element SETUP_CSFMAKE.COM
$!========================================================================
$!
$! Name      : SETUP_CSFMAKE
$!
$! Purpose   : Define logicals for CSFMAKE
$!
$! Arguments :
$!
$! Created   2-MAR-1992   Harrison B. Prosper, Chip Stewart
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$
$!================================================
$!   RCP file containing Conversion factor and
$!   Weights
$!================================================
$   DEFINE/NOLOG    CSF_RCP  D0$CALOR_OFF:CSF.RCP
$
$!================================================
$!   RCP files containing Sampling Fractions
$!================================================
$   DEFINE/NOLOG    CSF_CCEM_RCP D0$CALOR_OFF:CSF_CCEM.RCP
$   DEFINE/NOLOG    CSF_ECEM_RCP D0$CALOR_OFF:CSF_ECEM.RCP
$   DEFINE/NOLOG    CSF_ICD_RCP  D0$CALOR_OFF:CSF_ICD.RCP
$!================================================
$!   Output file for conversion constants
$!================================================
$   DEFINE/NOLOG    CSF_STPFILE D0$STP:CSF_STPFILE.DAT
$
$!================================================
$!   Define symbols to run program
$!================================================
$
$   CSFMAKE      :== $D0$STP:CSFMAKE.EXE
$   DEB_CSFMAKE  :== $D0$STP:DEB_CSFMAKE.EXE
$!
$EXIT:
$   EXIT
