$!-------------------------------------------------------------
$!
$! Name      : SETUP_SAWSTP.COM
$!
$! Purpose   : Setup logical names and commands for
$!             programs which create the SAMUS 
$!             geometrical data in GEANT format.
$!
$! Created  06-MAY-1991   Vladimir Glebov 
$! Modified 23-FEB-1994   Alexander Efimov : for Geom data base
$!
$!-------------------------------------------------------------
$   ON ERROR     Then Goto EXIT
$   ON CONTROL_Y Then Goto EXIT
$!
$   INPUT = "D0$STP$SAMUS:"
$   OUTPUT = ""
$!
$!-------------------------------------------------------------
$!   Define INPUT files
$!-------------------------------------------------------------
$!
$   DEFINE/NOLOG SAMUS_BEAM_RCP    'INPUT'SAMUS_BEAM.RCP
$   SHOW LOGICAL SAMUS_BEAM_RCP
$!
$   DEFINE/NOLOG SAMUS_MAGNET_RCP  'INPUT'SAMUS_MAGNET.RCP 
$   SHOW LOGICAL SAMUS_MAGNET_RCP
$!
$   DEFINE/NOLOG SAMUS_STATION_RCP 'INPUT'SAMUS_D0STATION'P1'.RCP 
$   SHOW LOGICAL SAMUS_STATION_RCP
$!
$   WRITE SYS$OUTPUT "  "
$!-------------------------------------------------------------
$!   Define OUTPUT files
$!-------------------------------------------------------------
$!
$   DEFINE/NOLOG SAM_STPFILE    'OUTPUT'SAM_D0STPFILE'P1'.DAT
$   SHOW LOGICAL SAM_STPFILE
$!
$EXIT:
$   EXIT
