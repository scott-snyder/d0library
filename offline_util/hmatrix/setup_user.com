$!========================================================================
$!
$! Name      : SETUP_USER
$!
$! Purpose   : Sets up User defined symbols
$!
$! Arguments : 
$!
$! Created  17-APR-1990   Rajendran Raja
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!
$!DEFINE DATAFILE D0$DATA:[HMATRIX.D0GEANT]D0GEANT_ELEC_50GEV_ETA1.OUT
$!DEFINE DATAFILE D0$DATA:[HMATRIX.D0GEANT]D0GEANT_PION_40GEV_ETA1.OUT
$DEFINE DATAFILE d0sf13$dkb0:[top]TTB100_110_ISA.GEN
$define dst_out d0sf13$dkb0:[top]ttb100_110_isa.dst
$DEFINE DATAFILE_LIST D0$CMS:[SHOWERLIBRARY]BLND05.LIST
$DEFINE SHOWERLIBRARY_RCP SHOWERLIBRARY.RCP
$define src d0$calor_off$source
$@setup_dst_anal
$
$SHOW LOG DATAFILE
$SHOW LOG DATAFILE_LIST
$EXIT:
$   EXIT
