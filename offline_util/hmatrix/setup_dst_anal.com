$!========================================================================
$!
$! Name      : SETUP_DST_ANAL
$!
$! Purpose   : sets up dst analysis
$!
$! Arguments : 
$!
$! Created   4-JAN-1991   Rajendran Raja
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$define datafile mc_data$hroot:[tops.dst]dst_n_ttb100_isa.gen
$define calframe_rcp usr$area:calframe_dst.rcp   !Do DST analysis
$write sys$output "SET up for DST analysis"
$EXIT:
$   EXIT
