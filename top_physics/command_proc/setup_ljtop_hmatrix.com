$!========================================================================
$!
$! Name      : SETUP_ljtop_Hmatrix
$!
$! Purpose   : Setup file for LJTOP_HMATRIX package
$!
$! Arguments : P1 = ACC if you want to accumulate hmatrices
$!             P2 = NEW_RZ if you want a new rz file
$!             p3 = name of subdirectory to be created
$!             p4 = name of subdirecotry to be used
$!
$! Modified 18-JAN-1993   Rajendran Raja 
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$@D0$CALOR_OFF$ROOT:[000000]SETUP_CALOR_OFF
$define d0$pixie [],d0$pixie$root:[000000]
$@setup_hmatrix 
$@setup_files 'p1' 'p2' 'p3'
$EXIT:
$   EXIT
