$!========================================================================
$!
$! Name      : SETUP_FILES
$!
$! Purpose   : Sets up files for batch run
$!
$! Arguments : p1 = ACC if accumulate mode
$!             p2 = NEW_RZ if you want new rz
$!             p3 = Name of subdirectory to be created
$!             p4 = name of subdirectory to be used
$!
$! Modified 18-JAN-1993   Rajendran Raja 
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$
$
$COPY USR$EXE:LJTOP_HM_TEMPLATE.RCP LJTOP_HMATRIX_PACKAGE.RCP
$
$   IF p1 .eqs. "ACC"
$   THEN
$SWAP ljtop_hmatrix_package.RCP XACCUM T
$   ELSE
$SWAP ljtop_hmatrix_package.RCP XACCUM F
$   ENDIF
$
$   IF p2 .eqs. "NEW_RZ"
$   THEN
$SWAP ljtop_hmatrix_package.RCP XNEW_RZ T
$   ELSE
$SWAP ljtop_hmatrix_package.RCP XNEW_RZ F
$   ENDIF
$
$   IF p3 .ne. ""
$   THEN
$SWAP ljtop_hmatrix_package.RCP XSUB_DIR 'p3'
$   ENDIF
$
$   IF p4 .ne. ""
$   THEN
$SWAP ljtop_hmatrix_package.RCP XUSE_DIR 'p4'
$   ENDIF
$
$purge ljtop_hmatrix_package.RCP
$
$Define datafile 
$
$DEFINE datafile_list datafile_list.dat
$DEFINE HBOOK_SAVE HBOOK_LJTOP_HMATRIX.SAVE
$DEFINE CALORIMETER_DMP LJTOP_CAL.DMP
$DEFINE CALORIMETER_OUT LJTOP_CAL.OUT
$
$SHO LOG CALORIMETER_DMP,CALORIMETER_OUT
$SHO LOG datafile,datafile_list
$EXIT:
$   EXIT
