$!========================================================================
$!
$! Name      : BUILD_L2PROD_EXE
$!
$! Purpose   : Build all L2PRODuction Exes
$!
$! Arguments :
$!
$! Created  15-JAN-1993   D0library @ D0GSA::
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO ERROR_EXIT
$   ON CONTROL_Y THEN $ GOTO CONTY_EXIT
$
$   WRITE SYS$OUTPUT "*** START BUILD_L2PROD_EXE ***"
$   libp l2prod
$   libt all
$
$   WRITE SYS$OUTPUT "*** START - BUILD_DEFINE_L2_PASS ***"
$   @d0$level2$command_proc:build_define_l2_pass.com
$   WRITE SYS$OUTPUT "*** END - BUILD_DEFINE_L2_PASS ***"
$   if .not.d0$status then goto ERROR_EXIT
$
$   WRITE SYS$OUTPUT "*** START - COMPILE_L2PROD_PASS ***"
$   @d0$level2$command_proc:compile_l2prod_pass.com
$   WRITE SYS$OUTPUT "*** END - COMPILE_L2PROD_PASS ***"
$   if .not.d0$status then goto ERROR_EXIT
$
$   WRITE SYS$OUTPUT "*** START - BUILD_STANDARD_FILES all vms ***"
$   @d0$level2$command_proc:build_standard_files.com all vms
$   WRITE SYS$OUTPUT "*** END - BUILD_STANDARD_FILES all vms ***"
$   if .not.d0$status then goto ERROR_EXIT
$
$!?$   WRITE SYS$OUTPUT "*** START - GENERATE_COOR_SIM_INFO ***"
$!?$   @d0$level2$command_proc:generate_coor_sim_info.com
$!?$   WRITE SYS$OUTPUT "*** END - GENERATE_COOR_SIM_INFO ***"
$!?$   if .not.d0$status then goto ERROR_EXIT
$
$   WRITE SYS$OUTPUT "*** START - BUILD_STANDARD_FILES ALL REGULAR NOSETUP ***"
$   @d0$level2$command_proc:build_standard_files.com all regular nosetup
$   WRITE SYS$OUTPUT "*** END - BUILD_STANDARD_FILES all regular nosetup ***"
$   if .not.d0$status then goto ERROR_EXIT
$
$   WRITE SYS$OUTPUT "*** START - BUILD_STANDARD_FILES ALL FILT_SHADOW NOSETUP ***"
$   @d0$level2$command_proc:build_standard_files.com all filt_shadow nosetup
$   WRITE SYS$OUTPUT "*** END - BUILD_STANDARD_FILES ***"
$   if .not.d0$status then goto ERROR_EXIT
$
$   WRITE SYS$OUTPUT "*** START - BUILD_STANDARD_FILES ALL FILT_TEST NOSETUP ***"
$   @d0$level2$command_proc:build_standard_files.com all filt_test nosetup
$   WRITE SYS$OUTPUT "*** END - BUILD_STANDARD_FILES ***"
$   if .not.d0$status then goto ERROR_EXIT
$
$   WRITE SYS$OUTPUT "*** START - GENERATE_COOR_SIM_INFO ***"
$   @d0$level2$command_proc:generate_coor_sim_info.com
$   WRITE SYS$OUTPUT "*** END - GENERATE_COOR_SIM_INFO ***"
$   if .not.d0$status then goto ERROR_EXIT
$
$   PURGE *.*/KEEP=2
$
$EXIT:
$   nolibp
$   d0$status :== TRUE
$  WRITE SYS$OUTPUT "*** END BUILD_L2PROD_EXE ***"
$   exit
$ERROR_EXIT:
$CONTY_EXIT:
$   nolibp
$   d0$status :== FALSE
$  WRITE SYS$OUTPUT "*** END BUILD_L2PROD_EXE -ERROR- ***"
$   exit
