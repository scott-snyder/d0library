$!========================================================================
$!
$! Name      : L2PROD_SETUP
$!
$! Purpose   : library setup for gamma area for production linking
$!
$! Arguments : P1 - []/NEW: choose between TOP (PREVIOUS) and NEW LIBPROD areas
$!
$! Created   3-AUG-1992   James T. Linnemann
$! Modified 29-APR-1993   Alan M. Jonckheere Add NEW parameter
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$
$!set up enough environment to run setup_l2sim and l2lib_setup
$ DEFINE L2$new d0$prod:[d0production.l2prod.new]
$ DEFINE L2$old d0$prod:[d0production.l2prod.old]
$ DEFINE L2$top d0$prod:[d0production.l2prod]
$ DEFINE L2$PROD l2$new,l2$top
$ DEFINE/trans=(concealed) L2$root d0$prod:[d0production.l2prod.]
$ DEF_DIR = F$ENVIRONMENT( "DEFAULT" )
$
$ p1 = f$edit(p1,"UPCASE")
$ IF p1 .eqs. "NEW" .or. p1 .eqs "N"
$ THEN
$     WRITE SYS$OUTPUT "*** Using L2PROD NEW ***"
$     LIBPROD L2PROD
$     set def l2$new
$ ELSE
$     WRITE SYS$OUTPUT "*** Using L2PROD TOP ***"
$     LIBPROD/PREVIOUS L2PROD
$     set def l2$top
$ ENDIF
$
$ @d0$level2$command_proc:SETUP_L2SIM D D SRV "" 1     !setup for data, std exe
$ SET DEFAULT 'DEF_DIR'
$
$! com files for handling releases
$ DEFINE l2$mgr d0$prod:[d0production.admin_util.l2prod.mgr]
$ DEFINE l2$com d0$level2$command_proc
$
$! logicals and symbols for library work
$ DEFINE L2$ROOT PROD$L2PROD$ROOT
$ DEFINE l2$eln l2$mgr
$ DEFINE l2$work d0$prod:[d0production.admin_util.l2prod.work]
$ DEFINE l2$regular l2$new
$ DEFINE L2$RCP D0$LEVEL2$L2SIM
$ @d0$level2$command_proc:L2RCP_SETUP
$ SHOW SYM L2_VERSION_NUM
$ SHOW SYM L2_PASS_NUM
$ update_pass :== @d0$level2$command_proc:update_l2_pass
$ update_lib :== @d0$level2$command_proc:update_l2_lib
$ l2_link :== @D0$D0DAQ:L2_MAIN.LNK
$ L2_STP_LINK :== @D0$FILTER_UTIL:FILTER_STP.LNK
$ DEFINE D0$BETA D0$CMS     !for local cms'ing
$!!!!!!!!probably obsolete!!!!!!!!!!!!!!!!!!! 
$!DEFINE D0$L2PROD L2$NEW,L2$PROD
$! DEFINE L2$BETA ONLINE:[FILTERS.ELN.BETA]
$! DEFINE L2$SRC L2$NEW
$
$EXIT:
$   EXIT
