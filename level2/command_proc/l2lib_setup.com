$!========================================================================
$!
$! Name      : L2LIB_SETUP
$!
$! Purpose   : To define  LOGICAL  search lists  through the local,
$!             test, and official directories, and assorted SYMBOLs
$!             needed to run the  COOR_SIM  and VMS_FILTER programs
$!
$!             Performs  the  currently  recommended   LIBTEST  for
$!             LINKing   VMS_FILTER   and   accessing   the  latest
$!             releases  of  RCPs  and  configuration  files
$!
$! Arguments : p1 = SKIP will perform NO libtesting (option needed during
$!                                                    release procedures)
$! Created  28-NOV-1992   Daniel R. Claes, James T. Linnemann
$! Modified  7-DEC-1992   James T. Linnemann, A. Jonchkeere for production test
$!
$!========================================================================
$ WRITE SYS$OUTPUT "*** Start L2LIB_SETUP ''p1' ***"
$ DEF_DIR = F$ENVIRONMENT( "DEFAULT" )
$ DEFINE/NOLOG FILTER_DEFAULT   "''F$ENVIRONMENT(""DEFAULT"")'",D0$LEVEL2$L2SIM
$!
$! Setup to run Jan Hoftun's FILTER defining routines
$!
$ FILTER_MAKER     :== @D0$VMS_FILTER:FILTER_MAKER
$ TOOL_MAKER       :== $D0$L2CONTROL:TOOL_MAKER
$ FILTER_DISPLAY   :== $D0$VMS_FILTER:FILTER_DISPLAY
$!
$! turn on and off load map
$!
$ DOMAP :== "$d0$beta_util:swap *.lnk nomap ""map/cross"" "
$ NOMAP :== "$d0$beta_util:swap *.lnk  ""map/cross""  nomap"
$!
$! Setup to run COM files changing Trigger Set choice or Monte Carlo Version
$!
$ VMS_LINK         :== @D0$LEVEL2$COMMAND_PROC:L2_VMS.LNK
$ CHANGE_VERSION   :== @D0$LEVEL2$COMMAND_PROC:CHANGE_VERSION
$ CHANGE_FILTER    :== @D0$LEVEL2$COMMAND_PROC:CHANGE_FILTER
$ NEW_STP          :== @D0$LEVEL2$COMMAND_PROC:NEW_STP
$!
$! Setup to link/run L1SIM/L2SIM out of L2PROD development area.
$!
$ LOCAL_PASS       :== @D0$LEVEL2$COMMAND_PROC:LOCAL_PASS
$!
$! Setup to run COOR_SIM and access the L1SIM/L2SIM configs
$!
$ TEST = F$TRNLNM("DBL3$CAL","LNM$PROCESS_TABLE")
$ IF (TEST.EQS."") THEN GOTO NODBL3
$ DEASSIGN DBL3$CAL         ! Return to server for running simulator
$NODBL3:
$!
$ DEFINE L2TOOL             FILTER_DEFAULT:L2TOOL.DAT
$ DEFINE RUN_FILTER         FILTER_DEFAULT:RUN_FILTER_0000001.DAT
$ DEFINE TRIG_FILT_RUN      FILTER_DEFAULT:TRIG_FILT_RUN.INFO
$ DEFINE LEVEL2$RCP_FILES   FILTER_DEFAULT                ! for FILTER_RUN_RCP
$ DEFINE TRIG_FILT_CURRENT  'DEF_DIR'
$ DEFINE TRIG_FILT_DIR      'DEF_DIR'
$ DEFINE COOR_LAST_RUN      'DEF_DIR'
$ DEFINE LOGGER$RUN_CONTROL 'DEF_DIR'STREAM.INFO
$ DEFINE RES                'DEF_DIR',D0$CONFIGS$SOURCE
$ DEFINE resource RES:COOR_SIM.CTL
$ DEFINE REQS               'DEF_DIR',D0$CONFIGS$SOURCE
$ COOR_SIM      :== @D0$CONFIGS$COOR_SIM:RUN_COOR_SIM.COM
$ EXPAND_CFG    :== @D0$CONFIGS$COOR_SIM:EXPAND_COOR_CONFIG.COM
$ GET_TRIGGER   :== D0$CONFIGS$COOR_SIM:GET_TRIGGER.COM
$ SETUP_GENERIC :== @D0$CONFIGS$COOR_SIM:SETUP_GENERIC.COM
$ DEFINE TRGP$DIR            D0$CONFIGS$COOR_SIM
$ TRIGP*ARSE :== $TRGP$DIR:TRIGPARSE -MAP TRGP$DIR:TRIGPARSE.DUMP -BATCH -FILES
$!
$! the following lines redirect CTL, CFG away from the REAL configuration files
$ DEFINE CTL RES
$ DEFINE CFG REQS
$!
$! Set up GRAND_FSUM
$ FSUM :== @d0$level2$command_proc:run_fsum.com
$!
$! Setup current library configuration for linking/running VMS_FILTER
$!
$ IF p1.EQS."SKIP" .OR. p1.EQS."SK"
$ THEN 
$     WRITE SYS$OUTPUT "*** End L2LIB_SETUP ''p1' ***"
$     GOTO SKP
$ ENDIF
$!
$ NOLIBTEST
$ LIBTEST ALL
$! LIBTEST/GAMMA=D0$L2BETA ALL
$ @D0$BETA_UTIL:SETUP_BETA_UTIL
$!CERNP=="D0$FATMEN:FATLIB/LIB,D0$CERNLIB:PACKLIB/LIB,MATHLIB/LIB,KERNLIB/LIB"
$!
$SKP:
$@FILTER_DEFAULT:VMS_FILTER_D0USER.COM "" "" "" DEFAULT
$ DEFINE VMS_FILTER_STP D0$LEVEL2$L2SIM:VMS_FILTER_D_SRV_MU.STP
