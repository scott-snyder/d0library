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
$!
$!========================================================================
$ DEF_DIR = F$ENVIRONMENT( "DEFAULT" )
$ DEFINE/NOLOG FILTER_DEFAULT   "''F$ENVIRONMENT(""DEFAULT"")'",D0$LEVEL2$L2SIM
$!
$! Setup to run Jan Hoftun's FILTER defining routines
$!
$ FILTER_MAKER     :== @D0$VMS_FILTER:FILTER_MAKER
$ TOOL_MAKER       :== $D0$L2CONTROL:TOOL_MAKER
$!
$! Setup to run COM files changing Trigger Set choice or MC Version
$!
$ CHANGE_VERSION   :== @D0$LEVEL2$COMMAND_PROC:CHANGE_VERSION
$ CHANGE_FILTER    :== @D0$LEVEL2$COMMAND_PROC:CHANGE_FILTER
$!
$! Setup to run COOR_SIM and access the L1SIM/L2SIM configs
$!
$ DEFINE TRIG_FILT_RUN     'DEF_DIR'
$ DEFINE TRIG_FILT_CURRENT 'DEF_DIR'
$ DEFINE COOR_LAST_RUN     'DEF_DIR'
$ DEFINE LEVEL2$RCP_FILES  'DEF_DIR'
$ DEFINE LOGGER$RUN_CONTROL 'DEF_DIR'STREAM.INFO
$ DEFINE RES               'DEF_DIR',D0$CONFIGS$SOURCE
$ DEFINE REQS              'DEF_DIR',D0$CONFIGS$SOURCE
$ COOR_SIM   :== $D0$CONFIGS$COOR_SIM:COOR_SIM.EXE
$ EXPAND_CFG :== @D0$CONFIGS$COOR_SIM:EXPAND_COOR_CONFIG.COM
$!DEFINE D0$LEVEL1$DATA  'DEF_DIR',D0$LEVEL1$ROOT:[DATA] !For local TRIGGER.INFO
$ DEFINE D0$LEVEL1          'DEF_DIR',D0$LEVEL2$L2SIM,  -
                            D0$LEVEL1$ROOT:[000000]   !For local L1SIM_RCP
$ DEFINE D0$CALOR_OFF       'DEF_DIR',D0$LEVEL2$L2SIM,  -
                            D0$CALOR_OFF$ROOT:[000000]   !For local CAHITS_RCP
$!
$! Setup current library configuration for linking/running VMS_FILTER
$!
$ IF p1.EQS."SKIP" .OR. p1.EQS."SK"
$   THEN GOTO SKP
$ ENDIF
$! 
$   RELtest = F$TRNLNM("D0$RELEASE")            ! To accomodate use of this
$   IF RELtest.NES.""                           ! LOGICAL, which MUST be used
$     THEN DEASSIGN D0$RELEASE                  ! by some of our COMMAND files
$   ENDIF                                       ! during a RELEASE procedure
$!
$ NOLIBTEST
$ LIBTEST ALL                       ! EDIT to provide the most current LEVEL2
$! LIBTEST/GAMMA=D0$L2GAMMA ALL     ! compatible libraries for LINKing & running
$! LIBTEST/GAMMA=D0$L2BETA ALL      ! (Current development area on FNALD0)
$!
$SKP:
$!
$! Define the STP file consistent with the MC version of data being studied
$!              (appended by running CHANGE_VERSION)
$!
