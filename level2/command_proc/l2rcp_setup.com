$!========================================================================
$!
$! Name      : L2RCP_SETUP
$!
$! Purpose   : To define the LOGICALS needed by VMS_FILTER_STP
$!
$! Created  16-OCT-1991   Daniel R. Claes
$!
$!========================================================================
$   ON ERROR     THEN $ exit
$   ON CONTROL_Y THEN $ exit
$   WRITE SYS$OUTPUT "*** Start L2RCP_SETUP ***"
$ DEF_DIR = F$ENVIRONMENT( "DEFAULT" )
$!
$! Prepare search list for all RCPs pointing to user's area first
$!
$ DEFINE L2$RCP              D0$LEVEL2$L2SIM
$ DEFINE RCP_AREA           'DEF_DIR',L2$RCP
$!
$! fixed information for level2
$ DEFINE SA$CONST            D0$STP:SAM_D0STPFILE.DAT
$!
$! Default set is for running current Collider DATA
$!
$! The following part of the file will be rewritten by NEW_STP
$   WRITE SYS$OUTPUT "*** End L2RCP_SETUP ***"
$SKP:
$@d0$level2$command_proc:L2RCP_STANDARD.COM "" "" "" DEFAULT
$ DEFINE CL2HITS_RCP  RCP_AREA:CL2HITS_D.RCP
$ DEFINE MU$CONST     D0$STP$MUON:MUON_L2_6_94.CONST
