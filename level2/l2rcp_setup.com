$!========================================================================
$!
$! Name      : L2RCP_SETUP
$!
$! Purpose   : To define the LOGICALS needed by VMS_FILTER_STP
$!
$! Created  16-OCT-1991   Daniel R. Claes
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$ DEF_DIR = F$ENVIRONMENT( "DEFAULT" )
$!
$! Prepare search list for all RCPs pointing to user's area first
$!
$ DEFINE RCP_AREA            'DEF_DIR',D0$LEVEL2$L2SIM
$!
$ DEFINE CAHITS_RCP          RCP_AREA:CAHITS_MC.RCP   ! MC flags set
$ DEFINE CALEVT_RCP          D0$CALOR_OFF:CALEVT.RCP  
$ DEFINE L2_EM_RCP           RCP_AREA:L2_EM.RCP
$ DEFINE L2ETMISS_RCP        RCP_AREA:L2ETMISS.RCP
$ DEFINE L2ETSUM_RCP         RCP_AREA:L2ETSUM.RCP
$ DEFINE L2JETS_RCP          RCP_AREA:L2JETS.RCP
$ DEFINE L2SETUP_RCP         RCP_AREA:L2SETUP.RCP
$ DEFINE L2TRAK_RCP          RCP_AREA:L2TRAK.RCP
$ DEFINE DTRAKS_RCP          D0$TRACKING_UTIL:DTRAKS.RCP !RCP for MC running
$ DEFINE MUON_L2_RCP         RCP_AREA:MUON_L2.RCP
$ DEFINE MU$CONST            RCP_AREA:D0MUON_GEO.DAT
$!
$ DEFINE L1SIM_RCP           RCP_AREA:L1SIM.RCP      ! Is this needed?
