$!******************************************************!                       
$!     File Name : ALL_DUMP_D0USER.COM                  !                       
$!     Generated by the Program Builder                 !                       
$!     It defines a set of logical names for the        !                       
$!     Run Control Parameters files associated with     !                       
$!     the combined packages.                           !                       
$!     14-AUG-97 08:46:45                               !                       
$!******************************************************!                       
$ RCP_TYPE = P4                                                                 
$ if RCP_TYPE .nes. "" then goto CHECK_RCP_TYPE                                 
$GET_RCP_TYPE:                                                                  
$ inquire/nopunct RCP_TYPE -                                                    
    " Do you wish MC, TB, COSMIC or default RCP file? [Default]: "
$CHECK_RCP_TYPE:                                                                
$ if RCP_TYPE .eqs. "" .or. RCP_TYPE .eqs. "DT" then -                          
   RCP_TYPE = "DEFAULT"                                                         
$ if RCP_TYPE .eqs. "DEFAULT" then goto DEFINE_RCP                              
$ if RCP_TYPE .eqs. "MC" then goto DEFINE_RCP
$ if RCP_TYPE .eqs. "TB" then goto DEFINE_RCP
$ if RCP_TYPE .eqs. "COSMIC" then goto DEFINE_RCP
$ write sys$output " Invalid type "                                             
$ goto GET_RCP_TYPE                                                             
$                                                                               
$DEFINE_RCP:                                                                    
$ write sys$output " Setting RCP file type to ''RCP_TYPE'                       
$                                                                               
$ define CAHITS_RCP D0$CALOR_OFF:CAHITS.RCP
$ define CALICD_RCP D0$CALOR_OFF:CALICD.RCP
$ define CSF_RCP D0$CALOR_OFF:CSF.RCP
$ define CSF_ECEM_RCP D0$CALOR_OFF:CSF_ECEM.RCP
$ define CSF_CCEM_RCP D0$CALOR_OFF:CSF_CCEM.RCP
$ define CSF_ICD_RCP D0$CALOR_OFF:CSF_ICD.RCP
$ define CALEVT_RCP D0$CALOR_OFF:CALEVT.RCP
$ define MURECO_RCP D0$MUON_RECO:MURECO.RCP
$ define DTRAKS_RCP D0$CDC_UTIL:DTRAKS.RCP
$ define VTRAKS_RCP D0$VTX_UTIL:VTRAKS.RCP
$ define FTRAKS_RCP D0$FDC_UTIL:FTRAKS.RCP
$ define TRD_RCP D0$TRD_UTIL:TRD.RCP
$ define ZTRAKS_RCP D0$CD_UTIL:ZTRAKS.RCP
$ define VERTEX_RCP D0$CD_UTIL:VERTEX.RCP
$ define VEES_RCP D0$B_PHYSICS:VEES.RCP
$
$ if RCP_TYPE .eqs. "MC"
$ then
$   define CALICD_RCP D0$CALOR_OFF:MC_CALICD.RCP
$   define MURECO_RCP D0$MUON_RECO:MC_MURECO.RCP
$   define DTRAKS_RCP D0$CDC_UTIL:MC_DTRAKS.RCP
$   define VTRAKS_RCP D0$VTX_UTIL:MC_VTRAKS.RCP
$   define FTRAKS_RCP D0$FDC_UTIL:MC_FTRAKS.RCP
$   define ZTRAKS_RCP D0$CD_UTIL:MC_ZTRAKS.RCP
$ endif
$
$ if RCP_TYPE .eqs. "TB"
$ then
$   define CSF_RCP D0$CALOR_OFF:TB_CSF.RCP
$   define CSF_ECEM_RCP D0$CALOR_OFF:TB_CSF_ECEM.RCP
$   define CSF_CCEM_RCP D0$CALOR_OFF:TB_CSF_CCEM.RCP
$   define CSF_ICD_RCP D0$CALOR_OFF:TB_CSF_ICD.RCP
$ endif
$
$ if RCP_TYPE .eqs. "COSMIC"
$ then
$   define MURECO_RCP D0$MUON_RECO:COSMIC_MURECO.RCP
$   define DTRAKS_RCP D0$CDC_UTIL:COSMIC_DTRAKS.RCP
$   define VTRAKS_RCP D0$VTX_UTIL:COSMIC_VTRAKS.RCP
$   define FTRAKS_RCP D0$FDC_UTIL:COSMIC_FTRAKS.RCP
$   define ZTRAKS_RCP D0$CD_UTIL:COSMIC_ZTRAKS.RCP
$ endif
$
$ IF F$SEARCH("D0$D0USER:D0USER_SETUP.COM") .NES. "" THEN -                     
   @D0$D0USER:D0USER_SETUP.COM -                                                
   D0$D0USER$ROOT:[000000]ALL_DUMP "''P1'" "''P2'" "''P3'" 'RCP_TYPE'           
