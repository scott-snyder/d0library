$!------------------------------------------------
$!
$! Name      : D0RECO_SETUP
$!
$! Purpose   : setup logicals and symbols for D0RECO program
$!
$! Arguments : P1 = task name,
$!             P2 = input file name,
$!             P3 = sta output area,
$!             P4 = dst output area,
$!             P5 = RCP_TYPE
$!
$! Created  20-OCT-1989   Serban D. Protopopescu
$! Modified 15-MAY-1992   Boaz Klima, Adam Para, Pushpa Bhat
$!     New naming convention
$! Modified 02-AUG-1993   Qizhong Li-Demarteau added RECO_TYPE for
$!     standard RECO or full tracking
$! Modified 29-OCT-1993   Qizhong Li-Demarteau added RECO_TYPE for
$!     shower library MC data
$!
$!------------------------------------------------
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$ TASK = P1
$ INPUT_DATA = P2
$ EXT_STA=".STA"
$ EXT_DST=".DST"
$ IF INPUT_DATA .EQS. "" THEN INQUIRE INPUT_DATA "Give input data file"
$ IF INPUT_DATA .EQS. "" THEN GOSUB NO_INPUT
$ DEVICE = F$PARSE(INPUT_DATA,,,"DEVICE","SYNTAX_ONLY")
$ DIRECTORY = F$PARSE(INPUT_DATA,,,"DIRECTORY","SYNTAX_ONLY")
$ NAME = F$PARSE(INPUT_DATA,,,"NAME","SYNTAX_ONLY")
$ EXT = F$PARSE(INPUT_DATA,,,"TYPE","SYNTAX_ONLY")
$ IF EXT .EQS. ".X_RAW01" THEN GOSUB X01
$ DEFINE EVENT_DATA 'DEVICE''DIRECTORY''NAME''EXT'
$ OUTPUT_DIR = P3
$ IF OUTPUT_DIR .EQS. "" THEN INQUIRE OUTPUT_DIR "Give STA output directory"
$ IF OUTPUT_DIR .EQS. "" THEN GOSUB NO_OUTPUT
$ DEVICE = F$PARSE(OUTPUT_DIR,,,"DEVICE","SYNTAX_ONLY")
$ DIRECTORY = F$PARSE(OUTPUT_DIR,,,"DIRECTORY","SYNTAX_ONLY")
$ DEFINE STA_OUTPUT_DATA 'DEVICE''DIRECTORY''NAME''EXT_STA'
$ write sys$output " STA_OUTPUT_DATA = ''DEVICE'''DIRECTORY'''NAME'''EXT_STA'  "
$ OUTPUT_DIR = P4
$ IF OUTPUT_DIR .EQS. "" THEN INQUIRE OUTPUT_DIR "Give DST output directory"
$ IF OUTPUT_DIR .EQS. "" THEN GOSUB NO_OUTPUT
$ DEVICE = F$PARSE(OUTPUT_DIR,,,"DEVICE","SYNTAX_ONLY")
$ DIRECTORY = F$PARSE(OUTPUT_DIR,,,"DIRECTORY","SYNTAX_ONLY")
$ DEFINE DST_OUTPUT_DATA 'DEVICE''DIRECTORY''NAME''EXT_DST'
$ write sys$output " DST_OUTPUT_DATA = ''DEVICE'''DIRECTORY'''NAME'''EXT_DST'  "
$ DEFINE SUMMARY_OUTPUT 'DEVICE''DIRECTORY'SUM_'NAME'.OUT
$ DEFINE HISTOGRAMS 'DEVICE''DIRECTORY''NAME'.HST4
$ DEFINE D0RECO_RCP D0$D0RECO:D0RECO.RCP
$ D0RECO     :== $'P1'_D0RECO
$ DEB_D0RECO :== $'P1'_DEB_D0RECO
$ PCA_D0RECO :== $'P1'_PCA_D0RECO
$ NAME = F$PARSE(TASK,,,"NAME","SYNTAX_ONLY")
$ RCP_TYPE = P5
$ if RCP_TYPE .eqs. "" then RCP_TYPE = "DEFAULT"
$ RCP_TYPE == f$edit(RCP_TYPE,"UPCASE,TRIM")
$GET_RECO_TYPE:
$ inquire/nopunct RECO_TYPE -
    " Using SD (Standard D0RECO) or FT (Full Tracking) or SH (SHowerlib)? [SD]: "
$ if RECO_TYPE .eqs. "" .or. RECO_TYPE .eqs. "sd" then -
   RECO_TYPE = "SD"
$ RECO_TYPE == f$edit(RECO_TYPE,"UPCASE,TRIM")
$ if RECO_TYPE .eqs. "SD" then goto DEFINE_RCP
$ if RECO_TYPE .eqs. "FT" then goto DEFINE_RCP
$ if RECO_TYPE .eqs. "SH"
$ then
$   if RCP_TYPE .eqs. "MC" then goto DEFINE_RCP
$ endif
$ write sys$output " Invalid RECO type "
$ goto GET_RECO_TYPE
$DEFINE_RCP:
$ if RECO_TYPE .eqs. "SD" then -
   write sys$output " Setting RECO type to Standard D0RECO "
$ if RECO_TYPE .eqs. "FT" then -
   write sys$output " Setting RECO type to Full Tracking "
$ if RECO_TYPE .eqs. "SH" then -
   write sys$output " Setting RECO type to SHOWERLIB special "
$ if RECO_TYPE .eqs. "FT" .and. RCP_TYPE .eqs. "DEFAULT"
$ then
$   define ZTRAKS_RCP D0$CD_UTIL:FULL_ZTRAKS.RCP
$   define FTRAKS_RCP D0$FDC_UTIL:FULL_FTRAKS.RCP
$ endif
$ if RECO_TYPE .eqs. "FT" .and. RCP_TYPE .eqs. "MC"
$ then
$   define ZTRAKS_RCP D0$CD_UTIL:MC_FULL_ZTRAKS.RCP
$   define FTRAKS_RCP D0$FDC_UTIL:MC_FULL_FTRAKS.RCP
$ endif
$ if RECO_TYPE .eqs. "FT" .and. RCP_TYPE .eqs. "COSMIC"
$ then
$   define ZTRAKS_RCP D0$CD_UTIL:COSMIC_FULL_ZTRAKS.RCP
$   define FTRAKS_RCP D0$FDC_UTIL:COSMIC_FULL_FTRAKS.RCP
$ endif
$ if RECO_TYPE .eqs. "SH" .and. RCP_TYPE .eqs. "MC"
$ then
$!   define CAHITS_RCP D0$CALOR_OFF:CAHITS_MC_VERTEX.RCP
$!   define CAPHEL_RCP D0$CALOR_OFF:CAPHEL_MC_VERTEX.RCP
$!   define MURECO_RCP D0$MUON_RECO:MC_MURECO_MC_VERTEX.RCP
$ define vertex_rcp d0$cd_util:vertex_isajet.rcp
$ endif
$ EXIT:
$ EXIT
$ NO_INPUT:
$ write sys$output "No input file was defined."
$ NO_OUTPUT:
$ write sys$output "No output directory was defined."
$ write sys$output "You must give an input file and output directories."
$ EXIT
$ X01:
$ EXT_STA = ".X_STA01"
$ EXT_DST = ".X_DST01"
$ RETURN
