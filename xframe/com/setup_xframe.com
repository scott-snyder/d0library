$!========================================================================
$!
$! Name      : SETUP_XFRAME
$!
$! Purpose   : for d0$xframe stuff
$!
$! Arguments : 
$!
$! Created  22-NOV-1992   Drew Baden
$! Modified 17-oct-1992   J. Wilcox
$!                        allow to be run via command line in addition
$!                        to inquiring parameters.
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!
$! get rid of pesky util's logical  OBSOLETE!!!
$!
$!! if f$trnlnm("D0$XFRAME","LNM$PROCESS_TABLE").eqs."D0$UTIL" 
$!! then
$!!   deassign d0$xframe
$!! endif
$ define/nolog d0xuid d0$xframe$source:d0x.uid
$ d0x_color :== "define d0xuid d0$xframe$source:d0x.uid"
$ d0x_bw :== "define d0xuid d0$xframe$source:d0xbw.uid"
$ if p1.nes."" then goto doit
$inq:
$ inquire p1 "Do you want color (C) or black/white (BW)?"
$ ws1 = f$edit(f$extract(0,1,p1),"UPCASE")
$ if ws1.nes."B".and.ws1.nes."C" then goto inq
$ if ws1.eqs."B" then define/nolog d0xuid d0$xframe$source:d0xbw.uid
$doit:
$ link_xframe :== @d0$xframe:link_d0x
$!
$! if we're at fnal, do something different...
$!
$ hn = f$trnlnm("multinet_host_name")
$ fn = f$locate("fnal",hn)
$ fl = f$length(hn)
$ if fn.ne.fl 
$ then
$   d0x :== @d0$xframe:d0xm
$ else
$   d0x :== "$d0$xframe:d0x -title ""D0X: Non Illegitimus Carborundum"""
$ endif
$copy sys$input sys$output

d0x     runs the program

if you want to switch from color to b&w or vice versa, either
do a d0setup xframe again and specify, or use the symbols:

d0x_color     

   or

d0x_bw

$
$ call rcp "qcd_jet_correction_rcp" -
                  "D0$PHYSICS_UTIL$GENERAL:QCD_JET_CORRECTION.RCP"
$ call rcp "ftraks_rcp"     "D0$FDC_UTIL:FTRAKS.RCP"
$ call rcp "tags_map_rcp"   "D0$PHYSICS_UTIL:TAGS_MAP.RCP"
$ call rcp "calevt_rcp"     "D0$CALOR_OFF:CALEVT.RCP"
$ call rcp "cafix_rcp"      "D0$CALOR_OFF:CAFIX.RCP"
$ call rcp "correctem_rcp"  "D0$CALOR_OFF:CORRECTEM.RCP"
$ call rcp "caphel_rcp"     "D0$CALOR_OFF:CAPHEL.RCP"
$ call rcp "cleanmu_rcp"    "D0$MUON_RECO:CLEANMU.RCP"
$ call rcp "cleanem_rcp"    "D0$CALOR_UTIL:CLEANEM.RCP"
$ call rcp "trd_analysis_rcp" "D0$TRD_UTIL:TRD_ANALYSIS.RCP"
$ call rcp "trd_rcp"        "D0$TRD_UTIL:TRD.RCP"
$ call rcp "cahits_rcp"     "D0$CALOR_OFF:CAHITS.RCP"
$ call rcp "calevt_rcp"     "D0$CALOR_OFF:CALEVT.RCP"
$ call rcp "calicd_rcp"     "D0$CALOR_OFF:CALICD.RCP"
$ call rcp "cal_module_rcp" "D0$CALOR_OFF:CAL_MODULE.RCP"
$ call rcp "csf_ccem_rcp"   "D0$CALOR_OFF:CSF_CCEM.RCP"
$ call rcp "csf_ecem_rcp"   "D0$CALOR_OFF:CSF_ECEM.RCP"
$ call rcp "csf_icd_1a_rcp" "D0$CALOR_OFF:CSF_ICD_1A.RCP"
$ call rcp "csf_icd_rcp"    "D0$CALOR_OFF:CSF_ICD.RCP"
$ call rcp "csf_rcp"        "D0$CALOR_OFF:CSF.RCP"
$!
$exit
$!
$ rcp: subroutine
$ if f$trnlnm(p1).eqs.""
$ then
$    define 'p1' 'p2'
$    write sys$output "''p1'   set to ''p2'"
$ endif
$ endsubroutine
