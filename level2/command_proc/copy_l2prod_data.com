$!========================================================================
$!
$! Name      : COPY_L2PROD_DATA
$!
$! Purpose   : Copy data from PRODUCTION GAMMA libraries to the TOP level
$!              L2PROD directory
$!
$! Arguments :
$!
$! Created  16-JAN-1993   Alan M. Jonckheere
$! Modified 22-JAN-1993   James T. Linnemann specific files to copy
$!
$!========================================================================
$ on error then goto ERROR_EXIT
$ on control_y then goto CONTY_EXIT
$
$ defdir = f$environment("DEFAULT")
$
$ write sys$output -
    "%PROD-I-Copying *.DAT, *.RCP ,*.SAVE files to top directory"
$ set def prod$l2prod$root:[000000]
$ copy/log prod$l2prod$root:[level2.command_proc]l2lib_setup.com *
$ copy/log prod$l2prod$root:[level2.command_proc]setup_l2sim.com *
$
$ copy/log prod$l2prod$root:[level2.l2sim]vms_filter_d0user.com *
$
$ copy/log prod$l2prod$root:[LEVEL2.L2SIM]MC_L1SIM.RCP *
$ copy/log prod$l2prod$root:[level2.l2sim]*.stp/exclude=(filt_*.stp) *
$ copy/log prod$l2prod$root:[level2.l2sim]L2SIM.RCP *       ! moved AMJ 12/22/93
$
$! this next section goes away when d0user dropped from release
$ copy/log prod$l2prod$root:[D0USER]D0USER_SETUP.COM *
$ copy/log prod$l2prod$root:[D0USER]D0USER_MENUS.COM *
$ copy/log prod$l2prod$root:[D0USER.MENU]*.MENU *
$
$ copy/log prod$l2prod$root:[CALOR_OFF]CALEVT.RCP *
$ copy/log prod$l2prod$root:[CALOR_OFF]CAHITS.RCP *
$ copy/log prod$l2prod$root:[CALOR_OFF]CADMAKE.RCP *
$ copy/log prod$l2prod$root:[CALOR_OFF]CALICD.RCP *
$ copy/log prod$l2prod$root:[CALOR_OFF]CSF.RCP *
$ copy/log prod$l2prod$root:[CALOR_OFF]CSF_ECEM.RCP *
$ copy/log prod$l2prod$root:[CALOR_OFF]CSF_CCEM.RCP *
$ copy/log prod$l2prod$root:[CALOR_OFF]CSF_ICD.RCP *
$ copy/log prod$l2prod$root:[CALOR_OFF]MC_CALEVT.RCP *
$ copy/log prod$l2prod$root:[CALOR_OFF]MC_CALICD.RCP *
$ copy/log prod$l2prod$root:[CALOR_OFF]CAL_MODULE.RCP *
$ copy/log prod$l2prod$root:[CALOR_OFF]MIX_CSF.RCP *
$ copy/log prod$l2prod$root:[CALOR_OFF]PLT_CSF.RCP *
$
$ copy/log prod$l2prod$root:[STP]CAL_STPFILE.DAT *
$ copy/log prod$l2prod$root:[STP]CAD_STPFILE.DAT *
$ copy/log prod$l2prod$root:[STP]CSF_STPFILE.DAT *
$
$ copy/log prod$l2prod$root:[LEVEL1.DATA]CALORIMETER_TRIGGER.LSO *
$ copy/log prod$l2prod$root:[LEVEL1.DATA]MU_L15_TABLES_V1.RCP *
$ copy/log prod$l2prod$root:[LEVEL1.DATA]*.DAT *
$ copy/log prod$l2prod$root:[LEVEL1.source]MUSIM.RCP  *
$
$!to display run_filter_xxx.dat
$ copy/log prod$l2prod$root:[vms_filter]filter_display.exe   *
$
$! display l2tool.dat
$ copy/log prod$l2prod$root:[l2control]tool_maker.exe *
$! display l2_end reports
$ copy/log prod$l2prod$root:[l2control]filter_end_display.exe  *
$
$! default simulation inputs
$ copy/log prod$l2prod$root:[LEVEL2.L2SIM]*.INFO   *
$ copy/log prod$l2prod$root:[LEVEL2.L2SIM]TRIGGER.RES *
$ copy/log prod$l2prod$root:[LEVEL2.L2SIM]R*.DAT *
$
$! support for making your own configuration files
$ copy/log prod$l2prod$root:[CONFIGS.COOR_SIM]RUN_COOR_SIM.COM *
$ copy/log prod$l2prod$root:[CONFIGS.COOR_SIM]COOR_SIM.EXE *
$ copy/log prod$l2prod$root:[CONFIGS.SOURCE]*.CTL *
$
$ copy/log prod$l2prod$root:[LEVEL2.L2SIM]L2TOOL.DAT *
$
$! support for making contemporary trigger setups
$! copy/log prod$l2prod$root:[configs.source]*.cfg *
$! copy/log prod$l2prod$root:[configs.source]*.trig *
$! copy/log prod$l2prod$root:[configs.source]*.lev1 *
$! copy/log prod$l2prod$root:[configs.source]*.filt *
$! copy/log prod$l2prod$root:[configs.source]*.rs *
$! copy/log prod$l2prod$root:[configs.source]*.req *
$!
$!  Support of building STP files
$
$!??????$ copy/log prod$l2prod$root:[level2.l2sim]*stp*.exe *
$!?$ copy/log prod$l2prod$root:[level2.l2sim]*stp*.exe *
$
$ copy/log prod$l2prod$root:[level2.command_proc]new_stp.com *
$ copy/log prod$l2prod$root:[level2.command_proc]change_version.com *
$ copy/log prod$l2prod$root:[level2.command_proc]l2rcp_setup.com *
$ copy/log prod$l2prod$root:[level2.command_proc]l2rcp_standard.com *
$!assumes holds current muon_l2, samus_l2, l2jxx: needs some cleanup
$ copy/log prod$l2prod$root:[level2.l2sim]*.rcp  *
$
$!danger of overkill here ?
$!??????$ copy/log prod$l2prod$root:[level2.l2sim]*.const  *
$!?$ copy/log prod$l2prod$root:[level2.l2sim]*.const  *
$
$ copy/log prod$l2prod$root:[CDC_UTIL]DTRAKS.RCP *
$ copy/log prod$l2prod$root:[STP]SAM_D0STPFILE.DAT *
$
$! if f$search ("prod$l2prod$root:[*...]*.dat") .nes. ""
$! then
$!     copy/log/exclude=[.old] prod$l2prod$root:[*...]*.dat *
$! endif
$! if f$search ("prod$l2prod$root:[*...]*.rcp") .nes. ""
$! then
$!     copy/log/exclude=[.old] prod$l2prod$root:[*...]*.rcp *
$! endif
$! if f$search ("prod$l2prod$root:[*...]*.save") .nes. ""
$! then
$!     copy/log/exclude=[.old] prod$l2prod$root:[*...]*.save *
$! endif
$! if f$search ("prod$l2prod$root:[d0user]*.com") .nes. ""
$! then
$!     copy/log prod$l2prod$root:[d0user]*.com *
$! endif
$!
$ PURGE *.*
$!
$EXIT:
$ d0$status :== TRUE
$ set def 'defdir'
$ exit
$ERROR_EXIT:
$CONTY_EXIT:
$ d0$status :== FALSE
$ set def 'defdir'
$ exit
