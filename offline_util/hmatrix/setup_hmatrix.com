$!------------------------------------------------
$!
$! Name      : SETUP_HMATRIX.COM
$!
$! Purpose   : Setup some CALOR_OFF commands
$!
$! Arguments : 
$!
$! Created 20-DEC-1990   Rajendran Raja 
$!
$!------------------------------------------------
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   
$!------------------------------------------------
$!   Define general logical names
$!------------------------------------------------
$
$NOBNL :== 1    !No BNL files prompted for
$   @DEFINE                     ! Setup standard logicals
$   
$   DIR         = F$ENVIRONMENT("DEFAULT")
$   
$   DEFINE/NOLOG D0$CAL        'DIR', -
                                D0$CMS:[HMATRIX],-
                                D0$CMS:[CALOR_OFF], -
                                        D0$CALOR_OFF$ROOT:[000000], -
                                                D0$CALOR_UTIL$ROOT:[000000]
$   DEFINE/NOLOG D0$TRK        'DIR', -
                                D0$CMS:[TRACKING_UTIL], -
                                        D0$TRACKING_UTIL$ROOT:[000000]
$   DEFINE/NOLOG D0$MUO        'DIR', -
                                D0$CMS:[D0MUON], -
                                        D0$MUON_RECO$ROOT:[000000], -
                                                D0$MUON_UTIL$ROOT:[000000]
$   
$!------------------------------------------------
$!   Define names of output files
$!------------------------------------------------
$   DEFINE STANDARD_OUT         USR$OUT:STANDARD.OUT
$   DEFINE DST_OUT              USR$OUT:DST.OUT
$   DEFINE CALORIMETER_OUT      USR$OUT:CAL.OUT
$   DEFINE CALORIMETER_DMP      USR$OUT:CAL.DMP
$   DEFINE HBOOK_SAVE    	USR$OUT:HBOOK.SAVE
$   DEFINE HMATRIX_SAVE    	USR$EXE:HMATRIX_TOP_SAVE.DAT
$
$!------------------------------------------------
$!   Define RCP files for framework
$!------------------------------------------------
$   DEFINE CALFRAME_RCP         D0$CAL:CALFRAME.RCP
$   DEFINE CALEVT_RCP           D0$CAL:CALEVT.RCP
$   
$!------------------------------------------------
$!   Define RCP files for packages
$!------------------------------------------------
$   DEFINE CTRAN_RCP            D0$CAL:CTRAN.RCP
$   DEFINE TB90_CALOR_UNPACK_RCP        D0$CAL:TB90_CALOR_UNPACK.RCP
$   DEFINE CAHITS_RCP           D0$CAL:CAHITS.RCP
$   DEFINE CALICD_RCP           D0$CAL:CALICD.RCP
$   DEFINE CAJETS_RCP           D0$CAL:CAJETS.RCP
$   DEFINE CAPHEL_RCP           D0$CAL:CAPHEL.RCP
$   DEFINE PLOT_TOWERS_RCP      D0$CAL:PLOT_TOWERS.RCP
$   DEFINE PJET_RCP             D0$CAL:PJET.RCP
$   DEFINE HMATRIX_RCP          D0$CAL:HMATRIX.RCP
$   
$   DEFINE DTRAKS_RCP           D0$TRK:DTRAKS.RCP
$   DEFINE FTRAKS_RCP           D0$TRK:FTRAKS.RCP
$   DEFINE VTRAKS_RCP           D0$TRK:VTRAKS.RCP
$   DEFINE VERTEX_RCP           D0$TRK:VERTEX.RCP
$   DEFINE ZTRAKS_RCP           D0$TRK:ZTRAKS.RCP
$   DEFINE TRD_RCP              D0$TRK:TRD.RCP
$
$   DEFINE MURECO_RCP           D0$MUO:MURECO.RCP
$   
$!------------------------------------------------
$!   Define RCPE files ( Overwrite RCP default files )
$!------------------------------------------------
$   DEFINE CALFRAME_RCPE         CALFRAME.RCPE
$   DEFINE CALEVT_RCPE           CALEVT.RCPE
$   
$!------------------------------------------------
$!   Define Calorimeter geometry/dbl3 files
$!------------------------------------------------
$   DEFINE CAL_STPFILE          D0$STP:CAL_STPFILE.DAT
$   DEFINE DB                   D0TEST::USER1:[ONLINE.DBL3]
$!------------------------------------------------
$!   Execute User's own logicals eg.:
$!      DATAFILE
$!      DATAFILE_LIST
$!      PXPARAMS
$!      PXSCREEN
$   
$    DEFINE/NOLOG USR$SETUP     D0$CAL:SETUP_USER.COM
$    
$    IF F$SEARCH("USR$SETUP") .NES. ""
$    THEN
$        WRITE SYS$OUTPUT "  "
$        WRITE SYS$OUTPUT "  ---- Do USER setup ----"
$        WRITE SYS$OUTPUT "  "
$        @USR$SETUP
$    ENDIF
$   
$!------------------------------------------------
$!   Define commands and show logicals
$!------------------------------------------------
$   
$   SHOW LOGICAL D0$CAL
$   SHOW LOGICAL D0$TRK
$   SHOW LOGICAL D0$MUO
$   WRITE SYS$OUTPUT "   "
$   SHOW LOGICAL DB
$   WRITE SYS$OUTPUT "   "
$   
$   FILE = F$TRNLNM("USR$OUT")
$   WRITE SYS$OUTPUT " Output files will appear in ''FILE'"
$   WRITE SYS$OUTPUT "   "
$!
$EXIT:
$   EXIT
