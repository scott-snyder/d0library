$!------------------------------------------------
$!
$! Name      : SETUP_SHOWERLIBRARY.COM
$!
$! Purpose   : Define logical names used in SHOWERLIBRARY
$!
$! Arguments : [p1]     Tape name (e.g. BLNd15,blnd16 etc)
$!
$! Created  27-FEB-1989   Harrison B. Prosper, John Womersley
$! Modified 27-JUL-1989   Rajendran Raja
$! Modified 28-SEP-1989   Harrison B. Prosper, Chip Stewart
$! Added CTRAN_RCP, Made output files into logicals
$! Modified  2-OCT-1989   Harrison B. Prosper
$! Use argument p1 to overwrite default data file name
$! Modified 21-FEB-1992   W.G.D.Dharmaratna
$!------------------------------------------------
$   ON ERROR     Then Goto EXIT
$   ON CONTROL_Y Then Goto EXIT
$
$
$!------------------------------------------------
$!   Define general logical names
$!------------------------------------------------
$
$   @DEFINE                     ! Setup logicals
$
$   DIR_DEF         = F$ENVIRONMENT("DEFAULT")
$   DEFINE/NOLOG D0$CAL     'DIR_DEF', -
                                D0$BETA:[CALOR_OFF], -
                                        D0$ROOT:[CALOR_OFF]
$   DEFINE/NOLOG D0$SHLB    'DIR_DEF', -
                                D0$BETA:[SHOWERLIBRARY], -
                                        D0$ROOT:[SHOWERLIBRARY]
$!------------------------------------------------
$!   Define data file to be read
$!------------------------------------------------
$!DEFINE/NOLOG DATAFILE_LIST  USR$ROOT:[DHARMA.SHOWERLIBRARY]G314_2_5.LIST
$!DEFINE/NOLOG DATAFILE_LIST  USR$ROOT:[DHARMA.SHOWERLIBRARY]PT10TO20_01.LIST
$!DEFINE/NOLOG DATAFILE_LIST  USR$ROOT:[DHARMA.SHOWERLIBRARY]PT5TO10_01.LIST
$!DEFINE/NOLOG DATAFILE_LIST  USR$ROOT:[DHARMA.SHOWERLIBRARY]PT20TO40_02.LIST
$!DEFINE/NOLOG DATAFILE_LIST  USR$ROOT:[DHARMA.SHOWERLIBRARY]PT5TO10_02.LIST
$!DEFINE/NOLOG DATAFILE_LIST  USR$ROOT:[DHARMA.SHOWERLIBRARY]PT2TO5.LIST
$!DEFINE/NOLOG DATAFILE_LIST  USR$ROOT:[DHARMA.SHOWERLIBRARY]PT40TO80_317.LIST
$!DEFINE/NOLOG DATAFILE_LIST  USR$ROOT:[DHARMA.SHOWERLIBRARY]G314_40_160.LIST
$!DEFINE/NOLOG DATAFILE_LIST  USR$ROOT:[DHARMA.SHOWERLIBRARY]NELEC1.LIST
$!DEFINE/NOLOG DATAFILE_LIST  FSU3:[PRJ7.SHOWERLIB2]PION442.LIST
$DEFINE/NOLOG DATAFILE_LIST  FSU3:[PRJ7.SHOWERLIB2]PION442.LIST
$DEFINE DATAFILE D0fsu3$MKB400:TOP_PIONSHLB_IS649_G314SS0_D_01.X_RAW01
$!access/tmp kwm
$!    DEFINE/NOLOG DATAFILE KWM$HROOT:[GOUT]PT_5_10_dec.GEN;
$!    DEFINE/NOLOG DATAFILE d0fsu3$mkb400:PT40TO80_02.gen
$
$
$!    SHLIB =  "SHOWERLIBRARY:BLND151614_SHOWERLIBRARY.DAT"
$!    SHAUX  =  "SHOWERLIBRARY:BLND151614_SHOWERLIBRARY.AUX"
$define shlb_new FSU3:[PRJ7.SHOWERLIB2]
$define shlb_d0fsa d0fsax::DBOFFLINE1:[GEANT]
$!    SHLIB =  "shlb_new:SHLB314_SHOWERLIBRARY.DAT"
$!    SHAUX  =  "shlb_new:SHLB314_SHOWERLIBRARY.AUX"
$!    SHLIB =  "shlb_new:SHOWERLIBRARY_2040.DAT"
$!    SHAUX  =  "shlb_new:SHOWERLIBRARY_2_160_junk.AUX"
$!    SHAUX  =  "shlb_new:SHOWERLIBRARY_jets_elec_pion.AUX"
$    SHLIB =  "D0TSAT::PROMAN25:[DATA]SHOWERLIBRARY_FINAL.DAT"
$    SHAUX  =  "D0TSAT::PROMAN25:[DATA]SHOWERLIBRARY_FINAL.AUX"
$
$!------------------------------------------------
$!   Define names of output files
$!------------------------------------------------
$   define usr$out      FSU3:[PRJ7.SHOWERLIB2]
$   DEFINE CALORIMETER_OUT      USR$OUT:CAL.OUT
$   DEFINE HBOOK_SAVE    	USR$OUT:HBOOK.SAVE
$   DEFINE SHOWER_LIBRARY       'SHLIB'
$   DEFINE SHOWER_LIBRARY_AUX   'SHAUX'
$   DEFINE UF13 USR$ROOT2:[RAJA.'p1']
$
$!------------------------------------------------
$!   Define RCP files for framework
$!------------------------------------------------
$   DEFINE CALFRAME_RCP         D0$CAL:CALFRAME.RCP
$   DEFINE CALEVT_RCP           D0$CAL:CALEVT.RCP
$!------------------------------------------------
$!   Define RCP files for packages
$!------------------------------------------------
$   DEFINE SHOWERLIBRARY_RCP    D0$SHLB:SHOWERLIBRARY.RCP
$!------------------------------------------------
$!   Define commands and show logicals
$!------------------------------------------------
$
$
$   WRITE SYS$OUTPUT " "
$   WRITE SYS$OUTPUT " The SHOWER LIBRARY has been defined to "
$   SHOW LOGICAL SHOWER_LIBRARY
$   DIR SHOWER_LIBRARY
$   WRITE SYS$OUTPUT " "
$   WRITE SYS$OUTPUT " The Auxilliary file has been defined to "
$
$   SHOW LOGICAL SHOWER_LIBRARY_AUX
$   DIR SHOWER_LIBRARY_AUX
$   WRITE SYS$OUTPUT "   "
$
$   SHOW LOGICAL DATAFILE_LIST
$
$   SHOW LOGICAL CALORIMETER_OUT
$   WRITE SYS$OUTPUT "   "
$
$   SHOW LOGICAL HBOOK_SAVE
$   WRITE SYS$OUTPUT "   "
$
$   WRITE SYS$OUTPUT " Output files will appear in ''DIR_DEF'"
$   WRITE SYS$OUTPUT "   "
$
$   SETUP DI3000
$
$   WRITE SYS$OUTPUT " Changing RMS_default values to optimize file access"
$   SET RMS_DEF/BUF=8/INDEX
$   SET RMS_DEF/BLOCK=0  !FOR BEST INDEXING
$   SET RMS_DEF/BUF=10
$!
$EXIT:
$   EXIT
