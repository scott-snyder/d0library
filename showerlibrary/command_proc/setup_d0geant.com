$!------------------------------------------------
$!
$! Name      : SETUP_SHOWERLIBRARY.COM
$!
$! Purpose   : Define logical names used in SHOWERLIBRARY
$!
$! Arguments : [p1]     Data file file-spec
$!
$! Created  27-FEB-1989   Harrison B. Prosper, John Womersley
$! Modified 27-JUL-1989   Rajendran Raja 
$! Modified 28-SEP-1989   Harrison B. Prosper, Chip Stewart
$! Added CTRAN_RCP, Made output files into logicals
$! Modified  2-OCT-1989   Harrison B. Prosper 
$! Use argument p1 to overwrite default data file name
$!------------------------------------------------
$   ON ERROR     Then Goto EXIT
$   ON CONTROL_Y Then Goto EXIT
$   
$  
$!------------------------------------------------
$!   Define general logical names
$!------------------------------------------------
$
$NOBNL :== 1     !Don't make BNL files
$   @DEFINE                     ! Setup logicals
$   @d0$cms:[showerlibrary]setup_showerlibrary  !setup showerlibrary logicals
$   DIR_EXE     = "USR$EXE:"    ! Directory containing EXE file
$
$   DIR         = F$ENVIRONMENT("DEFAULT")
$   DEFINE/NOLOG D0$CAL       'DIR', -
                                D0$CMS:[SHOWERLIBRARY], -
                                D0$CMS:[CALOR_OFF], -
                                        D0$ROOT:[CALOR_OFF.SOURCE]
$!
$!------------------------------------------------
$!   Define data file to be read
$!------------------------------------------------
$    SHLIB  =  "D0SF13$DKB0:[RAJA]BLND151614_SHOWERLIBRARY.DAT"
$    SHAUX  =  "D0SF13$DKB0:[RAJA]BLND151614_SHOWERLIBRARY.AUX"
$    CALOUT  =  "SYS$OUTPUT"
$    CALDMP  =  "SHOWERLIBRARY.DMP"
$    HBSAVE  =  "SHOWERLIBRARY.HBK"
$    
$   
$!------------------------------------------------
$!   Define names of output files
$!------------------------------------------------
$   DEFINE SHOWER_LIBRARY       'SHLIB'
$   DEFINE SHOWER_LIBRARY_AUX   'SHAUX'
$   DEFINE SF13 D0SF13$DKB0:[RAJA]
$
$   DEFINE SHOWERLIBRARY_RCP    D0$CAL:SHOWERLIBRARY.RCP
$   
$!------------------------------------------------
$!   Define commands and show logicals
$!------------------------------------------------
$   
$   SHOW LOGICAL SHOWER_LIBRARY
$   WRITE SYS$OUTPUT "   "
$   
$   WRITE SYS$OUTPUT " Output files will appear in ''DIR'"
$   WRITE SYS$OUTPUT "   "
$
$DEFINE FOR003 SYS$OUTPUT       !ZEBRA
$   @D0GEANT.ASS
$set rms_def/buf=127/index
$set rms_def/block=127  !For best indexing
$!
$EXIT:
$   EXIT
