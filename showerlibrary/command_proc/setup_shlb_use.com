$!========================================================================
$!
$! Name      : SETUP_SHLB_USE
$!
$! Purpose   : Define logical names needed to use SHOWERLIBRARY
$!             in Geant.
$! Arguments : None
$!
$! Created  21-FEB-1992   W.G.D.Dharmaratna
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   
$  
$!------------------------------------------------
$!   Define general logical names
$!------------------------------------------------
$
$
$   DEFINE/NOLOG D0$SHLB    d0tsat::dboffline1:[geant.d0geant]
$   DEFINE D0$SHOWERLIBARY [],D0$SHOWERLIBARY$ROOT:[000000]
$
$    SHLIB =  "d0tsat::dboffline1:[geant]SHOWERLIBRARY_V2.DAT"
$    SHAUX  = "d0tsat::dboffline1:[geant]SHOWERLIBRARY_V2.aux"
$   
$   DEFINE SHOWER_LIBRARY       'SHLIB'
$   DEFINE SHOWER_LIBRARY_AUX   'SHAUX'
$
$!------------------------------------------------
$!   Define RCP files for packages
$!------------------------------------------------
$DEFINE SHOWERLIBRARY_RCP D0$SHOWERLIBRARY:SHOWERLIBRARY.RCP
$!------------------------------------------------
$!   Define commands and show logicals
$!------------------------------------------------
$   
$   WRITE SYS$OUTPUT " Changing RMS_default values to optimize file access"
$   SET RMS_DEF/BUF=20/INDEX    ! 8
$   SET RMS_DEF/BLOCK=0  !FOR BEST INDEXING
$   SET RMS_DEF/BUF=10
$
$EXIT:
$   EXIT
