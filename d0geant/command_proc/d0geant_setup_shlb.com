$!========================================================================
$!
$! Name      : D0GEANT_SETUP_SHLB.COM
$!
$! Purpose   : Define logical names needed to use SHOWERLIBRARY
$!             in Geant.
$! Arguments : None
$!
$! Created  21-FEB-1992   W.G.D.Dharmaratna
$! Modifed  26-Apr-1993   H. Greenlee
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
$    SHLIB ="D0TSAT::DBOFFLINE1:[GEANT]SHOWERLIBRARY_final.DAT"
$    SHAUX ="D0TSAT::DBOFFLINE1:[GEANT]SHOWERLIBRARY_JETS_ELEC_FINAL.aux"
$
$   
$   DEFINE SHOWER_LIBRARY       'SHLIB'
$   DEFINE SHOWER_LIBRARY_AUX   'SHAUX'
$
$!------------------------------------------------
$!   Define RCP files for packages
$!------------------------------------------------
$   DEFINE SHOWERLIBRARY_RCP    D0$SHOWERLIBRARY:SHOWERLIBRARY.RCP
$!------------------------------------------------
$!   Define commands and show logicals
$!------------------------------------------------
$   
$   
$   WRITE SYS$OUTPUT " Changing RMS_default values to optimize file access"
$   SET RMS_DEF/BUF=20/INDEX
$   SET RMS_DEF/BLOCK=0  !FOR BEST INDEXING
$   SET RMS_DEF/BUF=10
$
$EXIT:
$   EXIT
