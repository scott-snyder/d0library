$!------------------------------------------------
$!
$! Name      : SETUP_CALOR_OFF.COM
$!
$! Purpose   : Define logical names used in CALOR_OFF
$!
$! Arguments : None
$!
$! Created  27-FEB-1989   Harrison B. Prosper, John Womersley
$! Modified 27-JUL-1989   Rajendran Raja
$! Modified 23-MAR-1990   Boaz Klima, Harrison B. Prosper
$!      Add call to SETUP_USER
$! Modified 12-DEC-1991   Harrison B. Prosper
$!      Suppress some print-out, use D0$BETA
$! Modified 28-MAY-1992   Chip Stewart - use PBD setup, DBL3$CAL
$!
$!--------------------------------------------------------------
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$
$        WRITE SYS$OUTPUT "  "
$        WRITE SYS$OUTPUT "  ---- Doing CALOR_OFF SETUP ----"
$        WRITE SYS$OUTPUT "  "
$!------------------------------------------------
$!   Define general logical names
$!------------------------------------------------
$
$   COMB_PACKAGE :== ""
$   IF F$SEARCH("DEFINE.COM") .NES. ""
$   THEN
$       @DEFINE "''P2'" "''P1'"  
$   ELSE
$       WRITE SYS$OUTPUT "  ---- NO DEFINE.COM ----"
$   ENDIF
$   IF COMB_PACKAGE .NES. ""
$   THEN
$       PBD_SETUP = "USR$AREA:" + COMB_PACKAGE + "_CALOR_OFF.COM "
$       IF F$SEARCH(PBD_SETUP) .NES. ""
$       THEN
$           WRITE SYS$OUTPUT " "
$           WRITE SYS$OUTPUT "  ---- PBD SETUP ----"
$           WRITE SYS$OUTPUT "  ---- " + PBD_SETUP + " ------"
$           @'pbd_setup'
$       ELSE
$           WRITE SYS$OUTPUT "  ---- NO PBD SETUP ----"
$           WRITE SYS$OUTPUT "  ---- NO " + PBD_SETUP + " FOUND ------"
$       ENDIF
$   ELSE
$       WRITE SYS$OUTPUT "  ---- NO COMBINED PACKAGE DEFINED ----"
$   ENDIF
$
$   DIR         = F$ENVIRONMENT("DEFAULT")
$
$   DEFINE/NOLOG D0$CAL        'DIR',D0$CALOR_OFF$ROOT:[000000]
$
$!------------------------------------------------
$!   Define names of output files
$!------------------------------------------------
$   DEFINE/NOLOG STANDARD_OUT         USR$OUT:STANDARD.OUT
$   DEFINE/NOLOG DST_OUT              USR$OUT:DST.OUT
$   DEFINE/NOLOG CALORIMETER_OUT      USR$OUT:CAL.OUT
$   DEFINE/NOLOG CALORIMETER_DMP      USR$OUT:CAL.DMP
$   DEFINE/NOLOG HBOOK_SAVE           USR$OUT:HBOOK.SAVE
$   DEFINE/NOLOG HMATRIX_SAVE         USR$OUT:HMATRIX.SAVE
$!------------------------------------------------
$!   Define RCP files for framework
$!------------------------------------------------
$   DEFINE/NOLOG CALFRAME_RCP         D0$CAL:CALFRAME.RCP
$!------------------------------------------------
$!   Define RCPE files ( Overwrite RCP default files )
$!------------------------------------------------
$   DEFINE/NOLOG CALFRAME_RCPE        CALFRAME.RCPE
$!------------------------------------------------
$!   Define Calorimeter geometry/dbl3 files
$!------------------------------------------------
$   @D0$DBL3:DEFINE_DBL3 NEW
$   DEFINE/NOLOG DBMON$GLB            D0HSA::DBONLINE:[DBL3.DBMON]DBMON$GLB.DAT
$!------------------------------------------------
$!   Execute User's own logicals eg.:
$!      DATAFILE
$!      DATAFILE_LIST
$
$    IF F$SEARCH("SETUP_USER.COM") .NES. ""
$    THEN
$        WRITE SYS$OUTPUT "  "
$        WRITE SYS$OUTPUT "  ---- Do USER setup ----"
$        WRITE SYS$OUTPUT "  ---- for DATAFILE and/or DATAFILE_LIST ----"
$        WRITE SYS$OUTPUT "  "
$        @SETUP_USER
$    ELSE
$        WRITE SYS$OUTPUT "  ---- NO SETUP_USER ----"
$    ENDIF
$
$!------------------------------------------------
$!   Define commands and show logicals
$!------------------------------------------------
$
$        WRITE SYS$OUTPUT "  "
$        WRITE SYS$OUTPUT "  ---- Logicals      ----"
$        WRITE SYS$OUTPUT "  "
$   WRITE SYS$OUTPUT "   "
$   SHOW LOGICAL DBL3$CAL
$   WRITE SYS$OUTPUT "   "
$
$   FILE = F$TRNLNM("USR$OUT")
$   WRITE SYS$OUTPUT " Output files will appear in ''FILE'"
$   WRITE SYS$OUTPUT "   "
$!
$EXIT:
$   EXIT
