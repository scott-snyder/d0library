$!========================================================================
$!
$! Name      : GM_EXPRESS
$!
$! Purpose   : Making plots out of Express Line DST's
$!
$! Arguments : P1   Run Number
$!
$! Created   7-DEC-1992   Harrison B. Prosper, Boaz Klima
$! Modified 15-DEC-1992   Harrison B. Prosper
$!  Add Histogram Summary
$! Modified  5-JAN-1993   Harrison B. Prosper, Boaz Klima
$!  Include LOCK files, Delete PS files
$! Modified 20-JAN-1993   Harrison B. Prosper
$!  Purge, rather than delete GM_*.FOR files
$! Modified  3-APR-1993   Harrison B. Prosper
$!  Create luminosity file
$! Modified  4-MAY-1993   J. Guida  -  add calorimeter hot channel suppression
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$
$   SET DEFAULT GM$DISK:[GM.EXPRESS]
$   WR          :== WRITE SYS$OUTPUT
$   DEFINE/NOLOG    USR$OUT GM$DATA$EXPRESS
$
$   WR "  "
$   WR "GM_EXPRESS: Started    : " + F$TIME()
$   WR "GM_EXPRESS: Output Area: " + F$TRNLNM("USR$OUT")
$   WR "  "
$
$!================================================
$!   Build DataFile List
$!================================================
$
$GetRun:
$   
$   IF P1 .EQS. ""
$   THEN
$       Inq P1 "Enter Run Number "
$   ENDIF
$   IF P1 .EQS. "" THEN GOTO GetRun
$   
$   Run = P1
$   Run     = F$ELEMENT(0,"/",P1)
$   Switch  = F$ELEMENT(1,"/",P1)
$
$
$!================================================
$!   define logicals DATAFILE_LIST, 
$!                   NTUPLE and 
$!                   POSTS
$!================================================
$   
$DefineList:
$   
$   DEFINE_LIST  -
        EXPRESS_'RUN'.LIST  -
        EXPRESS_'RUN'.NTUP
$
$   IF F$SEARCH("USR$OUT:EXPRESS_''run'.LIST") .NES. "" THEN GOTO GetLuminosity
$   
$!================================================
$!   Make sure run is complete by searching RAW$DIR
$!================================================
$
$   IF F$SEARCH("RAW$DIR:GMSTREAM_*''RUN'*.*RAW*") .EQS. "" THEN GOTO GetList
$   WR "GM_EXPRESS: Run ''RUN' not yet complete; try again later"
$   GOTO EXIT
$
$GetList:
$   
$   CREATE_LIST  -
        "GMSTREAM*''RUN'*.X*"  -
        EXPRESS_'RUN'.LIST  -
        EXPRESS_'RUN'.NTUP
$
$   file = "USR$OUT:EXPRESS_''Run'.LIST"
$   IF F$SEARCH(File) .NES. "" THEN GOTO GetLuminosity
$   
$   WR "GM_EXPRESS: Datafile list ''file' NOT FOUND"
$   GOTO EXIT
$   
$!================================================
$!   Compute Integrated luminosity
$!================================================
$   
$GetLuminosity:
$   
$   file = "USR$OUT:EXPRESS_''Run'.LUMI"
$   IF F$SEARCH(File) .NES. "" THEN GOTO MakeNtuple
$   
$   WR "  "
$   WR "GM_EXPRESS: Get Integrated Luminosity using Minimum Bias Count"
$   WR "  "
$   DEFINE/NOLOG MBLUM  'file'
$   GET_MBLUM 'Run' ALL         ! Use ALL stream
$   TYPE/NOPAGE MBLUM
$   WR "  "
$   DEASSIGN MBLUM
$   IF F$EXTRACT(0,1,Switch) .EQS. "L" THEN GOTO EXIT
$   
$!================================================
$!   Create Ntuple
$!================================================
$MakeNtuple:
$
$   IF F$SEARCH("USR$OUT:EXPRESS_''run'.NTUP") .NES. "" THEN GOTO RunPaw
$
$   WR "  "
$   WR "GM_EXPRESS: The following files will be read"
$   WR "  "
$   TYPE/NOPAGE DATAFILE_LIST
$   WR "   "
$
$   CREATE_NTUPLE
$
$   file = "USR$OUT:EXPRESS_''Run'.NTUP"
$   IF F$SEARCH(File) .NES. "" THEN GOTO RunPaw
$   WR "GM_EXPRESS: Ntuple File ''file' NOT FOUND"
$   GOTO EXIT
$
$RunPaw:
$
$   file = "USR$OUT:EXPRESS_''Run'.PS"
$   DEFINE/NOLOG POSTS      'File'
$   
$!================================================
$
$   @D0$GM:CREATE_CALHOT -
        "EXPRESS_''RUN'*.CHOT"  -
        CALHOT_'RUN'.LIST  -
        CHOT_'RUN'.HST  -
$
$!================================================
$!   Define Summary Files
$!================================================
$   Sumfile = "USR$OUT:EXPRESS_''Run'.OUT"
$   DEFINE/NOLOG SUMMARY    'SumFile'
$
$   IF F$SEARCH(file) .NES. "" THEN GOTO PrintPaw
$
$   WR "  "
$   WR "GM_EXPRESS: Run PAW"
$   WR "  "
$   COPY/NOLOG/NOCONF D0$GM$PAW:GM_*.FOR []
$
$   Date = F$EXTRACT(0,11,F$TIME())
$
$   COPY/NOLOG/NOCONF NL:   RUNPAW.COM
$   OPEN/APPEND outfile RUNPAW.COM
$   WRITE outfile "$SETDRV PST"
$   WRITE outfile "$PAWDI3"
$   WRITE outfile "1"
$   WRITE outfile "TITLE        'GM EXPRESS - Run ''RUN' - ''Date''"
$   WRITE outfile "VECTOR/INPUT RUNNO(1)    ''RUN'"
$   WRITE outfile "MACRO/EXEC   GM_EXPRESS"
$   WRITE outfile "EXIT"
$   WRITE outfile "$SETDRV XDW PST"
$   CLOSE outfile
$
$   @RUNPAW
$   PURGE/NOLOG/NOCONF   GM_*.FOR
$   PURGE/NOLOG/NOCONF  RUNPAW.COM
$   RENAME/NOLOG/NOCONF RUNPAW.COM *.*
$   PURGE/NOLOG/NOCONF  LAST.KUMAC
$   RENAME/NOLOG/NOCONF LAST.KUMAC *.*
$
$   IF F$SEARCH(File) .NES. "" THEN GOTO PrintPaw
$   WR "GM_EXPRESS: Histogram File ''file' NOT FOUND"
$   GOTO EXIT
$
$PrintPaw:
$
$   IF F$TRNLNM("SYS$PRINT") .NES. ""   .AND. -
       F$TRNLNM("SYS$PRINT") .NES. "NL:"
$   THEN
$       PRINT/que=d0cr_post/DELETE/parameters=(sides=two) 'file'
$       WR "GM_EXPRESS: Printed file ''file'"
$   ENDIF
$
$EXIT:
$   WR "  "
$   WR "GM_EXPRESS: Ended      : " + F$TIME()
$   EXIT
