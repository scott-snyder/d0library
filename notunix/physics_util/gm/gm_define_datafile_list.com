$!========================================================================
$!
$! Name      : GM_DEFINE_DATAFILE_LIST
$!
$! Purpose   : Define logicals DATAFILE_LIST and NTUPLE.
$!
$! Arguments : P1   Data File List
$!             P2   Ntuple File Name
$!             
$!
$! Created  26-OCT-1992   Harrison B. Prosper
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   
$   datafilelist= P1
$   ntuplefile  = P2
$   Defdir      = F$ENVIRONMENT("DEFAULT")
$   DefList     = F$TRNLNM("DATAFILE_LIST")
$!================================================
$!  Prompt for DATAFILE list name
$!================================================
$   IF DefList .EQS. ""
$   THEN
$       DefList = "DATAFILE.LIST"
$   ELSE
$       DefList = F$PARSE(deflist,,,"NAME") + F$PARSE(deflist,,,"TYPE")
$   ENDIF
$   
$   IF datafilelist .EQS. ""
$   THEN
$       IF F$SEARCH("GM$OUT:*.LIST") .NES. ""
$       THEN
$           DIRECTORY GM$OUT:*.LIST
$       ENDIF
$       INQUIRE datafilelist "Data File List [''deflist'] "
$   ENDIF
$   IF datafilelist .EQS. "" 
$   THEN 
$       datafilelist = deflist
$   ENDIF
$   DataFileList = "GM$OUT:" + datafilelist
$   
$!================================================
$!   Prompt for NTUPLE file Name
$!================================================
$   name    = F$PARSE(datafilelist,,,"NAME")
$   defntup = name + ".NTUP"
$   
$   IF ntuplefile .EQS. ""
$   THEN
$       INQUIRE ntuplefile "Ntuple Output File [''defntup'] "
$   ENDIF
$   IF ntuplefile .EQS. "" 
$   THEN 
$       ntuplefile = defntup
$   ENDIF
$   name    = F$PARSE(ntuplefile,,,"NAME")
$   ntuplefile  = "GM$OUT:" + ntuplefile
$   postsfile   = "GM$OUT:" + name + ".PS"
$   
$   WRITE SYS$OUTPUT    "  "
$   WRITE SYS$OUTPUT    "  DataFileList     : ''datafilelist'"
$   WRITE SYS$OUTPUT    "  NtupleFile       : ''ntuplefile'"
$   WRITE SYS$OUTPUT    "  PostScriptFile   : ''postsfile'"
$   WRITE SYS$OUTPUT    "  "
$!================================================
$!   DEFINE LOGICALS
$!================================================
$   DEFINE/NOLOG    DATAFILE_LIST   'datafilelist'
$   DEFINE/NOLOG    NTUPLE          'ntuplefile'
$   DEFINE/NOLOG    POSTS           'postsfile'
$   
$EXIT:
$   EXIT
