$!========================================================================
$!
$! Name      : GM_CREATE_DATAFILE_LIST
$!
$! Purpose   : For the given file-spec create a DataFile.List from
$!             Data files in the area pointed to by the logical
$!             GM$IN (The default area is D0$DATA$DST).
$!
$! Arguments : P1   File_Spec
$!             P2   DataFile List Name
$!             P3   Ntuple File Name
$!             
$! Created  26-OCT-1992   Harrison B. Prosper
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!================================================
$!   SETUP
$!================================================
$   FileSpec    = P1
$   DataFileList= P2
$   NtupleFile  = P3
$   
$   Defdir      = F$ENVIRONMENT("DEFAULT")
$   DataArea    = F$TRNLNM("GM$IN")
$   IF DataArea .EQS. ""
$   THEN
$   DataArea    = F$TRNLNM("DATAFILE$AREA")
$   ENDIF
$   
$   IF DataArea .EQS. ""
$   THEN
$       DEFINE/NOLOG GM$IN     D0$DATA$DST
$       DataArea = "D0$DATA$DST"
$   ENDIF
$   IF F$LOCATE("]",DataArea) .GE. F$LENGTH(DataArea)
$   THEN
$       DataArea = DataArea - ":"
$       DataArea = DataArea + ":"
$   ENDIF
$!================================================
$!   Get file-spec
$!================================================
$   IF filespec .EQS. ""
$   THEN
$       WRITE SYS$OUTPUT "Data File Area : ''DataArea'"
$       INQUIRE filespec "File-Spec "
$   ENDIF
$   IF filespec .EQS. "" THEN GOTO EXIT
$!================================================
$!   Extract possible qualifier
$!================================================
$   String      = FileSpec
$   FileSpec    = F$ELEMENT(0,"/",String)
$   Qualifier   = F$ELEMENT(1,"/",String)
$   IF Qualifier .EQS. "/"
$   THEN
$       Qualifier = ""
$   ELSE
$       IF Qualifier .NES. ""
$       THEN
$           Qualifier = "/" + Qualifier
$       ENDIF
$   ENDIF
$!================================================
$!   Define logicals DATAFILE_LIST and NTUPLE
$!================================================
$   DEFINE_LIST "''DataFileList'" "''NtupleFile'"
$   DataFileList    = F$TRNLNM("DATAFILE_LIST")
$   ntuplefile      = F$TRNLNM("NTUPLE")
$   postsfile       = F$TRNLNM("POSTS")
$   
$!================================================
$!   Check if files present
$!================================================
$   WRITE SYS$OUTPUT "Searching Data Area ''DataArea' ..."
$   FileSpec    = DataArea + filespec
$   IF F$SEARCH(FileSpec) .EQS. ""
$   THEN
$       WRITE SYS$OUTPUT "*** NO FILES FOUND with FILE-SPEC ''filespec'"
$       GOTO EXIT
$   ENDIF
$   
$!================================================
$!   Create list
$!================================================
$   
$   WRITE SYS$OUTPUT "Creating File     ''DataFileList'"
$   WRITE SYS$OUTPUT "Using File-Spec   ''FileSpec'''Qualifier' ..."
$   DIRECTORY/OUTPUT='DataFileList' -
             /COLUMN=1 -
             /VERSION=1 -
             /NOHEADER -
             /NOTRAIL -
             /NODATE -
             /NOSIZE    'FileSpec' 'Qualifier'
$   
$   WRITE SYS$OUTPUT "Sorting File     ''DataFileList'"
$   OPEN/READ   infile  'DataFileList' 
$   OPEN/WRITE  outfile 'DataFileList'_1
$   
$Loop:
$   
$   READ/END=CloseFile infile record
$   record  = F$ELEMENT(0,";",record)
$   FileName= F$ELEMENT(1,"]",record)
$!   FileName    = F$PARSE(record,,,"NAME") +  F$PARSE(record,,,"TYPE")
$   FileName    = "''DataArea'''FileName'"
$   WRITE outfile FileName
$   
$   GOTO Loop
$   
$CloseFile:
$   
$   CLOSE   infile
$   CLOSE   outfile
$   
$   SORT 'DataFileList'_1 'DataFileList'_1
$   
$   PURGE/NOLOG/NOCONF  'DataFileList'_1
$   RENAME/NOLOG/NOCONF 'DataFileList'_1    'DataFileList'
$   PURGE/NOLOG/NOCONF  'DataFileList'
$   
$   WRITE SYS$OUTPUT "Created Datafile List ''Datafilelist'"
$   WRITE SYS$OUTPUT "  "
$EXIT:
$   EXIT
