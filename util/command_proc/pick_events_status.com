$!========================================================================
$!
$! Name      : PICK_EVENTS_STATUS
$!
$! Purpose   : Given a file containing a list of run, event numbers
$!             check if they are available on the file-server.
$!
$! Arguments : P1   FileName
$!             P2   Stream
$!
$! Created  28-JAN-1993   Harrison B. Prosper
$! Modified 17-MAR-1993   Harrison B. Prosper 
$!      Use D0$DATA$PICK
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   
$   WR          :== WRITE SYS$OUTPUT
$   MESSAGE_OFF :== SET MESSAGE/NOFACILITY/NOIDENT/NOSEVERITY/NOTEXT
$   MESSAGE_ON  :== SET MESSAGE/FACILITY/IDENT/SEVERITY/TEXT
$   RemoteArea  :== "D0$DATA$PICK:"
$   
$!================================================
$!   Get file name
$!================================================
$   IF P1 .eqs. ""
$   THEN
$       INQUIRE P1 "Run_Event FileName   "
$   ENDIF
$   IF p1 .EQS. "" THEN GOTO EXIT
$   FileName = F$ELEMENT(0,";",P1)
$!================================================
$!   Check if it exists
$!================================================
$   IF F$SEARCH(FileName) .EQS. ""
$   THEN
$       WR "File ''FileName' NOT FOUND"
$       EXIT
$   ENDIF
$!================================================
$!   Get stream name
$!================================================
$   IF P2 .eqs. ""
$   THEN
$       INQUIRE P2 "Run_Event Stream [EXPRESS] "
$   ENDIF
$   IF p2 .EQS. "" 
$   THEN 
$       P2 = "EXPRESS"
$   ENDIF
$   Stream = F$EXTRACT(0,3,P2)
$   
$!================================================
$!   Now search for files
$!================================================
$   WR "  "
$   WR " Working..."
$   WR "  "
$   OutFileName = F$PARSE(FileName,,,"NAME") + ".STATUS"
$   COPY/NOCONF/NOLOG NL:   'OutFileName'
$   OPEN/APPEND outfile     'OutFileName'
$   OPEN/READ   infile      'FileName'
$   
$ReadRecord:
$   
$   READ/END=CLoseFile  infile  record
$   record  = F$EDIT(record,"TRIM,COMPRESS")
$   run     = F$ELEMENT(0," ",record)
$   event   = F$ELEMENT(1," ",record)
$   
$   DataFile= RemoteArea + stream + "_" + run + "_" + event + ".X*"
$   DataFile= F$SEARCH(DataFile)
$   IF DataFile .NES. ""
$   THEN
$       WR DataFile
$       WRITE outfile DataFile
$   ELSE
$       string  =" --> Not Available <-- ''Run' ''Event'"
$       WR string
$       WRITE outfile "!" + string
$   ENDIF
$   
$   GOTO ReadRecord
$   
$CloseFile:
$   
$   CLOSE infile
$   CLOSE outfile
$   WR "  "
$   WR "Summary in file ''OutFileName'"
$   
$EXIT:
$   EXIT
