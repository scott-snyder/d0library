$!========================================================================
$!
$! Name      : COPY_RUN_EVENT_LIST
$!
$! Purpose   : Copy an run/event list from local node to the area
$!              D0FSA::PICK_EVENTS$ROOT:[LIST]
$!
$! Arguments : P1   FileName
$!             P2   Stream
$!             P3   Version
$!
$! Created  22-OCT-1992   Harrison B. Prosper
$! Modified  3-MAY-1993   Harrison B. Prosper 
$!  Shorten Stream name to first 3 characters
$!
$!========================================================================
$   ON ERROR     THEN  GOTO EXIT
$   ON CONTROL_Y THEN  GOTO EXIT
$   
$   WR          :== WRITE SYS$OUTPUT
$   MESSAGE_OFF :== SET MESSAGE/NOFACILITY/NOIDENT/NOSEVERITY/NOTEXT
$   MESSAGE_ON  :== SET MESSAGE/FACILITY/IDENT/SEVERITY/TEXT
$   RemoteArea  :== "D0FSA::PICK_EVENTS$ROOT:[LIST]"
$   
$   Node    = F$TRNLNM("SYS$NODE") - "::"
$   Username= F$EDIT(F$GETJPI("","USERNAME"),"TRIM")
$!================================================
$!   Make a unique ID from the current time
$!================================================
$   Time    = F$cvtime(F$TIME())
$   Day     = F$ELEMENT(0," ",Time)
$   Id      = F$EXTRACT(5,2,Time)+-
              F$EXTRACT(8,2,Time)+F$EXTRACT(11,2,Time)+-
              F$EXTRACT(14,2,Time)+F$EXTRACT(17,2,Time)
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
$       INQUIRE P2 "Run_Event Stream [EXP] "
$   ENDIF
$   IF p2 .EQS. "" 
$   THEN 
$       P2 = "EXP"
$   ENDIF
$   Stream = F$EXTRACT(0,3,P2)
$   
$!================================================
$!   Get version
$!================================================
$   IF P3 .eqs. ""
$   THEN
$       INQUIRE P3 "Run_Event Version [LATEST] "
$   ENDIF
$   IF p3 .EQS. "" 
$   THEN 
$       P3 = "LATEST"
$   ENDIF
$   Version = P3
$   
$!================================================
$!   Create file-name
$!================================================
$   NewFileName =  Stream   + "_" + -
                   Version  + "_" + -
                   Node     + "_" +  -
                   username + "_" +  -
                   Id       + ".REV"
$   
$   RemoteFile  = RemoteArea  + NewFileName
$   
$   WR "  "
$   WR "Copying File ''FileName'"
$   WR "     To File "
$   WR "             ''NewFileName'"
$   WR "                    on the FileServer"
$   INQUIRE answer "Continue [Y] ? "
$   IF F$EXTRACT(0,1,answer) .EQS. "N" THEN GOTO EXIT
$   
$   WR "Working..."
$   COPY/NOLOG/NOCONF/PROT=(W:REWD) 'FileName' 'RemoteFile'
$   WR "Done!"
$   WR "  "
$   
$EXIT:
$   EXIT
