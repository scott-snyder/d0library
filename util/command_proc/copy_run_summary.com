$!========================================================================
$!
$! Name      : COPY_RUN_SUMMARY
$!
$! Purpose   : Copy RUN_SUMMARY_xxxx.DAT from FNBIT::D0HS18::COPYCFG$ARCHIVE:
$!              default directory.
$!
$! Arguments : P1   Run Number
$!             P2   Time offset (eg: 7-00:00:00.00)
$!
$! Created   1-DEC-1992   Harrison B. Prosper
$! Modified 19-JAN-1993   Harrison B. Prosper 
$!      Do a directory listing if run not given
$! Modified 28-JAN-1993   Harrison B. Prosper 
$!      Use a search-list
$! Modified  2-FEB-1993   Harrison B. Prosper 
$!      Use FNBIT to allow for access from outside
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   
$   Run     = P1
$   Offset  = P2
$   IF Offset .EQS. ""
$   THEN
$       Offset = "7-00:00:00.00"
$   ENDIF
        
$!================================================
$!   Check which cluster
$!================================================
$   Cluster = F$TRNLNM("SYS$CLUSTER_NODE")
$   IF Cluster .EQS. "D0::"
$   THEN
$       Node = ""
$   ELSE
$       Node = "FNBIT::D0::"
$   ENDIF
$!================================================
$!   Get run=number
$!================================================
$   IF Run .EQS. ""
$   THEN
$       WRITE SYS$OUTPUT "Working..."
$       DIRECTORY/COLUMNS=2 -
                 /SINCE="TODAY-''OFFSET'"  -
                 'Node'COPYCFG$ARCHIVE:RUN_SUMMARY_*.DAT
$       INQUIRE Run "Enter Run Number "
$   ENDIF
$   IF Run .EQS. "" THEN GOTO EXIT
$!================================================
$!   Search for file
$!================================================
$   File = "RUN_SUMMARY_*''Run'.DAT
$   
$   WRITE SYS$OUTPUT "Please be patient"
$   WRITE SYS$OUTPUT "Searching for file ''File'..."
$   
$   FileName = Node + "COPYCFG$ARCHIVE:" + File
$   FileName = F$SEARCH(FileName)
$   IF FileName .EQS. ""
$   THEN
$       WRITE SYS$OUTPUT "Searching LOGGER$BRD..."
$       FileName = Node + "LOGGER$BRD:" + File
$       FileName = F$SEARCH(FileName)
$   ENDIF
$!================================================
$!   Copy file
$!================================================
$   
$   IF FileName .NES. ""
$   THEN
$       WRITE SYS$OUTPUT "Copying file ''File'..."
$       COPY/NOLOG/NOCONF 'FileName' []
$       WRITE SYS$OUTPUT "Done!"
$   ELSE
$       WRITE SYS$OUTPUT "Sorry, no RUN_SUMMARY file for run ''Run'"
$   ENDIF
$   
$EXIT:
$   EXIT
