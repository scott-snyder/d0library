$!------------------------------------------------
$!
$! Name      : D0USER_PBD
$!
$! Purpose   : Make D0USER PBD files
$!
$! Arguments : None
$!
$! Created  16-MAR-1991   Harrison B. Prosper 
$!
$!------------------------------------------------
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   WR  :== WRITE SYS$OUTPUT
$   WF  :== WRITE file
$   AREA = "!D0$D0USER:!"
$   
$!------------------------------------------------
$!
$!   Get name of package
$!
$!------------------------------------------------
$   INQUIRE PBD_NAME " Enter name of your PACKAGE "
$   IF PBD_NAME .EQS. "" THEN GOTO EXIT
$   
$   WR "  "
$   WR " For each D0USER hook supply the name of your user routine, if one" 
$   WR " is needed by your package ''PBD_NAME'; otherwise hit RETURN"
$   WR "  "
$   
$   COPY NL: 'PBD_NAME'.PBD
$   OPEN/APPEND file 'PBD_NAME'.PBD
$   WF "!D0USER!"
$   WF "!V1.10!"
$   
$   INQUIRE USER_ROUTINE "Process event        (USREVT) "
$   string[0,33] := "!''user_routine'!                                       "
$   string[34,40]:= "Process event        (USREVT)"
$   WF string
$   
$   INQUIRE USER_ROUTINE "Job Initialization   (USRINI) "
$   string[0,33] := "!''user_routine'!                                       "
$   string[34,40]:= "Job Initialization   (USRINI)"
$   wf string
$   
$   INQUIRE USER_ROUTINE "User Dialog          (USDIAL) "
$   string[0,33] := "!''user_routine'!                                       "
$   string[34,40]:= "User Dialog          (USDIAL)"
$   wf string
$   
$   INQUIRE USER_ROUTINE "End of run           (USENDR) "
$   string[0,33] := "!''user_routine'!                                       "
$   string[34,40]:= "End of run           (USENDR)"
$   wf string
$   
$   INQUIRE USER_ROUTINE "Read parameters      (USRPAR) "
$   string[0,33] := "!''user_routine'!                                       "
$   string[34,40]:= "Read parameters      (USRPAR)"
$   wf string
$   
$   INQUIRE USER_ROUTINE "Report Status        (USRPST) "
$   string[0,33] := "!''user_routine'!                                       "
$   string[34,40]:= "Report Status        (USRPST)"
$   wf string
$   
$   INQUIRE USER_ROUTINE "Set Standard Summary (USETSS) "
$   string[0,33] := "!''user_routine'!                                       "
$   string[34,40]:= "Set Standard Summary (USETSS)"
$   wf string
$   
$   INQUIRE USER_ROUTINE "Set User Summary     (USETUS) "
$   string[0,33] := "!''user_routine'!                                       "
$   string[34,40]:= "Set User Summary     (USETUS)"
$   wf string
$   
$   INQUIRE USER_ROUTINE "Event Display        (PXEXEC) "
$   string[0,33] := "!''user_routine'!                                       "
$   string[34,40]:= "Event Display        (PXEXEC)"
$   wf string
$   
$   INQUIRE USER_ROUTINE "Standard Summary     (USRSSM) "
$   string[0,33] := "!''user_routine'!                                       "
$   string[34,40]:= "Standard Summary     (USRSSM)"
$   wf string
$   
$   INQUIRE USER_ROUTINE "User Summary         (USRUSM) "
$   string[0,33] := "!''user_routine'!                                       "
$   string[34,40]:= "User Summary         (USRUSM)"
$   wf string
$   
$   INQUIRE USER_ROUTINE "Write records        (USRWRT) "
$   string[0,33] := "!''user_routine'!                                       "
$   string[34,40]:= "Write records        (USRWRT)"
$   wf string
$   
$   INQUIRE USER_ROUTINE "Reset event          (USRZEV) "
$   string[0,33] := "!''user_routine'!                                       "
$   string[34,40]:= "Reset event          (USRZEV)"
$   wf string
$   
$   INQUIRE USER_ROUTINE "Reset summaries      (USZERO) "
$   string[0,33] := "!''user_routine'!                                       "
$   string[34,40]:= "Reset summaries      (USZERO)"
$   wf string
$   
$   INQUIRE USER_ROUTINE "Define dump          (DMPDUF) "
$   string[0,33] := "!''user_routine'!                                       "
$   string[34,40]:= "Define dump          (DMPDUF)"
$   wf string
$   
$   INQUIRE USER_ROUTINE "Dump request         (DMPUSR) "
$   string[0,33] := "!''user_routine'!                                       "
$   string[34,40]:= "Dump request         (DMPUSR)"
$   wf string
$   
$   INQUIRE USER_ROUTINE "Exit Cleanup         (UQUIT)  "
$   string[0,33] := "!''user_routine'!                                       "
$   string[34,40]:= "Exit Cleanup         (UQUIT) "
$   wf string
$
$   WF "!! List INPUT   banks (one line/bank), end with blank"
$   wf "!! List OUTPUT  banks (one line/bank), end with blank"
$   wf "!! List DROPPED banks (one line/bank), end with blank"
$   WF "!!"
$   WF "!! List LINK OBJECTS, end with blank; List RCP-files, end with blank"
$   WF "!!"
$   CLOSE file
$!------------------------------------------------
$!
$!   Add user package to USER_PACKAGES.PBD
$!
$!------------------------------------------------
$   
$   IF F$SEARCH("USER_PACKAGES.PBD") .EQS. ""
$   THEN
$       COPY NL: USER_PACKAGES.PBD
$   ENDIF
$   
$   FOUND = "FALSE"
$   Package = "!''PBD_NAME'!"
$   
$   OPEN/READ file USER_PACKAGES.PBD
$READ_PACKAGE_LIST:
$   READ/END_OF_FILE=CLOSE_PACKAGE_LIST file package_name
$   READ/END_OF_FILE=CLOSE_PACKAGE_LIST file area_name
$   
$   IF Package_name .EQS. Package
$   THEN
$       FOUND = "TRUE"
$   ENDIF
$   
$   IF FOUND THEN GOTO CLOSE_PACKAGE_LIST
$   GOTO READ_PACKAGE_LIST
$   
$CLOSE_PACKAGE_LIST:
$   CLOSE file
$   IF FOUND THEN GOTO EXIT
$   
$!------------------------------------------------
$!   Package not in list; add to list
$!------------------------------------------------
$   IF F$SEARCH("''PBD_NAME'.PBD") .NES. ""
$   THEN
$       WR "  "
$       WR " Adding package name ''PBD_NAME' to USER_PACKAGES.PBD"
$       WR "  "
$       OPEN/APPEND file USER_PACKAGES.PBD
$       WF "''Package'"
$       WF "''area'"
$       CLOSE file
$   ELSE
$       WR "  "
$       WR " Package file ''PBD_NAME'.PBD NOT found!!!!"
$       WR "  "
$   ENDIF
$EXIT:
$   EXIT
