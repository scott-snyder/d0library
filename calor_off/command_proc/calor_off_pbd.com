$!------------------------------------------------
$!
$! Name      : CALOR_OFF_PBD.COM
$!
$! Purpose   : Make CALOR_OFF PBD files
$!
$! Arguments : None
$!
$! Created   7-FEB-1990   Harrison B. Prosper
$! Modified 17-JUL-1990   Harrison B. Prosper 
$!      Add lines for RCP files etc.
$! Modified 28-NOV-1990   Harrison B. Prosper 
$!      Add lines for PXEXEC hook
$!------------------------------------------------
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   WR  :== WRITE SYS$OUTPUT
$   WF  :== WRITE file
$!------------------------------------------------
$!
$!   Get name of package
$!
$!------------------------------------------------
$   INQUIRE PBD_NAME " Enter PACKAGE name "
$   IF PBD_NAME .EQS. "" THEN GOTO EXIT
$   
$   area = "!D0$CALOR_OFF:!"
$   
$   COPY NL: 'PBD_NAME'.PBD
$   OPEN/APPEND file 'PBD_NAME'.PBD
$   WF "!CALOR_OFF!"
$   WF "!V1.10!"
$   WR "  "
$   WR " For each CALOR_OFF hook supply a user routine, if a routine" 
$   WR " is needed by the package ''PBD_NAME'; otherwise hit RETURN"
$   WR "  "
$   INQUIRE USER_ROUTINE "CAL_BEGIN        --- [START of PROGRAM] "
$   WF "!''USER_ROUTINE'! ------------  CAL_BEGIN"
$   
$   INQUIRE USER_ROUTINE "      CAL_BEGIN_RUN    --- [START of RUN    ] "
$   WF "!''USER_ROUTINE'! ------------  CAL_BEGIN_RUN"
$   
$   INQUIRE USER_ROUTINE "            CAL_EVENT        --- [RECONSTRUCTION  ] "
$   WF "!''USER_ROUTINE'! ------------  CAL_EVENT"
$   
$   INQUIRE USER_ROUTINE "            CAL_EVENT_DUMP   --- [DUMPING         ] "
$   WF "!''USER_ROUTINE'! ------------  CAL_EVENT_DUMP"
$   
$   INQUIRE USER_ROUTINE "            CAL_EVENT_RESET  --- [ZEROING         ] "
$   WF "!''USER_ROUTINE'! ------------  CAL_EVENT_RESET"
$   
$   INQUIRE USER_ROUTINE "      CAL_END_RUN      --- [END of RUN      ] "
$   WF "!''USER_ROUTINE'! ------------  CAL_END_RUN"
$   
$   INQUIRE USER_ROUTINE "CAL_END          --- [END of PROGRAM  ] "
$   WF "!''USER_ROUTINE'! ------------  CAL_END"
$   
$   WF "!! ------------  PXEXEC (PIXIE hook)"
$   WF "!! Input banks"
$   WF "!! Output banks"
$   WF "!! Dropped banks"
$   WF "!! List of (link object, followed by link qualifier); end with blank"
$   WF "!D0$CALOR_OFF:''PBD_NAME'.RCP!   ----------- RCP file"
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
