$!------------------------------------------------
$!
$! Name      : PIXIE_PBD
$!
$! Purpose   : Make PIXIE PBD files
$!
$! Arguments : None
$!
$! Created  23-SEP-1990   Harrison B. Prosper 
$!
$!------------------------------------------------
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   
$   WR  :== WRITE SYS$OUTPUT
$   WF  :== WRITE file
$   AREA = "!D0$PIXIE:!"
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
$   WR " For each PIXIE hook supply the name of your user routine"
$   WR "  "
$   
$   COPY NL: 'PBD_NAME'.PBD
$   OPEN/APPEND file 'PBD_NAME'.PBD
$   WF "!PIXIE!"
$   WF "!V1.10!"
$   
$   INQUIRE USER_ROUTINE "PXINIT --- [Initialization] "
$   WF "!''USER_ROUTINE'!       ----------  PXINIT"
$   
$   INQUIRE USER_ROUTINE "PXEXEC --- [Execution     ] "
$   WF "!''USER_ROUTINE'!       ----------  PXEXEC"
$   
$   rcp_file = "D0$PIXIE:PX_''PBD_NAME'.RCP"
$   
$   WF "!!"
$   WF "!!"
$   WF "!!"
$   WF "!D0$PIXIE:PIXIE.OLB! ----------  LINK-OBJECT"
$   WF "!/LIBRARY!           ----------  LINK-QUALIFIER"
$   WF "!!"
$   WF "!''rcp_file'!       ----------  RCP-FILE"
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
