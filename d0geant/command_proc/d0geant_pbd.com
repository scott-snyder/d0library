$!------------------------------------------------
$!
$! Name      : D0GEANT_PBD.COM
$!
$! Purpose   : Make D0GEANT PBD files
$!
$! Arguments : None
$!
$! Created   7-FEB-1990   Harrison B. Prosper
$! Modified 17-JUL-1990   Harrison B. Prosper
$!      Add lines for RCP files etc.
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
$   area = "!D0$D0GEANT:!"
$
$   OPEN/WRITE file 'PBD_NAME'.PBD
$   WF "!D0GEANT!"
$   WF "!V1.10!"
$   WR "  "
$   WR " For each D0GEANT hook supply a user routine, if a routine"
$   WR " is needed by the package ''PBD_NAME'; otherwise hit RETURN"
$   WR "  "
$   INQUIRE USER_ROUTINE "     LURSWT   --- [Read run-time switches(FFIxxx)]"
$   WF "!''USER_ROUTINE'! ------------  LURSWT"
$
$   INQUIRE USER_ROUTINE "     LUESWT   --- [Edit run-time switches(CLNxxx)]"
$   WF "!''USER_ROUTINE'! ------------  LUESWT"
$
$   INQUIRE USER_ROUTINE "     LUBOOK   --- [HBOOK calls for ANLxxx(BOKxxx)]"
$   WF "!''USER_ROUTINE'! ------------  LUBOOK"
$
$   INQUIRE USER_ROUTINE "     LURGEO   --- [Read geometry banks(INIxxx)]"
$   WF "!''USER_ROUTINE'! ------------  LURGEO"
$
$   INQUIRE USER_ROUTINE "     LUSGEO   --- [Set up geometry(GEOxxx)]"
$   WF "!''USER_ROUTINE'! ------------  LUSGEO"
$
$   INQUIRE USER_ROUTINE "     LUINTI   --- [Initialize user commands(INTxxx)]"
$   WF "!''USER_ROUTINE'! ------------  LUINTI"
$
$   INQUIRE USER_ROUTINE "     LUHELP   --- [Print help information(HLPxxx)]"
$   WF "!''USER_ROUTINE'! ------------  LUHELP"
$
$   INQUIRE USER_ROUTINE "     LUMENU   --- [Interactive user menu(MENxxx)]"
$   WF "!''USER_ROUTINE'! ------------  LUMENU"
$
$   INQUIRE USER_ROUTINE "     LUKINE   --- [Define event kinematics(KINxxx)]"
$   WF "!''USER_ROUTINE'! ------------  LUKINE"
$
$   INQUIRE USER_ROUTINE "     LUTREV   --- [Called before each evt(TEVxxx)]"
$   WF "!''USER_ROUTINE'! ------------  LUTREV"
$
$   INQUIRE USER_ROUTINE "     LUSTEP   --- [Called at each step(STPxxx)]"
$   WF "!''USER_ROUTINE'! ------------  LUSTEP"
$
$   INQUIRE USER_ROUTINE "     L0STEP   --- [Used by D0 package only(STPD0)]"
$   WF "!''USER_ROUTINE'! ------------  L0STEP"
$
$   INQUIRE USER_ROUTINE "     LUTRAK   --- [Called after eack track(TRKxxx)]"
$   WF "!''USER_ROUTINE'! ------------  LUTRAK"
$
$   INQUIRE USER_ROUTINE "     LUDIGI   --- [Digitization - end of evt(DIGxxx)]"
$   WF "!''USER_ROUTINE'! ------------  LUDIGI"
$
$   INQUIRE USER_ROUTINE "     LUOUT    --- [Analysis after each evt(ANLxxx)]"
$   WF "!''USER_ROUTINE'! ------------  LUOUT "
$
$   INQUIRE USER_ROUTINE "     LULAST   --- [Program termination(LSTxxx)]"
$   WF "!''USER_ROUTINE'! ------------  LULAST"
$
$   WF "!! Input banks"
$   WF "!! Output banks"
$   WF "!! Dropped banks"
$   WF "!! Object Library e.g.: D0$D0GEANT:D0GEANT.OLB"
$   WF "!! linker switch  e.g.: /LIBRARY"
$   WF "!!"
$   WF "!! RCP file"
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
