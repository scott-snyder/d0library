$!========================================================================
$!
$! Name      : UPDATE_L2_PASS
$!
$! Purpose   : update the level 2 version/pass numbers
$!
$! Arguments : none
$!
$! assumes logicals l2$prod,l2$new, l2$mgr are known
$! Created   3-AUG-1992   James T. Linnemann
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO CLOS
$   ON CONTROL_Y THEN $ GOTO CLOS
$ set default l2$new
$ @l2$mgr:l2prod_setup.com
$ @L2$PROD:define_l2_pass
$ SHOW SYM L2_VERSION_NUM
$ SHOW SYM L2_PASS_NUM
$ INQUIRE/NOPUNCT change " New Version Number? [N] "
$ IF change
$   THEN L2_version_num = l2_version_num + 1
$   L2_PASS_NUM = 0
$ ELSE
$   L2_PASS_NUM = L2_PASS_NUM + 1
$ ENDIF
$ SHOW SYM L2_VERSION_NUM
$ SHOW SYM L2_PASS_NUM
$ l2_pass_str = "''l2_pass_num'"
$ IF l2_pass_num.LT.10 
$ THEN
$     l2_pass_str = "0" + "''l2_pass_num'"
$ ENDIF
$ l2_version_str = "''l2_version_num'"
$ IF l2_version_num.LT.10 
$ THEN
$     l2_version_str = "0" + "''l2_version_num'"
$ ENDIF
$ say :== write sys$output
$   true = -1
$   false = 0
$!record version number
$   vernum = L2_VERSION_NUM
$   ver_str = "$ L2_VERSION_NUM == "
$   vers =  "''ver_str'" + "''vernum'"
$!record pass number
$   passnum = L2_PASS_NUM
$   pass_str = "$ L2_PASS_NUM == "
$   pass = "''pass_str'" + "''passnum'"
$   OPEN/READ/ERROR=CLOS old_version L2$PROD:define_l2_pass.com
$   OPEN/WRITE new_version L2$PROD:define_l2_pass.com
$READ_LOOP:
$   found = false
$   READ/END=CLOS/ERROR=CLOS old_version line
$   IF f$locate("''ver_str'",line) .NE. f$length(line)
$     THEN
$       WRITE new_version vers
$       found = true
$   ENDIF
$   IF f$locate("''pass_str'",line) .NE. f$length(line)
$     THEN
$       WRITE new_version pass
$       found = true
$   ENDIF
$   IF found.EQ.false THEN WRITE new_version line
$   GOTO READ_LOOP
$CLOS:
$   CLOSE new_version
$   CLOSE old_version
$ @L2$PROD:define_l2_pass
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$ DEFINE/USER_MODE SYS$INPUT SYS$COMMAND
$ EV NEW_RELEASE.NOTES
$ RENAME NEW_RELEASE.NOTES -
      l2$new:V_'L2_VERSION_STR'.'L2_PASS_STR'-REL_NOTES
$   pbd/frame=hstrfl/name=vms_filter/hstrfl/prod=2/pass='l2_pass_num'-
        /ver='l2_version_num'/nocompile
$ @l2$mgr:compile_l2prod_pass.com
$EXIT:
$   EXIT
