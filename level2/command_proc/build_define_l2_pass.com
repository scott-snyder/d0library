$!========================================================================
$!
$! Name      : Build_define_l2_pass.com
$!
$! Purpose   : update the level 2 version/pass numbers in define_l2_pass.com
$!
$! Arguments : none
$!
$! assumes logicals l2$prod,l2$new, l2$mgr are known
$! Created   3-AUG-1992   James T. Linnemann
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO ERROR_EXIT
$   ON CONTROL_Y THEN $ GOTO CONTY_EXIT
$ say :== write sys$output
$   SDIR = F$ENVIRONMENT("DEFAULT") ! Starting directory
$   true = -1
$   false = 0
$   @d0$level2$command_proc:define_l2_pass
$!record version number
$   if f$type(prod$cur_ver).NES."" then l2_version_num = f$integer(prod$cur_ver)
$   vernum = l2_version_num
$   ver_str = "$ L2_VERSION_NUM == "
$   vers =  "''ver_str'" + "''vernum'"
$!record pass number
$   if f$type(prod$cur_pass).NES."" then l2_pass_num = f$integer(prod$cur_pass)
$   passnum = l2_pass_num
$   pass_str = "$ L2_PASS_NUM == "
$   pass = "''pass_str'" + "''passnum'"
$!
$   @d0$level2$command_proc:temp_dir.com SET_DEFAULT
$   OPEN/READ old_version d0$level2$command_proc:define_l2_pass.com
$   OPEN/WRITE new_version define_l2_pass.com
$READ_LOOP:
$   found = false
$   READ/END=CLOS old_version line
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
$   RENAME define_l2_pass.com 'L2_DESTdir'
$   @d0$level2$command_proc:temp_dir.com DELETE
$EXIT:
$   d0$status :== TRUE
$   SET DEFAULT 'sdir'
$   EXIT
$ERROR_EXIT:
$CONTY_EXIT:
$   CLOSE new_version
$   CLOSE old_version
$   SET DEFAULT 'sdir'
$   d0$status :== FALSE
$   EXIT
