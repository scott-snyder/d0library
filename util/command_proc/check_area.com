$!========================================================================
$!
$! Name      : CHECK_AREA
$!
$! Purpose   : traverse the dir tree of an entire release area
$!                  RUN FROM TOP DIR OF THE AREA
$!                  eg SD D0$L2BETA:[000000]
$!
$! Arguments : none
$! SYMBOLS USED:
$!
$!  CHECK_LOCAL_LIB
$!   PROCESS_SDIR       a symbol which can be redefined to do what you want
$!                      for checking libraries, use
$!              PROCESS_SDIR :== @d0$beta_util$command_proc:COMPARE_SDIR
$!   COMPARE_SDIR       a typical definition; compare contents with release
$!    which uses
$!      COMPARE_MODE        = "CMS" compare with cms rather than release
$!
$! Created  21-JUN-1992   James T. Linnemann
$! Modified 24-FEB-1994   James T. Linnemann add SAME.LIS
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$! collect results in PIPE.LIS
$  PIPE SET DEFAULT
$  PIPE SET LIS
$  SHOW SYMBOL COMPARE_MODE
$!tell SAME to write wildcard file names instead of specific versions
$  same_write_wild == "TRUE"
$  PIPE CHECK_LOCAL_LIB *.DIR/EXCL=(CMS.DIR,GENERAL.DIR) NAME
$  IF (F$SEARCH("general.dir").NES."") 
$  THEN
$     SET DEFAULT [.GENERAL]
$     PIPE CHECK_LOCAL_LIB *.DIR/EXCL=(CMS.DIR) NAME
$     RENAME OLDER.LIS;* [-]OLDER.LIS;
$     RENAME NEWER.LIS;* [-]NEWER.LIS;
$     RENAME SAME.LIS;*  [-]SAME.LIS;
$     SET DEFAULT [-]
$  ENDIF
$EXIT:
$  DELETE/SYMBOL/GLOBAL same_write_wild
$ SET NOON
$! now concatenate the lists
$ type = "_RELEASED"
$ IF ("''compare_mode'" .NES. "") THEN type = "_CMS"
$ COPY OLDER.LIS;* ALL_OLDER'type'.LIS
$ COPY SAME.LIS;* ALL_SAME'type'.LIS
$ COPY NEWER.LIS;* ALL_NEWER'type'.LIS
$ DELETE OLDER.LIS;*
$ DELETE SAME.LIS;*
$ DELETE NEWER.LIS;*
$ SET ON
$   EXIT
