$!========================================================================
$!
$! Name      : CHECK_LOCAL_LIB
$!
$! Purpose   : Check a whole library
$!
$! Arguments : P1   the name of the library
$! symbols used:
$!   PROCESS_SDIR       a symbol which can be redefined
$!   COMPARE_SDIR       a typical definition; compare contents with release
$!    which uses
$!      COMPARE_MODE        = "CMS" compare with cms rather than release
$!
$! Created  21-JUN-1992   James T. Linnemann
$! Modified 24-FEB-1994   James T. Linnemann Add SAME.LIS
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$! force everything to go into the main PIPE.LIS
$  PIPE SET NOLIS
$  lib = P1
$  IF ("''P1'" .EQS. "" ) THEN GOTO QUICK_EXIT
$! first check whether the library exists!!!
$  libstr := "D0$''lib'"
$  test = f$trnlnm ("''libstr'")
$  IF ("''test'".EQS."") THEN GOTO QUICK_EXIT
$  sho log "''libstr'$ROOT"
$!process the top directory
$  PROCESS_SDIR 'lib' 'lib'
$  SET DEF [.'lib']
$  SHO DEF
$!now process any subdirectories
$  PIPE PROCESS_SDIR *.DIR/EXCL=(CMS.DIR,REL.DIR) NAME 'lib'
$EXIT:
$ SET NOON
$!collect results
$  RENAME OLDER.LIS;* [-]OLDER.LIS;
$  RENAME SAME.LIS;* [-]SAME.LIS;
$  RENAME NEWER.LIS;* [-]NEWER.LIS;
$  SET DEF [-]
$  SHO DEF
$  SET ON
$QUICK_EXIT:
$  EXIT
