$!========================================================================
$!
$! Name      : COMPARE_SDIR
$!
$! Purpose   : Compare one subdir with the released or CMS version
$!
$! Arguments : P1   name of subdir of library
$!             P2   name of library
$!
$! symbols used:
$!  COMPARE_MODE    = "CMS" then do CMS compare
$! Created  21-JUN-1992   James T. Linnemann
$! Modified 24-FEB-1994   James T. Linnemann Add SAME.LIS
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$  SAY := WRITE SYS$OUTPUT
$  sdir = P1
$  IF ("''P1'" .EQS. "" ) THEN GOTO QUICK_EXIT
$  IF ("''P2'" .EQS. "" ) THEN GOTO QUICK_EXIT
$  lib = P2
$  libstr := "D0$''lib'$root:[000000...]"
$  SHO SYM libstr
$  SET DEF [.'sdir']
$  SHO DEF
$  current_dir = f$directory()
$  dirspec = "''current_dir'"+-
       "/excl=(*.dir,*.gam,*.gamma,*.lis,*.mms,*.dif,*.OL,*.OLB,*.BNL,"+-
       "AAA_OLB*.DAT,*ALPHA_RELEASE.OPT,GAMMA.DATE,*.OBJ,*.FOR_ELN,"+ -
       "*.CHECK,V''lib'.FOR,*.*-REL_NOTES,FOR00%.DAT)" 
$ DELETE OLDER.LIS;*
$ DELETE SAME.LIS;*
$ DELETE NEWER.LIS;*
$ IF COMPARE_MODE.EQS."CMS" 
$ THEN
$   SAY "CMS COMPARE"
$  COMPARE_CMS 'dirspec' 'lib'
$ ELSE
$   SAY "Compare Released"
$   IF lib.EQS."ZEB" 
$   THEN                 !Zeb is a special case, since files in many subdirs
$       @d0$util:compare_files 'dirspec' d0$zeblst
$   ELSE
$       @d0$util:compare_files 'dirspec' 'libstr'
$   ENDIF
$ ENDIF
$EXIT:
$ SET NOON
$ RENAME OLDER.LIS;* [-]OLDER.LIS;
$ RENAME SAME.LIS;*  [-]SAME.LIS;
$ RENAME NEWER.LIS;* [-]NEWER.LIS;
$ SET DEF [-]
$ SHO DEF
$ SET ON
$QUICK_EXIT:
$   EXIT
