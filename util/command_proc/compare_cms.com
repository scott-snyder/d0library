$!========================================================================
$!
$! Name      : COMPARE_CMS
$!
$! Purpose   : compare a directory the cms version of those files
$!
$!              ATTEMPTS TO CREATE A [.CMS] SUBDIRECTORY IN CURRENT DEFAULT
$!
$!              SEE COMPARE_RELEASED FOR MORE DETAILS
$!
$!       This actually fetches the files so that date comparison is possible.
$!
$! Arguments : P1 dirspec for files to be compared
$!             P2 the cms library in D0
$!
$! example:
$!  COMPARE_CMS d0$L2ALPHA:[filter_util.source] filter_util
$! Created  21-JUN-1992   James T. Linnemann
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$  CMS SET LIB D0$CMS:['P2']
$  SET NOON
$  CREATE/dir [.CMS]
$  SET ON
$  SET DEF [.CMS]
$  PIPE SET CONTINUE
$  PIPE "CMS FETCH/NOHIS" 'P1'  FNAME
$  PUR *.*
$  SET DEF [-]
$@d0$util:compare_files 'P1' [.CMS]
$! clean up the fetched files
$ SET DEF [.CMS]
$ DELETE *.*;*
$ SET DEF [-]
$ UNPROTECT CMS.DIR
$ DELETE CMS.DIR;*
$EXIT:
$   EXIT
