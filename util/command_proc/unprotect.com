$!========================================================================
$!
$! Name      : UNPROTECT
$!
$! Purpose   : Remove protections (normal security + ACL) from a file
$!
$! Arguments : P1   filename
$!
$! Created   8-JUL-1992   James T. Linnemann
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$ SET PROTECTION =(S:RWED,O:RWED,GROUP:RE,WORLD:RE) 'P1
$ SET ACL/DEFAULT 'P1
$EXIT:
$   EXIT
