$!========================================================================
$!
$! Name      : SETUP_LO
$!
$! Purpose   : To setup Leading order calculation program
$!
$! Arguments : NONE
$!
$! Created  20-OCT-1993   Sandor Feher and Patrick Mooney
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$ DEFINE LO$EXE [],d0$PHYSICS_UTIL
$ DEFINE LO_RCP [],d0$PHYSICS_UTIL:LO.RCP
$!
$ RLOD  :== RUN LO$EXE:DEB_LO_MAIN_R.EXE
$ RLOND :== RUN/NODEBUG LO$EXE:DEB_LO_MAIN_R.EXE
$ DEFINE/KEY PF1 "RLOND"/TERMINATE/ECHO
$ DEFINE/KEY PF4 "RLOD"/TERMINATE/ECHO
$ FILE = F$SEARCH("LO.RCP")
$ IF FILE .EQS. "" THEN GOTO TY
$
$ TYPE D0$PHYSICS_UTIL$DOCS:LO_AFTER.DOC
$EXIT:
$   EXIT
$ TY:
$  TYPE d0$PHYSICS_UTIL$DOCS:LO.DOC
$ EXIT
