$!========================================================================
$!
$! Name      : MAKE_PICK_D0RECO
$!
$! Purpose   : make production PICK_D0RECO.EXE 
$!
$! Arguments : P1= version number, P2= pass number
$!
$! Created  19-AUG-1992   Serban Protopopescu
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$ vrsn = P1
$ IF vrsn .eqs. "" 
$ THEN 
$   inquire vrsn " version number?"
$ ENDIF
$ pass = P2
$ IF pass .eqs. "" 
$ THEN 
$   inquire pass " pass number?"
$ ENDIF
$ PBD/FRAME=D0RECO/PACK=PICK_EVENTS/NAME=PICK/ZEBCOM=2000000-
/HISTORY/PRODID=3/VERSION='vrsn'/PASS='pass'
$! @PICK_D0RECO.LNK
$EXIT:
$   EXIT
