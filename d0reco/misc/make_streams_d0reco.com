$!========================================================================
$!
$! Name      : MAKE_STREAMS_D0RECO
$!
$! Purpose   : make production STREAMS_D0RECO.EXE 
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
$ PBD/FRAME=D0RECO/PACK=STREAM_FILTER/NAME=STREAMS/ZEBCOM=2000000-
/HISTORY/PRODID=2/VERSION='vrsn'/PASS='pass'
$! @STREAMS_D0RECO.LNK
$EXIT:
$   EXIT
