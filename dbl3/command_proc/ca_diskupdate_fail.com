$!========================================================================
$!
$! Name      : DB_TOFFLINE.COM
$!
$! Purpose   : Used to send jounal files to offline
$!
$! Arguments : 
$!
$!  Createded  20-MAY-1992   SHAHRIAR ABACHI
$!
$!========================================================================
$!
$   ON ERROR THEN GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!
$  set noverify
$!		Definitions
$     keeper           = "D0SF11::ABACHI"
$     manager          = "D0::JGUIDA"
$     deputy           = " "
$!
$     time = f$time()
$     mode = f$mode()
$!
$ mail/subj="Update from disk failed" -
   /pers="diskupdate_fail" nla0: 'keeper'
$ mail/subj="Update from disk failed" -
   /pers="_diskupdate_fail" nla0: 'manager'
$!
$EXIT:
$ RETURN
