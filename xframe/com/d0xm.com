$ username = f$edit(f$getjpi("","USERNAME"),"TRIM")
$ time = f$edit(f$time(),"TRIM")
$! mail/subject="D0X ''username' ''time'" nl: drew
$ rd0x :== $d0$xframe:d0x
$ rd0x -title ""D0X: Non Illegitimus Carborundum"""
$exit
