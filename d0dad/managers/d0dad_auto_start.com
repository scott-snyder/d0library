$!========================================================================
$!
$! Name      : D0DAD_AUTO_START
$!
$! Purpose   : Start the D0DAD automated jobs
$!
$! Arguments : P1 - queue to run jobs
$!
$! Created  10-JUN-1994   John Hobbs
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$
$ a=f$edit(f$getjpi(f$pid(pidnum),"USERNAME"),"COLLAPSE")
$ if( a.nes."PROMAN" .and. a.nes."SYSTEM" ) 
$ then
$   write sys$OUtput "ILLEGAL USER: ''a'.  Stopping"
$   exit
$ endif
$
$ queue = "SYS$BATCH"
$ if( P1.nes."" ) then queue = P1
$
$ submit := submit/keep/noprint/queue='queue'
$ libtest d0dad
$ d0setup d0dad
$ if( f$search("usr$root:[proman.d0dad]d0dad_define.com") .nes. "" ) then -
                               @usr$root:[proman.d0dad]d0dad_define
$
$ delete/noconf/nolog d0$d0dad$todo:auto_jobs.stop;*
$
$ contxt=0
$ if( f$search("usr$root:[proman.d0dad]d0dad_auto_manage.com").nes. "" ) 
$ then
$  submit/log=usr$root:[proman.d0dad] usr$root:[proman.d0dad]d0dad_auto_manage
$ else
$  submit/log=usr$root:[proman.d0dad] d0$d0dad$managers:d0dad_auto_manage
$ endif 
$
$EXIT:
$   EXIT
