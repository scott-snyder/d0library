$!========================================================================
$!
$! Name      : D0DAD_AUTO_STOP
$!
$! Purpose   : Stop the D0DAD automated jobs in a controlled manner.
$!
$! Arguments : 
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
$ d0setup d0dad
$ if( f$search("usr$root:[proman.d0dad]d0dad_define.com") .nes. "" ) then -
                               @usr$root:[proman.d0dad]d0dad_define
$
$   create d0$d0dad$todo:auto_jobs.stop;
$
$EXIT:
$   EXIT
