$
$ old = f$verify(1)
$
$!------------------------------------------------------------------------------
$! Build D0DAD management code
$!------------------------------------------------------------------------------
$
$ link := link/nodebug
$ libs = "d0$d0dad:d0dad/l,d0$offline_util:offline_util/l,d0$general:general/l"
$ deb_libs = "d0$d0dad:deb_d0dad/l,d0$offline_util:deb_offline_util/l,d0$general:deb_general/l"
$
$ obj_lib = "d0$d0dad:d0dad"
$ deb_obj_lib = "d0$d0dad:deb_d0dad"
$
$!  Link a non-debug version of d0dad
$
$ fort/opt/nodebug d0$d0dad$source:d0dad.for
$ link/exe=d0dad d0dad,'libs','cernp'
$
$!  Link a debug version of d0dad
$
$ fort/noopt/debug/obj=deb_d0dad.obj d0$d0dad$source:d0dad.for
$ link/debug/exe=deb_d0dad  deb_d0dad,'deb_libs','cernp'
$
$! Link the event scanning program
$
$ fort/nowarn/opt/nodeb d0$d0dad$source:event_scan.for
$ link/exe=event_scan event_scan.obj,'libs','cernp'
$
$ fort/nowarn/noopt/debug/obj=deb_event_scan.obj d0$d0dad$source:event_scan.for
$ link/exe=deb_event_scan event_scan.obj,'deb_libs','cernp'
$
$! Link the NEW_RUNS_xxxxxx.lis checking program...
$
$ cc/nowarn/opt/nodebug d0$d0dad$source:check_newruns.c
$ link/exe=check_newruns check_newruns.obj,'libs','cernp'
$
$ cc/nowarn/noopt/debug/obj=deb_check_newruns.obj d0$d0dad$source:check_newruns.c
$ link/exe=deb_check_newruns deb_check_newruns.obj,'deb_libs','cernp'
$
$! Copy up the RCP file...
$
$ copy d0$d0dad$source:d0dad.rcp d0$d0dad:*.*/log
$
$! Clean up...
$
$ delete *d0dad.obj;,*event_scan.obj;,*check_newruns.obj;
$
$
$ old = f$verify(old)
$
