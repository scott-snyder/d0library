$!
$! @make_d0x [debug]           [debug] to debug, 
$!
$ verify_mode = f$verify(1)
$ on error then goto cleanup
$ on control_y then goto cleanup
$!
$! link?
$!
$ if p1.nes."" 
$ then
$     cdeb = "/noopt/debug"
$     deb = "deb_"
$     swit = "/debug"
$ else
$     cdeb = ""
$     deb = ""
$     SWIT = ""
$ endif
$!
$!  do it
$!
$ show time
$ link'swit'/exe='deb'd0x -
d0$xframe:'deb'xframe/lib/include=(d0x,tree),-
D0$D0USER:'deb'PHYSICS.OLB/LIBRARY,-
D0$ISAZEB:'deb'ISAZEB.OLB/LIBRARY,-
D0$calor_off:'deb'calor_off.olb/library,-
D0$util:'deb'UTIL.OLB/LIBRARY,-
D0$disk:[d0library.UTIL]'deb'util4/opt,-
d0$cernlib:PACKLIB/LIB,GENLIB/LIB,KERNLIB/LIB,-
sys$input/opt
psect_attr=XMQMOTIF,noshr,lcl
sys$share:decw$dxmlibshr/share
sys$share:decw$xlibshr/share
sys$share:vaxcrtl/share
$
$CLEANUP:
$ show time
$ Verify_Mode = f$verify(Verify_Mode)
$ EXIT
