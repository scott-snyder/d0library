$!
$! @make_d0x [debug]           [debug] to debug, 
$!
$ verify_mode = f$verify(1)
$ on error then goto cleanup
$ on control_y then goto cleanup
$!
$! what version of vms?
$!
$ v1 = f$getsyi("VERSION")
$ write sys$output "VAX Version ''v1''"
$ vv = f$extract(1,1,v1)
$!
$! what version of motif?
$!
$ v1 = f$search("SYS$SHARE:DECW$XMLIBSHR12.EXE")
$ if v1.eqs.""
$ then
$   mv = "1.1"
$ else
$   mv = "1.2"
$ endif
$ write sys$output "MOTIF version ''mv'"
$ if p1.nes."" 
$ then
$     cdeb = "/noopt/debug"
$     deb = "deb_"
$     swit = "/debug/map=d0x.map"
$ else
$     cdeb = ""
$     deb = ""
$     SWIT = ""
$ endif
$!
$!  do it
$!
$ d0lib1 = -
"d0$xframe:''deb'xframe/lib/include=(tree)," +-
"D0$D0USER:''deb'PHYSICS.OLB/LIBRARY," +-
"D0$QCD:''deb'QCD/LIB," +-
"d0$physics_util:''deb'physics_util/lib," +-
"D0$ISAZEB:''deb'ISAZEB.OLB/LIBRARY," +-
"D0$calor_off:''deb'calor_off.olb/library," +-
"D0$CALOR_FILTER:''deb'CALOR_FILTER/LIB," +-
"D0$LEVEL2:''deb'LEVEL2/LIB," +-
"d0$level1:''deb'level1/lib" 
$ d0lib2 = -
"d0$level0:''deb'level0/lib," +-
"D0$CALOR_OFF:''deb'CALOR_OFF.OLB/LIB," +-
"D0$MUON_RECO:''deb'MUON_RECO.OLB/LIB," +-
"D0$util:''deb'UTIL.OLB/LIBRARY," +-
"D0$CDC_UTIL:''deb'cdc_util.olb/lib," +-
"d0$physics_util:''deb'physics_util/lib," +-
"D0$QCD:''deb'QCD/LIB, " +-
"D0$disk:[d0library.UTIL]''deb'util4/opt" 
$ sho sym d0lib1
$ sho sym d0lib2
$!
$!
$!
$ show log d0$xframe$root
$ show time
$!
$!  but first, see if this is an ALPHA vax or not
$!
$ CPU_TYPE = F$GETSYI("NODE_HWTYPE")
$ if cpu_type.eqs."ALPH"
$   then
$!
$!  it's an ALPHA - we assume motif 1.2 and xr5
$!
$   write sys$output "D0X Linking on a VAX/''cpu_type':  ALPHA"
$   cc'cdeb'/obj='deb'd0x.obj d0$xframe$source:d0x.c
$   link'swit'/nonative_only/exe='deb'd0x 'deb'd0x,'d0lib1','d0lib2','cernp',-
sys$input/opt
psect_attr=XMQMOTIF,noshr,lcl
sys$share:decw$mrmlibshr12/share
sys$share:decw$dxmlibshr12/share
sys$share:decw$xmlibshr12/share
sys$share:decw$xtlibshrr5/share
sys$share:decw$xlibshr/share
sys$share:vaxcrtl/lib
$!
$   else
$!
$!  it's the regular vax - if motif 1.2, then use the same shareables
$!  as on the alpha, otherwise use the "old" way
$!
$   write sys$output "D0X Linking on a VAX/''cpu_type'"
$   cc'cdeb'/obj='deb'd0x.obj d0$xframe$source:d0x.c
$   if mv.eqs."1.2"
$   then
$     link'swit'/exe='deb'd0x 'deb'd0x,'d0lib1,'d0lib2','cernp',-
sys$input/opt
psect_attr=XMQMOTIF,noshr,lcl
sys$share:decw$mrmlibshr12/share
sys$share:decw$dxmlibshr12/share
sys$share:decw$xmlibshr12/share
sys$share:decw$xtlibshrr5/share
sys$share:decw$xlibshr/share
$   else
$     link'swit'/exe='deb'd0x 'deb'd0x,'d0lib1','d0lib2','cernp',-
sys$input/opt
psect_attr=XMQMOTIF,noshr,lcl
sys$share:decw$dxmlibshr/share
sys$share:decw$xlibshr/share
sys$share:vaxcrtl/share
$   endif
$ endif
$
$CLEANUP:
$! if f$search("d0x.obj") .nes. "" then delete/log/noconf d0x.obj;*
$ show time
$ Verify_Mode = f$verify(Verify_Mode)
$ EXIT
