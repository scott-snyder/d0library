$!========================================================================
$!
$! Name      : FATMEN_FIND
$!
$! Purpose   : Convert generic name to physics filename (stage file if
$!             necessary)
$!
$! Arguments : P1 - Generic filename (no wildcard).
$!
$! Created   5-MAY-1994   Herbert Greenlee
$!
$!========================================================================
$!
$! Initialization
$!
$ if f$type(libtest).nes."STRING"
$ then
$       @d0$disk:[d0library]d0local
$       if f$type(libtest).nes."STRING" then exit
$       libtest fatmen
$       libtest unix
$       define d0$unix$vms [],d0$beta:[gamma.unix.vms],d0$unix$root:[vms]
$       find = f$search("d0$unix$vms:fatmen_find.com")
$       if find.nes."" then @'find' "''p1'" "''p2'"
$       exit
$ endif
$ gname = f$edit(p1, "upcase")
$!
$! Setup fatmen shell
$!
$ if f$type(fm).nes."STRING" then d0setup fatmen
$!
$! Generate fatmen shell .com file
$!
$ com = "FM_" + f$getjpi("","pid") + ".COM"
$ open/write temp 'com'
$ write temp "$ fm"
$ write temp "find ''gname' 88"
$ write temp "exit"
$ close temp
$!
$! Execute fatmen shell .com file
$!
$ @'com'
$ set noon
$ fname = f$search("for088")
$ if( fname.eqs."" ) then fname = f$trnlnm("for088")
$ write sys$output "Physical filename: ''fname'"
$ del/log 'com';*
$ del/log last.kumac;*
$ set noon
$ del/log/created/before="-1-" fm_*.com;*
