$!========================================================================
$!
$! Name      : FATMEN_EXPAND
$!
$! Purpose   : Expand FATMEN generic filename containing wildcards.
$!
$! Arguments : P1 - Generic filename (may contain wildcards).
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
$       expand = f$search("d0$unix$vms:fatmen_expand.com")
$       if expand.nes."" then @'expand' "''p1'" "''p2'"
$       exit
$ endif
$!
$! Parse first argument
$!
$ comma = f$locate(",", p1)
$ len = f$length(p1)
$ if comma.lt.len
$ then
$       gname = f$extract(0, comma, p1)
$       type = f$extract(comma+1, len-comma-1, p1)
$ else
$       gname = p1
$       type = ""
$ endif
$ gname = f$edit(gname, "upcase")
$ type = f$edit(type, "upcase")
$!
$! Parse generic name for longest directory not containing wildcards.
$!
$ head = ""
$ tail = f$extract(1,f$length(gname)-1,gname)
$ wild_loop:
$       n = f$length(tail)
$       d = f$locate("/",tail)
$       if n.eq.d then goto wild_end
$       dir = f$extract(0,d,tail)
$       if f$locate("*",dir).eq.d .and. -
           f$locate("%",dir).eq.d .and. -
           f$locate(":",dir).eq.d 
$       then
$           head = head + "/" + dir
$           tail = f$extract(d+1,n-d-1,tail)
$       else
$           goto wild_end
$       endif
$       goto wild_loop
$ wild_end:
$!
$! Setup fatmen shell
$!
$ if f$type(fm).nes."STRING" then d0setup fatmen
$!
$! Generate fatmen shell .com file
$!
$ com = "FM_" + f$getjpi("","pid") + ".COM"
$ if f$locate("DIR",type).eq.f$length(type)
$ then
$       open/write temp 'com'
$       write temp "$ fm"
$       write temp "cd ''head'"
$       write temp "ls ''tail' -g"
$       write temp "exit"
$       close temp
$ else
$       open/write temp 'com'
$       write temp "$ fm"
$       write temp "cd ''head'"
$       write temp "ld ''tail'"
$       write temp "exit"
$       close temp
$ endif
$!
$! Execute fatmen shell .com file
$!
$ @'com'
$ del/log 'com';*
$ del/log last.kumac;*
$ set noon
$ del/log/created/before="-1-" fm_*.com;*
