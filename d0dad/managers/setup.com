$!------------------------------------------------------------------------------
$!
$!  Setup file for d0dad.  Needed only for use of management code.
$!
$!------------------------------------------------------------------------------
$
$
$ catdir = f$trnlnm("d0$d0dad$catalogs") 
$ if( catdir.nes."" ) 
$ then
$   location = f$locate("]",catdir)
$   len = f$length(catdir)
$   if( location.eq.len ) 
$   then 
$     catdir = catdir -":"
$     catdir = f$trnlnm(catdir)
$   endif
$   base = catdir-"]"
$   define d0$d0dad$todo        "''base'.todo]"
$   define d0$d0dad$temp        "''base'.working]"
$   define d0$d0dad$archive     "''base'.uevtcat]"
$   define d0$d0dad$stream_defn "''base'.streams]"
$ endif
$
$ d0dad :== $d0$d0dad:d0dad
$ deb_d0dad :== $d0$d0dad:deb_d0dad
$
$ event_scan :== $d0$d0dad:event_scan
$ deb_event_scan :== $d0$d0dad:deb_event_scan
$
$ check_newruns :== $d0$d0dad:check_newruns
$ deb_check_newruns :== $d0$d0dad:deb_check_newruns
$
$
