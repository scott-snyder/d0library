$!========================================================================
$!
$! Name      : CMSFETCH
$!
$! Purpose   : CMSFETCH from UNIX host
$!
$! Arguments : P1 - Files to fetch (comma separated list w/wildcards)
$!             P2 - CMS library
$!             P3 - Remote node
$!             P4 - Remote user
$!             P5 - Remote directory specification
$!             P6 - BNL flag
$!             P7 - Debug flag
$!
$! This procedure is normally invoked via rsh from a UNIX host.
$!
$! Created  11-JUL-1991   Herbert Greenlee
$!
$!========================================================================
$ v=f$verify()
$ set noverify
$ on error then goto exit
$!
$! setup beta_util
$!
$ libtest beta_util
$ @d0$beta_util:setup_beta_util
$!
$! get arguments
$!
$ files = p1            ! files to fetch
$ cmslib = p2           ! cms library
$ rhost = p3            ! remote node
$ ruser = p4            ! remote user
$ rdir = p5             ! remote directory
$ bnl = p6
$ debug = p7
$ if debug.nes."" then sh sym/all
$!
$! Use BNL library?
$!
$ if bnl.eqs."1" 
$ then 
$   usecms :== dzero
$ else
$   usecms :== local
$ endif
$!
$! set up a temporary empty directory for fetching and go there
$!
$ current_dir = f$environment("default")
$ user = f$edit(f$getjpi("","username"),"trim")
$ user_dir = "usr$scratch:[''user']"
$ temp_dir = "usr$scratch:[''user'.temp]"
$ if f$search("usr$scratch:[000000]''user'.dir").eqs."" then -
      create/dir 'user_dir'
$ if f$search("''user_dir'temp.dir").eqs."" then -
      create/dir 'temp_dir'
$ set def 'temp_dir'
$ if f$parse(f$environment("default"),,,"device").nes."USR$SCRATCH:" then exit
$ if f$search("''temp_dir'*.*",1).nes."" then delete/log 'temp_dir'*.*;*
$!
$! fetch files
$!
$ cmsfetch 'files' 'cmslib'
$ if debug.nes.""
$ then
$       dir
$       set verify
$ endif
$ if f$search("*.*",2).eqs."" then goto exit
$!
$! send files to remote host
$!
$ rcp/username='ruser' *.* "''rhost'::''rdir'"
$ delete 'temp_dir'*.*;*
$ exit:
$ set def 'current_dir'
$ set noverify
$ if v.nes.0 then set verify
