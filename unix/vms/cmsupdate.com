$!========================================================================
$!
$! Name      : CMSUPDATE
$!
$! Purpose   : CMSUPDATE from UNIX host
$!
$! Arguments : P1 - Files to update
$!             P2 - CMS library
$!             P3 - CMS group
$!             P4 - Remark
$!             P5 - Remote node
$!             P6 - Remote user
$!             P7 - BNL flag
$!             P8 - Debug flag
$!
$! This procedure is normally invoked via rsh from a UNIX host.
$!
$! Created  24-JUL-1991   Herbert Greenlee
$!
$!========================================================================
$ v = f$verify()
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
$ cmsgroup = p3         ! cms group
$ remark = """"+p4+"""" ! remark
$ rhost = p5            ! remote node
$ ruser = p6            ! remote user
$ bnl = p7              ! bnl flag
$ debug = p8            ! debug flag
$ if debug.nes."" 
$       then sh sym/all
$       set verify
$ endif
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
$! set up a temporary empty directory for updating and go there
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
$! get files from remote host
$!
$ rcp/username='ruser' "''rhost'::''files'" 'temp_dir'
$ if f$search("*.*",2).eqs."" then goto exit
$!
$! update files
$!
$ set noverify
$ cmsupdate *.*/nobnl 'cmslib' 'cmsgroup' 'remark'
$ if debug.nes."" then set verify
$ delete 'temp_dir'*.*;*
$ exit:
$ set def 'current_dir'
$ set noverify
$ if v.nes.0 then set verify
