$!========================================================================
$!
$! Name      : COPYTXT
$!
$! Purpose   : Copy text files to a directory on a remote UNIX host.  Source
$!             files must be specified as a single VMS file specification, 
$!             which may contain wildcards.  The destination is assumed to 
$!             be a UNIX directory, which need not yet exist.  A directory 
$!             tree may be copied using directory wildcards.  Multiple trees
$!             (i.e. search lists) are not allowed.  Text files in CMS
$!             libraries are ignored.
$!
$! Arguments : P1 - VMS file specification
$!             P2 - Remote host
$!             P3 - Remote user
$!             P4 - Remote directory specification
$!             P5 - Debug flag
$!
$! Created   6-AUG-1991   Herbert Greenlee
$!
$!========================================================================
$ v=f$verify()
$ set noverify
$ on error then goto exit
$ ns = 0
$!
$! Get arguments
$!
$ files = p1
$ rhost = p2
$ ruser = p3
$ rdir  = p4
$ debug = p5
$! if debug.nes."" then set verify
$!
$! set up a temporary empty directory
$!
$ user = f$edit(f$getjpi("","username"),"trim")
$ user_dir = "usr$scratch:[''user']"
$ temp_dir = "usr$scratch:[''user'.temp]"
$ if f$search("usr$scratch:[000000]''user'.dir").eqs."" then -
      create/dir 'user_dir'
$ if f$search("''user_dir'temp.dir").eqs."" then -
      create/dir 'temp_dir'
$ ns = ns + 1
$ if f$search("''temp_dir'*.*",ns).nes."" then delete/log 'temp_dir'*.*;*
$!
$! Loop over files
$!
$ first_file = ""
$ dir_root = ""
$ old_sub_dir = ""
$ unix_dir = rdir
$ file_loop:
$     file = f$search(files)
$     if file.eqs.first_file .or. file.eqs."" then goto file_end
$     if debug.nes."" then sh sym file
$     if first_file.eqs."" then first_file = file
$     if dir_root.eqs.""
$     then
$         dir_root = f$parse(file,,,"device") + f$parse(file,,,"directory") -
                      - "]"
$         if debug.nes."" then sh sym dir_root
$     endif
$!
$! Ignore non-text files 
$!
$     if f$file_attributes(file, "rat").eqs.""  -
          .and. f$parse(file,,,"type").nes.".MEM" -
          .and. f$parse(file,,,"type").nes.".DOC" -
          then goto file_loop
$!
$! Ignore file specifications that do not descend from the root diretory
$!
$     if f$locate(dir_root, file).ne.0
$     then
$         write sys$output "File ''file' not in directory tree"
$         goto file_loop
$     endif
$!
$! Extract the subdirectories
$!
$     sub_dir = f$parse(file,,,"device") + f$parse(file,,,"directory") -
                  - dir_root - "]"
$     if debug.nes."" then sh sym sub_dir
$!
$! Is this a new directory?
$!
$     if sub_dir.nes.old_sub_dir
$     then
$         old_sub_dir = sub_dir
$!
$! Copy any files currently in the temporary area to UNIX and then delete them
$!
$         ns = ns + 1
$         if f$search("''temp_dir'*.*",ns).nes."" 
$         then
$             rshell/username='ruser' 'rhost'  -
                  "if (! -d ''unix_dir' ) mkdir -p ''unix_dir'"
$             rcp/username='ruser' 'temp_dir'*.* "''rhost'::''unix_dir'"
$             delete 'temp_dir'*.*;*
$         endif
$!
$! Calculate the new destination directory
$!
$         unix_dir = sub_dir
$         dir_loop:
$             n = f$locate(".", unix_dir)
$             if n.eq.f$length(sub_dir) then goto dir_end
$             unix_dir = f$extract(0, n, unix_dir) + "/" -
                  + f$extract(n+1, 256, unix_dir)
$             goto dir_loop
$         dir_end:
$         unix_dir = rdir + f$edit(unix_dir, "lowercase")
$         if debug.nes."" then sh sym unix_dir
$     endif
$!
$! Ignore null files in D0library
$!
$     if f$parse(file,,,"name") + f$parse(file,,,"type") .eqs."." then  -
          goto file_loop
$!
$! Special case: ignore files in CMS subdirectories
$!
$     if f$locate(".CMS", sub_dir) .ne. f$length(sub_dir) then goto file_loop
$     if f$locate(".CMS$000", sub_dir) .ne. f$length(sub_dir) then  -
          goto file_loop
$!
$! Got a good file.  Copy it to the temporary area
$!
$     write sys$output "''file' -> ''unix_dir'"
$     copy 'file' 'temp_dir'
$     goto file_loop
$ file_end:
$!
$! Copy any files currently in the temporary area to UNIX and then delete them
$!
$     ns = ns + 1
$     if f$search("''temp_dir'*.*",ns).nes."" 
$     then
$         rshell/username='ruser' 'rhost'  -
              "if (! -d ''unix_dir' ) mkdir -p ''unix_dir'"
$         rcp/username='ruser' 'temp_dir'*.* "''rhost'::''unix_dir'"
$         delete 'temp_dir'*.*;*
$     endif
$ exit:
$ set noverify
$ if v.nes.0 then set verify
