$!========================================================================
$!
$! Name      : CHECK_UNIX
$!
$! Purpose   : Preprocess and compile fortran code on a remote UNIX node 
$!             for test purposes.
$!
$! Arguments : P1 - Source file(s).  Default is *.for
$!             P2 - Output file.
$!             P3 - Remote node.
$!             P4 - Remote user.
$!
$! Created  13-JAN-1992   Herbert Greenlee
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!
$! Get arguments.  Supply defaults
$!
$ source = "*.for"
$ if p1.nes."" then source = p1
$ output = "sys$output"
$ if p2.nes."" then output = p2
$ node = unix_node
$ if p3.nes."" then node = p3
$ user = unix_user
$ if p4.nes."" then user = p4
$!
$! Make sure input file exists
$!
$ dir/out=nl: 'source'
$ if .not.$status then exit $status
$!
$! Setup remote temporary directory
$!
$ luser = f$edit(f$edit(f$getjpi("","username"), "lowercase"), "trim")
$ pid = f$edit(f$edit(f$getjpi("","pid"), "lowercase"), "trim")
$ tmpdir = "/usr/tmp/''user'/''luser'/''pid'"
$ write sys$output "Creating temporary directory on remote node: ''tmpdir'"
$ rshell/user='user'/input=nl: 'node'  -
      "rm -rf ''tmpdir' >& /dev/null;mkdir -p ''tmpdir'"
$ write sys$output "Copying source files."
$ rcp/user='user' 'source' "''node'::''tmpdir'"
$ write sys$output "Compiling source files on remote node."
$ rshell/user='user'/input=nl:/output='output'/error='output' 'node'  -
"source .login;set nonomatch = 1;cd ''tmpdir';fort -g [a-e]*;fort -g [f-j]*;fort -g [k-o]*;fort -g [p-t]*;fort -g [u-z]*"
$EXIT:
$   EXIT
