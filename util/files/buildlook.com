$!
$!     This is the command file to compile & link LOOK software and
$!     define logical & symbolical names needed.
$!
$!     Serge Kovalyov, March, 12, 1992.
$!
$ write sys$output "Compiling..."
$ cc look
$ write sys$output "Linking..."
$ link look,sys$input:/opt
sys$share:vaxcrtl.exe/share
$ write sys$output "Defining environment..."
$ look:==$'f$environment("DEFAULT")'look
$ define LOOK$HELP "look view$view_hlp.view$hlp"
$ define LOOK$EDIT "ed/tpu %s"
$ write sys$output ""
$ write sys$output "Usage:"
$ write sys$output "  $ look[ file-spec][ file-spec...] "
$ write sys$output "or (for help)"
$ write sys$output "  $ look ?"
