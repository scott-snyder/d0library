$
$ old = f$verify(1)
$
$!------------------------------------------------------------------------------
$! Copy a few files to top directory
$!------------------------------------------------------------------------------
$
$ copy d0$d0dad$managers:setup.com     d0$d0dad:setup.com
$ copy d0$d0dad$managers:d0dad_release d0$d0dad:d0dad_release
$
$ old = f$verify(old)
$
