$!------------------------------------------------
$!
$!     Olivier Callot       6-MAY-1988
$!
$!   Build the SOURCES program
$!
$!------------------------------------------------
$   link/notrace/nomap/exe=d0$util:d0sources -
    d0$util:sources/library/include=(d0sources)
