#include "unix.h"
#include <unistd.h>
#include <stdio.h>

long lib$delete_file_(char *file, long len_file)

/*
c----------------------------------------------------------------------
c-
c-   purpose and methods : Delete a file
c-
c-   Fortran usage:
c-
c-      ok = lib$delete_file(file)
c-
c-   returned value  : 1 (success)
c-                     0 (failure)
c-
c-   arguments:
c-
c-     file (character, read only) - Filename.
C-
C-   Created   15-Jul-1992   Herbert Greenlee
C-
C----------------------------------------------------------------------
*/

{
  char cfile[STRLEN];
  int len_cfile;

/* Fetch filename arguments.  Convert to c format. */

  cstring(file, len_file, cfile, STRLEN);

/* Delete. */

  if(unlink(cfile) < 0)
    return 0;
  else
    return 1;
}
