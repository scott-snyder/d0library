#include "unix.h"
#include <unistd.h>
#include <stdio.h>

long lib$rename_file_(char *from, char *to, long len_from, long len_to)

/*
c----------------------------------------------------------------------
c-
c-   purpose and methods : Rename a file
c-
c-   Fortran usage:
c-
c-      ok = lib$rename_file(from, to)
c-
c-   returned value  : 1 (success)
c-                     0 (failure)
c-
c-   arguments:
c-
c-     from (character, read only) - Source filename.
c-     to (character, read only)   - Destination filename.
C-
C-   Created   14-May-1992   Herbert Greenlee
C-
C----------------------------------------------------------------------
*/

{
  char cfrom[STRLEN], cto[STRLEN];
  int len_cfrom, len_cto;

/* Fetch filename arguments.  Convert to c format. */

  cstring(from, len_from, cfrom, STRLEN);
  cstring(to, len_to, cto, STRLEN);

/* Rename. */

  if(rename(cfrom, cto) < 0)
    return 0;
  else
    return 1;
}
