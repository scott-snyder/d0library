#include <unistd.h>
#include "unix.h"

long lib$wait_( float *seconds)

/*
C----------------------------------------------------------------------
C-
C-   Name: lib_wait
C-
C-   Purpose:  Suspend execution for the specified number of seconds.
C-
C-   Returned value: 0 (false) - Operation failed.
C-                   1 (true)  - Operation succeeded.
C-
C-   Fortran usage:
C-
C-      ok = lib$wait(seconds)
C-
C-   Arguments:
C-
C-     seconds (read*4, read only) - Number of seconds to wait.
C-   
C-   Created   5-Oct-1991  Herbert Greenlee
C-
C----------------------------------------------------------------------
*/

{
  unsigned int iseconds;

/* Round interval to the nearest second. */

  iseconds = *seconds + 0.5;

/* Go to sleep. */

  sleep(iseconds);
  return 1;
}
