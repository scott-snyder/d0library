#include "unix.h"
#include <time.h>

long sys$gettim_( long *tim)

/*
C----------------------------------------------------------------------
C-
C-   Name: sys$gettim
C-
C-   Purpose: Return system time in VMS 64-bit format.
C-
C-   Returned value  : 0 (false)     - Operation failed.
C-                     1 (true)      - Operation succeeded.
C-
C-   Fortran usage:
C-
C-      ok = sys$gettim(tim)
C-
C-   Arguments: 
C-
C-     tim (integer*4, 2 word array, write only)  - System time.
C-
C-   Created   10-DEC-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
*/

{
  time_t tloc;

  if(time(&tloc) < 0)
    return 0;
  unix_to_vms_time_(&tloc, tim);
  return 1;
}
