#include "unix.h"
#include <time.h>

long d0_tz_offset_()

/*
C----------------------------------------------------------------------
C-
C-   Name: d0_tz_offset
C-
C-   Purpose: Return the local timezone offset, in hours, relative to
C-            central time..
C-
C-   Returned value  : Time offset in hours.
C-
C-   Fortran usage:
C-
C-      hours = d0_tz_offset()
C-
C-   Created   11-DEC-1991   Herbert Greenlee
C-
C-   Usage Notes:
C-
C-   1.  The offset increases as you move east.
C-
C----------------------------------------------------------------------
*/

{

/* CST offset is 6 hours earlier than UTC. */

  return utc_tz_offset_()+6;
}
