#include <time.h>
#include "unix.h"

long daylight_time_offset_(long *tim)

/*
C----------------------------------------------------------------------
C-
C-   Name: daylight_time_offset
C-
C-   Purpose: Tell whether a given UNIX format time is daylight time.
C-
C-   Returned value  : 0=standard, 1=daylight.
C-
C-   Fortran usage:
C-
C-      hours = daylight_time_offset(tim)
C-
C-   Arguments:
C-
C-      tim - Time in UNIX binary representation.
C-
C-   Created   18-Apr-1992   Herbert Greenlee
C-
C----------------------------------------------------------------------
*/

{
  time_t tloc, tim1;
  long tz;
  struct tm *tm;

  tim1 = *tim;
  tm = localtime(tim);
  if(tm == NULL)
    return 0;
  if(tm->tm_isdst)
    return 1;
  else
    return 0;
}
