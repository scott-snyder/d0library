#include "unix.h"
#include <time.h>

long utc_tz_offset_()

/*
C----------------------------------------------------------------------
C-
C-   Name: utc_tz_offset
C-
C-   Purpose: Return the local timezone offset, in hours, relative to UTC.
C-
C-   Returned value  : Time offset in hours.
C-
C-   Fortran usage:
C-
C-      hours = utc_tz_offset()
C-
C-   Created   20-Mar-1992   Herbert Greenlee
C-
C-   Usage Notes:
C-
C-   1.  The offset is negative for the western hemisphere and increases 
C-       (becomes less negative) as you move east.
C-
C-   2.  The standard time offset is returned even during daylight time.
C-
C----------------------------------------------------------------------
*/

{
  time_t tloc;
  long tz;
  struct tm *local;

/* Extract components of zero time in local standard time. */

  tloc = 0;                         /* 00:00:00 1-Jan-1970 */
  local = localtime(&tloc);
  if(local == NULL)
    return 0;

/* Get local timezone offset relative to UTC (in seconds). */

    tz = local->tm_sec
         +  60*(local->tm_min
         +  60*(local->tm_hour
         +  24*(local->tm_yday
	 + 365*(local->tm_year - 70))));

/* Return hour difference relative to UTC. */

  return tz/3600;
}
