#include "unix.h"
#include <time.h>

long sys$numtim_(short *timbuf, long *tim)

/*
C----------------------------------------------------------------------
C-
C-   Name: sys$numtim
C-
C-   Purpose: Convert numeric system time into a seven element integer*2
C-            array of date and time values.
C-
C-   Returned value  : 0 (false)     - Operation failed.
C-                     1 (true)      - Operation succeeded.
C-
C-   Fortran usage:
C-
C-      ok = sys$numtim(timbuf, tim)
C-
C-   Arguments: 
C-
C-     timbuf (integer*2, 7-word array, write only) - Date/time array
C-     tim (integer*4, 2-word array, read only)
C-                                    - System time in VMS representation.
C-
C-   The format of the returned date/time array is:
C-
C-     timbuf(1) = 4-digit year
C-     timbuf(2) = month of year (1-12)
C-     timbuf(3) = day of month (1-31)
C-     timbuf(4) = hour (0-23)
C-     timbuf(5) = minute (0-59)
C-     timbuf(6) = second (0-59)
C-     timbuf(7) = hundredths (always 0)
C-
C-   Created   26-Jun-1992   Herbert Greenlee
C-
C----------------------------------------------------------------------
*/

{
  time_t tloc;
  char ctimbuf[STRLEN];
  int len_ctimbuf;
  long len_timbuf;
  static char *month[12] = {"JAN", "FEB", "MAR", "APR", "MAY", "JUN",
		            "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"};
  struct tm *tm;

/* Get numeric time. */

  if( tim != NULL )
    vms_to_unix_time_(tim, &tloc);
  else
    time(&tloc);
  if(tloc < 0)
    return 0;

/* Decode the numeric time into components. */

  tm = localtime(&tloc);
  if(tm == NULL)
    return 0;

/* Return values to calling program. */

  if(timbuf != NULL) {
    timbuf[0] = tm->tm_year + 1900;
    timbuf[1] = tm->tm_mon + 1;
    timbuf[2] = tm->tm_mday;
    timbuf[3] = tm->tm_hour;
    timbuf[4] = tm->tm_min;
    timbuf[5] = tm->tm_sec;
    timbuf[6] = 0;
  }
  return 1;
}
