#include <time.h>
#include <string.h>
#include "unix.h"

long lib$day_(long *day_number, long *user_time, long *day_time)

/*
C----------------------------------------------------------------------
C-
C-   Name: lib$day
C-
C-   Purpose: Return the number of the current day relative to the system
C-            zero date.
C-
C-   Returned value  : 0 (false)     - Operation failed.
C-                     1 (true)      - Operation succeeded.
C-
C-   Fortran usage:
C-
C-      ok = lib$day(day_number, user_time, day_time)
C-
C-   Arguments: 
C-
C-     day_number (integer*4, write only) - Number of full days since the
C-                                          since the zero time
C-     user_time (integer*4, 2-word array, read only)
C-                                        - Time to decode (VMS format).
C-     day_time (integer*4, write only)   - Time since midnight in 10 msec 
C-                                          units.
C-
C-   Created   10-DEC-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
*/
{
  long ndate;
  long vms_day, unix_day;
  long vms_time[2], vms_mid[2];
  time_t unix_time, unix_mid;
  char timbuf[40];
  char *s;
  long ok;

/* Get the time to decode in VMS format from user_time or the current local 
   time.  Get both unix and vms format. */

  if(user_time != NULL) {
    vms_time[0] = user_time[0];
    vms_time[1] = user_time[1];
  }
  else {
    ok = sys$gettim_(vms_time);
    if(!ok)
      return 0;
  }
  vms_to_unix_time_(vms_time, &unix_time);

/* Get the string corresponding to the time argument. */

  ok = sys$asctim_(NULL, timbuf, vms_time, 40, 40);
  if(!ok)
    return 0;

/* Find the number of characters in the date portion of the string (terminated
   by a space). */

  s = strchr(timbuf, ' ');
  if(s != NULL)
    ndate = s - timbuf;
  else
    ndate = strlen(timbuf);

/* Convert the date portion of the local time to numeric format.  That is,
   find the numeric time corresponding to midnight of the current day.  
   Convert to UNIX format.*/

  ok = sys$bintim_(timbuf, vms_mid, ndate);
  if(!ok)
    return 0;
  vms_to_unix_time_(vms_mid, &unix_mid);

/* Calculate day.  Correct for the 40,587 day bias between VMS and UNIX. */

  unix_day = unix_mid/86400;
  vms_day = unix_day + 40587;

/* Return day and residual time to calling program. */

  if(day_number != NULL)
    *day_number = vms_day;
  if(day_time != NULL)
    *day_time = 100 * (unix_time - unix_mid);
  return 1;
}
