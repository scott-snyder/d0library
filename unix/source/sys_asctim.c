#include "unix.h"
#include <time.h>

long sys$asctim_(short *timlen, char *timbuf, long *tim, long len_timbuf1,
  long len_timbuf2)

/*
C----------------------------------------------------------------------
C-
C-   Name: sys$asctim
C-
C-   Purpose: Convert numeric system time into a character string 
C-            representing the local system time.
C-
C-   Returned value  : 0 (false)     - Operation failed.
C-                     1 (true)      - Operation succeeded.
C-
C-   Fortran usage:
C-
C-      ok = sys$asctim(timlen, timbuf, tim)
C-
C-   Arguments: 
C-
C-     timlen (integer*2, write only) - Number of characters in string.
C-     timbuf (character, write only) - Returned string.
C-     tim (integer*4, 2-word array, read only)
C-                                    - System time in VMS representation.
C-
C-   Created   10-DEC-1991   Herbert Greenlee
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

/* Get length of timbuf. */

  len_timbuf = len_timbuf1;
  if(len_timbuf < 0 || len_timbuf > 256)
    len_timbuf = len_timbuf2;


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

/* Construct character string. */

  len_ctimbuf = strftime(ctimbuf, STRLEN, "%d-%b-%Y %H:%M:%S", tm);
  if(len_ctimbuf > len_timbuf)
    len_ctimbuf = len_timbuf;

/* Return values to calling program. */

  if(timbuf != NULL)
    fstring(ctimbuf, timbuf, len_timbuf);
  if(timlen != NULL)
    *timlen = len_ctimbuf;
  return 1;
}
