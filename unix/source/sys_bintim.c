#include "unix.h"
#include <time.h>

long sys$bintim_(char *timbuf, long *tim, long len_timbuf)

/*
C----------------------------------------------------------------------
C-
C-   Name: sys$bintim
C-
C-   Purpose: Convert Ascii time to numeric time.  This routine is the
C-            inverse of sys$asctim.
C-
C-   Returned value  : 0 (false)     - Operation failed.
C-                     1 (true)      - Operation succeeded.
C-
C-   Fortran usage:
C-
C-      ok = sys$bintim(timbuf, tim)
C-
C-   Arguments: 
C-
C-     timbuf (character, read only) - String containig date & time.
C-     tim (integer*4, 2-word array, write only)
C-                                   - Numeric time in VMS representation
C-
C-   The input string format is dd-mmm-yyyy hh:mm:ss.  The day, month and year
C-   fields are mandatory.  The time-of-day fields are individually optional.
C-   The year may be specified in 2-digit or 4-digit notation.
C-
C-   Created   10-DEC-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
*/

{
  int i,j;
  char *s;
  time_t tloc, difftime;
  char ctimbuf[STRLEN];
  static char *month[12] = {"JAN", "FEB", "MAR", "APR", "MAY", "JUN",
		            "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"};
  char *field[6];
  int year, mon, day, hour, min, sec;
  struct tm *tm;

/* Make a copy of the input string in c format. */

  cstring(timbuf, len_timbuf, ctimbuf, STRLEN);

/* Get pointers to the start of each field of the input string.  Field 
   separators are any of the following characters:  "/-:.<whitespace>".
   Field separators are replaced with nulls. */

  j = 0;
  for(i=0; i<6; ++i) {

  /* Skip over leading whitespace. */

    while(isspace(ctimbuf[j]))
      ++j;
    field[i] = &ctimbuf[j];

  /* Find field terminator and replace it with a null, but do not advance
     past a pre-existing null. */

    while(ctimbuf[j] != '/' &&
	  ctimbuf[j] != '-' &&
	  ctimbuf[j] != ':' &&
	  ctimbuf[j] != '.' &&
	  ctimbuf[j] != '\0' &&
	  !isspace(ctimbuf[j]))
      ++j;
    if(ctimbuf[j] != '\0')
      ctimbuf[j++] = '\0';
  }

/* Convert month field to upper case. */

  s = field[1];
  while(*s != '\0') {
    *s = toupper(*s);
    ++s;
  }

/* Convert fields of input string into numbers. */

  sscanf(field[2], "%d", &year);
  for(i=0; i < 12; ++i) {
    if(!strcmp(field[1], month[i]))
      break;
  }
  mon = i;
  sscanf(field[0], "%d", &day);
  hour = 0;
  min = 0;
  sec = 0;
  sscanf(field[3], "%d", &hour);
  sscanf(field[4], "%d", &min);
  sscanf(field[5], "%d", &sec);

/* Since SGI does not provide the inverse function to localtime (i.e. mktime),
   we invert localtime by iteration. */

  tloc = 0;
  difftime = 1000000;
  for(i=0; i<10 && abs(difftime) > 1; ++i) {
    tm = localtime(&tloc);
    if(tm == NULL)
      return 0;

  /* Compute the (approximate) difference in seconds between the desired time
     and the current guess. */

    difftime = 60*(60*(24*(365*((year - tm->tm_year) % 100) + 
		 30*(mon - tm->tm_mon) + 
		 day - tm->tm_mday ) + 
		 hour - tm->tm_hour ) + 
		 min - tm->tm_min ) +
		 sec - tm->tm_sec;
    tloc = tloc + difftime;
  }

/* Convert the result to VMS format and copy back to calling program. */

  if(tim != NULL)
    unix_to_vms_time_(&tloc, tim);
  return 1;
}
