#include <stdlib.h>
#include "unix.h"

char *fstring(char *cstr, char *fstr, long len_fstr)

/*
C----------------------------------------------------------------------
C-
C-   Name: fstring
C-
C-   Purpose: Convert a c-style (null terminated) character string to a 
C-            fortran character variable (fixed length, blank padded).
C-
C-   Returned value: Pointer to fortran character variable.
C-
C-   Arguments: 
C-
C-   cstr     - Pointer to c-style character string.
C-   fstr     - Pointer to fortran character variable.
C-   len_cstr - Size of fortran character variable.
C-
C-   Created   16-AUG-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
*/

{
  size_t n;

/* Make sure fortran character variable is big enough */

  n = strlen(cstr);
  if(n > len_fstr) {
    printf("fstr: Buffer overflow\n");
    abort();
  }

/* Copy the string and pad with spaces */

  strncpy(fstr, cstr, n);
  while(n < len_fstr)
    fstr[n++] = ' ';
  return cstr;
}


