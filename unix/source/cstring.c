#include <stdlib.h>
#include "unix.h"

char *cstring(char *fstr, long len_fstr, char *cstr, long len_cstr)

/*
C----------------------------------------------------------------------
C-
C-   Name: cstring
C-
C-   Purpose: Convert a fortran character variable (fixed length, blank 
C-            padded) to a c-style (null terminated) character string.  
C-            Trailing whitespace is removed.
C-
C-   Returned value: Pointer to c-style string
C-
C-   Arguments: 
C-
C-   fstr     - Pointer to fortran character variable.
C-   len_fstr - Size of fortran character variable.
C-   cstr     - Pointer to buffer to receive c-style string.
C-   len_cstr - Size of buffer to receive c-style string.
C-
C-   Created   16-AUG-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
*/

{
  int n;

/* Find length of fortran string without trailing whitespace */

  for(n=len_fstr-1; n>=0; --n)
    if(!isspace(fstr[n])) break;
  ++n;

/* Make sure buffer is big enough */

  if(n >= len_cstr) {
    printf("cstring: Buffer overflow\n");
    abort();
  }

/* Copy the string and add a null terminator */

  strncpy(cstr, fstr, n);
  cstr[n] = '\0';
  return cstr;
}


