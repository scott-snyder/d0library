#include "unix.h"
#include <stdio.h>
 
void flush_std_()
 
/*
C----------------------------------------------------------------------
C-
C-   Name: flush_std
C-
C-   Purpose: Fortran callable routine to flush standard output and 
C-            standard error streams.  No errors are returned.
C-
C-   Fortran usage:
C-
C-      call flush_std()
C-
C-   Created   6-Apr-1994   Herbert Greenlee
C-
C----------------------------------------------------------------------
*/
 
{
  fflush(stdout);
  fflush(stderr);
}
