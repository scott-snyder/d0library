#include "unix.h"

char *d0_loc_(char *var)

/*
C----------------------------------------------------------------------
C-
C-   Name: d0_loc
C-
C-   Purpose: Fortran callable routine to return address of variable.
C-            Unlike cernlib function locf, the address is in machine
C-            address units (e.g. bytes).
C-
C-   Fortran usage:
C-
C-      call d0_loc(var)
C-
C-   Argument:
C-
C-      var (read only) - Arbitrary variable
C-
C-   Created   9-Mar-1992   Herbert Greenlee
C-
C----------------------------------------------------------------------
*/

{
  return var;
}
