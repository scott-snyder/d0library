#include <stdlib.h>
#include "unix.h"
 
long lib$free_vm_(long *nbytes_ptr, long *baseadd_ptr)
 
/*
c----------------------------------------------------------------------
c-
c-   purpose and methods : Deallocate a block of dynamic memory
c-
c-   Fortran usage:
c-
c-      ok = lib$free_vm(nbytes, baseadd)
c-
c-   returned value  : 1 (success)
c-                     0 (failure)
c-
c-   arguments:
c-
c-     nbytes (integer*4, read only)   - number of bytes to deallocate.
c-     baseadd (integer*4, write only) - base address of allocated region.
C-
C-   Created   28-Sep-1994   Herbert Greenlee
C-
C-   Notes
C-
C-   1.  The number-of-bytes argument is included for VMS compatibity,
C-       but is ignored.  The entire region is alway deallocated.
C-
C----------------------------------------------------------------------
*/
 
{
  size_t nbytes;
  void *baseadd;
 
  baseadd = (void*)*baseadd_ptr;
  free(baseadd);
  return 1;
}
