#include <stdlib.h>
#include "unix.h"

long lib$get_vm_(long *nbytes_ptr, long *baseadd_ptr)

/*
c----------------------------------------------------------------------
c-
c-   purpose and methods : allocate a block of dynamic memory
c-
c-   Fortran usage:
c-
c-      ok = lib$get_vm(nbytes, baseadd)
c-
c-   returned value  : 1 (success)
c-                     0 (failure)
c-
c-   arguments:
c-
c-     nbytes (integer*4, read only)   - number of bytes to allocate.
c-     baseadd (integer*4, write only) - base address of allocated region.
C-
C-   Created   18-Sep-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
*/

{
  size_t nbytes;
  void *baseadd;

  nbytes = *nbytes_ptr;
  baseadd = malloc(nbytes);
  *baseadd_ptr = (long)baseadd;
  if(baseadd == NULL)
    return 0;
  else
    return 1;
}
