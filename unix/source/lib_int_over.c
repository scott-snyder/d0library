#include <stdlib.h>
#include "unix.h"
 
long lib$int_over_(long *new_setting)
 
/*
c----------------------------------------------------------------------
c-
c-   purpose and methods : VMS emulator.  Currently dummy.
c-
c-   Fortran usage:
c-
c-      old_setting = lib$int_over(new_setting)
c-
c-   returned value  : Previous setting of integer overflow flag.
c-
c-   arguments:
c-
c-     new_setting(logical*4, read only) - New setting of integer overflow.
C-
C-   Created   28-Sep-1994   Herbert Greenlee
C-
C----------------------------------------------------------------------
*/
 
{
  return 0;
}
