#include "unix.h"
#include <stdlib.h>

void exit_(long *exit_status)

/*
C----------------------------------------------------------------------
C-
C-   Name: exit
C-
C-   Purpose: Fortran callable exit routine.
C-
C-   Fortran usage:
C-
C-      call exit(status)
C-
C-   Argument:
C-
C-      status (read only) - Exit status (machine dependent).
C-
C-   Created   29-Sep-1992   Herbert Greenlee
C-
C----------------------------------------------------------------------
*/

{
  int status;

  if(exit_status != NULL) {
    status = *exit_status;
  }
  else {
    status = EXIT_SUCCESS;
  }
  exit(status);
}
