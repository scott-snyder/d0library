#include "smg.h"

int smg_reset_term_(void)

/*
C----------------------------------------------------------------------
C-
C-   Name: smg_reset_term
C-
C-   Purpose: Take the terminal out of visual mode.
C-
C-   Returned value: ERR - Operation failed.
C-                   OK  - Operation succeeded.
C-
C-   Fortran usage:
C-
C-     call smg_reset_term_();
C-
C-   Arguments: none
C-
C-   Created   16-AUG-1991   Herbert Greenlee
C-
C-   This function is used internally by the smg emulation package.  It 
C-   is called by smg$delete_pasteboard_ and smg_error.  In either case, it 
C-   resets the terminal into non-visual mode.  Error handling is limited 
C-   due to the fact that smg_reset_term must be callable after an error.
C-
C----------------------------------------------------------------------
*/

{
  long rval = OK;
  int ioerr;

/* Drop out of visual mode. */

  if(!isendwin())
    if(endwin() == ERR)
      rval = ERR;
  return rval;
}
