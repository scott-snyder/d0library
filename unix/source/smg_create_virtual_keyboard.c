#include "smg.h"

long smg$create_virtual_keyboard_( DISPLAY **kbid)

/*
C----------------------------------------------------------------------
C-
C-   Name: smg$create_virtual_keyboard
C-
C-   Purpose: Initialize curses input
C-
C-   Returned value: 0 (false) - Operation failed.
C-                   1 (true)  - Operation succeeded.
C-
C-   Fortran usage:
C-
C-      ok = smg$create_virtual_keyboard(kbid)
C-
C-   Arguments: 
C-
C-     kbid (integer*4, write only)  - Keyboard identifier.
C-
C-   Created   20-AUG-1991   Herbert Greenlee
C-
C-   This function set modes for curses input.  In curses, unlike smg, input
C-   is not independent of output.  Smg$create_pasteboard should be called
C-   before smg$create_virtual_keyboard to initialize curses output and
C-   curses generally.  If smg$create_pasteboard has not yet been called,
C-   this function calls it with default parameters.  The pointer to an 
C-   already open terminal is returned via the argument kbid.
C-
C----------------------------------------------------------------------
*/

{
  if(smg_display_root == NULL)
    if(smg$create_pasteboard_(kbid, "SYS$OUTPUT", 0, 0, 0, 10) == 0)
      return 0;

/* Set global input modes. */

  if(cbreak() == ERR)
    return smg_error("error returned by cbreak", 0);

/* Return kbid to calling program. */

  if(kbid != NULL)
    *kbid = smg_display_root;
  return 1;

}
