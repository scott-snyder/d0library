#include "smg.h"

long smg$delete_virtual_keyboard_( DISPLAY **kbid)

/*
C----------------------------------------------------------------------
C-
C-   Name: smg$delete_virtual_keyboard
C-
C-   Purpose: Deinitialize curses
C-
C-   Returned value  : 1 (true)
C-
C-   Fortran usage:
C-
C-      ok = smg$delete_virtual_keyboard(kbid)
C-
C-   Arguments: 
C-
C-     kbid (integer*4, read only)   - Keyboard identifier.
C-
C-   Created   16-AUG-1991   Herbert Greenlee
C-
C-   This routine is a dummy.  The job of deinitializing curses is left
C-   to smg$delete_pasteboard.
C-
C----------------------------------------------------------------------
*/

{
  return 1;
}
