#include <signal.h>
#include <stdlib.h>
#include <sys/types.h>
#include "smg.h"

long smg$delete_pasteboard_( DISPLAY **pbid, long *flags)

/*
C----------------------------------------------------------------------
C-
C-   Name: smg$delete_pasteboard
C-
C-   Purpose: Deinitialize curses
C-
C-   Returned value  : 0 (false) - Operation failed.
C-                     1 (true)  - Operation succeeded.
C-
C-   Fortran usage:
C-
C-      ok = smg$delete_pasteboard(pbid, flags)
C-
C-   Arguments: 
C-
C-     pbid (integer*4, read only)   - Pasteboard identifier.
C-     flags (integer*4, read only)  - Clear screen flag (1 = clear).
C-                                   Unimplemented.
C-
C-   Created   16-AUG-1991   Herbert Greenlee
C-
C-   This routine deletes all displays (except stdscr) and returns the 
C-   terminal to non-visual mode.
C-
C----------------------------------------------------------------------
*/

{
  DISPLAY *display, *prev_display;

  smg_static_data.function_name = "smg$delete_pasteboard";

/* Verify pbid. */

  if(*pbid != smg_display_root)
    return smg_error("invalid pasteboard id", 0);

/* Find the last display. */

  display = smg_display_root;
  while(display->next != NULL)
    display = display->next;

/* Delete all displays in the display chain in reverse order, except the 
   first. */

  while((prev_display = display->prev) != NULL) {
    if(!smg$delete_virtual_display_(&display))
      return 0;
    display = prev_display;
  }

/* Unpaste the first display. */

  if(!smg$unpaste_virtual_display_(&smg_display_root, &smg_display_root))
    return 0;

/* Take the terminal out of visual mode. */

  if(smg_reset_term_() == ERR)
    return smg_error("error returned by smg_reset_term", 0);

/* Kill child (if any). */

  if(smg_static_data.child > 0)
    kill(smg_static_data.child, SIGKILL);
  return 1;
}
