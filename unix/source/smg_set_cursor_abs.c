#include "smg.h"

long smg$set_cursor_abs_( DISPLAY **dpid, long *row, long *col)

/*
C-----------------------------------------------------------------------
C-
C-   Name: smg$set_cursor_abs_
C-
C-   Purpose: Move the cursor for display dpid.
C-
C-   Returned value: 0 (false) - Operation failed.
C-                   1 (true)  - Operation succeeded.
C-
C-   Fortran usage:
C-      ok = smg$set_cursor_abs( dpid, row, col)
C-
C-   Arguments:
C-
C-     dpid (integer*4, read only) - Display identifier.
C-     row (integer*4, read only)  - Row number.
C-     col (integer*4, read only)  - Column number.
C-
C-   Created  22-Aug-1991  Herbert Greenlee
C-
C-----------------------------------------------------------------------
*/

{
  DISPLAY *display;
  int dummy, newrow, newcol;
  int crow, ccol;            /* Curses row, column */

  smg_static_data.function_name = "smg$set_cursor_abs";

/* Verify the display. */

  display = smg_verify_display(*dpid);
  if(display == NULL)
    return smg_error("invalid display id", 0);

/* Get new row and column.  Change to curses coordinates. */

  if(row != NULL)
    newrow = *row - 1;
  else
    if(getyx(display->window, newrow, dummy) == ERR)
      return smg_error("error returned by getyx", 0);
  if(col != NULL)
    newcol = *col - 1;
  else
    if(getyx(display->window, dummy, newcol) == ERR)
      return smg_error("error returned by getyx", 0);

/* Move the cursor */

  if(wmove(display->window, newrow, newcol) == ERR)
    return smg_error("error returned by wmove", 0);
  display->nscroll = 0;
  return smg_update(display);
}
