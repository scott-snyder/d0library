#include "smg.h"

long smg$erase_line_( DISPLAY **dpid, long *row, long *col)

/*
C-----------------------------------------------------------------------
C-
C-   Name: smg$erase_line_
C-
C-   Purpose: Erase one line
C-
C-   Returned value: 0 (false) - Operation failed.
C-                   1 (true)  - Operation succeeded.
C-
C-   Fortran usage:
C-      ok = smg$erase_line( dpid, text, line_adv, vattr, vattrc, flags,
C-     &  char_set, direction)
C-
C-   Arguments:
C-
C-     dpid (integer*4, read only)      - Display identifier.
C-     start_row (integer*4, read only) - First row to erase.
C-     start_col (integer*4, read only) - First column to erase.
C-     end_row (integer*4, read only)   - Last row to erase.
C-     end_col (integer*4, read only)   - Last column to erase.
C-
C-   Created  22-Aug-1991  Herbert Greenlee
C-
C-----------------------------------------------------------------------
*/

{
  DISPLAY *display;
  long e_row, e_col;

  smg_static_data.function_name = "smg$erase_line";

/* Verify display. */

  display = smg_verify_display(*dpid);
  if(display == NULL)
    return smg_error("invalid display id", 0);

/* Go to the erase point. */

  if(getyx(display->window, e_row, e_col) == ERR)
    return smg_error("error returned by getyx", 0);
  if(row != NULL)
    e_row = *row - 1;
  if(col != NULL)
    e_col = *col - 1;
  if(wmove(display->window, e_row, e_col) == ERR)
    return smg_error("error returned by wmove", 0);

/* Erase. */

  if(wclrtoeol(display->window) == ERR)
    return smg_error("error returned by wclrtoeol", 0);

/* Update the screen. */

  return smg_update(display);
}
