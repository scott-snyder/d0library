#include "smg.h"

long smg$return_cursor_pos_( DISPLAY **dpid, long *row, long *col)

/*
C-----------------------------------------------------------------------
C-
C-   Name: smg$return_cursor_pos_
C-
C-   Purpose: Return the position of the virtual cursor in the specified 
C-            display.
C-
C-   Returned value: 0 (false) - Operation failed.
C-                   1 (true)  - Operation succeeded.
C-
C-   Fortran usage:
C-      ok = smg$return_cursor_pos( dpid, row, col)
C-
C-   Arguments:
C-
C-     dpid (integer*4, read only) - Display identifier.
C-     row (integer*4, write only) - Row number.
C-     col (integer*4, write only) - Column number.
C-
C-   Created  22-Aug-1991  Herbert Greenlee
C-
C-----------------------------------------------------------------------
*/

{
  DISPLAY *display;
  int crow, ccol;            /* Curses row, column */

  smg_static_data.function_name = "smg$return_cursor_pos";

/* Verify the display. */

  display = smg_verify_display(*dpid);
  if(display == NULL)
    return smg_error("invalid display id", 0);

/* Get cursor position. */

  if(getyx(display->window, crow, ccol) == ERR)
    return smg_error("error returned by getyx", 0);

/* Copy values back to calling program.  Change to smg coordinates. */

  if(row != NULL)
    *row = crow + 1;
  if(col != NULL)
    *col = ccol + 1;
  return 1;
}
