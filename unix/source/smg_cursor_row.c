#include "smg.h"

long smg$cursor_row_( DISPLAY **dpid)

/*
C-----------------------------------------------------------------------
C-
C-   Name: smg$cursor_row_
C-
C-   Purpose: Return the row position of the virtual cursor in the 
C-            specified display.
C-
C-   Returned value: 0     - Operation failed.
C-                   row # - Operation succeeded.
C-
C-   Fortran usage:
C-      row = smg$cursor_row( dpid)
C-
C-   Arguments:
C-
C-     dpid (integer*4, read only) - Display identifier.
C-
C-   Created  24-Sep-1991  Herbert Greenlee
C-
C-----------------------------------------------------------------------
*/

{
  DISPLAY *display;
  int crow, ccol;            /* Curses row, column */

  smg_static_data.function_name = "smg$cursor_row";

/* Verify the display. */

  display = smg_verify_display(*dpid);
  if(display == NULL)
    return smg_error("invalid display id", 0);

/* Get cursor position. */

  if(getyx(display->window, crow, ccol) == ERR)
    return smg_error("error returned by getyx", 0);

/* Return SMG row number to calling program. */

  return crow + 1;
}
