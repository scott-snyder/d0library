#include "smg.h"

long smg$cursor_column_( DISPLAY **dpid)

/*
C-----------------------------------------------------------------------
C-
C-   Name: smg$cursor_column_
C-
C-   Purpose: Return the column position of the virtual cursor in the 
C-            specified display.
C-
C-   Returned value: 0        - Operation failed.
C-                   column # - Operation succeeded.
C-
C-   Fortran usage:
C-      column = smg$cursor_column( dpid)
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

  smg_static_data.function_name = "smg$cursor_column";

/* Verify the display. */

  display = smg_verify_display(*dpid);
  if(display == NULL)
    return smg_error("invalid display id", 0);

/* Get cursor position. */

  if(getyx(display->window, crow, ccol) == ERR)
    return smg_error("error returned by getyx", 0);

/* Return SMG column number to calling program. */

  return ccol + 1;
}
