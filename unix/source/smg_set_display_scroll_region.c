#include <stdlib.h>
#include "smg.h"

long smg$set_display_scroll_region_(DISPLAY **dpid, long *start, long *end)

/*
C----------------------------------------------------------------------
C-
C-   Name: smg$set_display_scroll_region
C-
C-   Purpose: Define a software scrolling region for a virtual display.
C-
C-   Returned value: 0 (false) - Operation failed.
C-                   1 (true)  - Operation succeeded.
C-
C-   Fortran usage:
C-
C-     ok = smg$set_display_scroll_region(dpid, start, end)
C-
C-   Arguments: 
C-
C-     dpid (integer*4, read only)  - Display identifier.
C-     start (integer*4, read only) - First row of scroll region.
C-     end (integer*4, read only)   - Last row of scroll region.
C-
C-   Created   16-AUG-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
*/

{
  DISPLAY *display;
  long start_row, end_row;

  smg_static_data.function_name = "smg$set_display_scroll_region";

/* Verify display identifier. */

  display = smg_verify_display(*dpid);
  if(display == NULL)
    return smg_error("invalid display id", 0);

/* Get scroll region limits.  Change to curses row number convention and 
   supply defaults. */

  if(start != NULL)
    start_row = *start-1;
  else
    start_row = 0;
  if(end != NULL)
    end_row = *end -1;
  else
    end_row = display->nrows - 1;

/* Do it. */

  if(wsetscrreg(display->window, start_row, end_row) == ERR)
    return smg_error("error returned by wsetscrreg", 0);
  return 1;
}
