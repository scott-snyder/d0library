#include <stdlib.h>
#include "smg.h"

long smg$copy_virtual_display_( DISPLAY **dpid, DISPLAY **dpid2)

/*
C----------------------------------------------------------------------
C-
C-   Name: smg_copy_virtual_display
C-
C-   Purpose: Copy a virtual display
C-
C-   Returned value: 0 (false) - Operation failed.
C-                   1 (true)  - Operation succeeded.
C-
C-   Fortran usage:
C-
C-      ok - smg$copy_virtual_display( dpid, dpid2)
C-
C-   Arguments:
C-
C-     dpid (integer*4, read only)   - Existing display identifier.
C-     dpid2 (integer*4, write only) - New display identifier.
C-   
C-   Created   5-Oct-1991  Herbert Greenlee
C-
C----------------------------------------------------------------------
*/

{
  DISPLAY *display, *display2;
  long nrows, ncols;

  smg_static_data.function_name = "smg$copy_virtual_display";

/* Verify the source display. */

  display = smg_verify_display(*dpid);
  if(display == NULL)
    return 0;

/* Create a new virtual display. */

  nrows = display->nrows;
  ncols = display->ncols;
  if(!smg$create_virtual_display_(&nrows, &ncols, &display2,
    &display->dattr, &display->vattr, NULL))
    return 0;

/* Fill up the text window of the new display. */

  if(copywin(display->window, display2->window, 0, 0, 0, 0, 
    display2->nrows - 1, display2->ncols - 1, 0) == ERR)
    smg_error("error returned by copywin", 0);

/* Copy the new display identifier back to the calling program. */

  if(dpid2 != NULL)
    *dpid2 = display2;
  return 1;
}
