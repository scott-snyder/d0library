#include <stdlib.h>
#include "smg.h"

long smg$delete_viewport_( DISPLAY **dpid)

/*
C----------------------------------------------------------------------
C-
C-   Name: smg_delete_viewport
C-
C-   Purpose:  Unpaste and delete the viewport associated with a virtual
C-             display.
C-
C-   Returned value: 0 (false) - Operation failed.
C-                   1 (true)  - Operation succeeded.
C-
C-   Fortran usage:
C-
C-      ok = smg$delete_viewport( dpid, row_vport, col_vport, vrows, vcols)
C-
C-   Arguments:
C-
C-     dpid (integer*4, read only)      - Display identifier.
C-     row_vport (integer*4, read only) - First of viewport in display.
C-     col_vport (integer*4, read only) - First column of viewport in display.
C-     vrows (integer*4, read only)     - Number of rows in viewport.
C-     vcols (integer*4, read only)     - Number of columns in viewport
C-   
C-   Created  23-Sep-1991  Herbert Greenlee
C-
C----------------------------------------------------------------------
*/

{
  DISPLAY *display;

  smg_static_data.function_name = "smg$delete_viewport";

/* Verify display. */

  display = smg_verify_display(*dpid);
  if(display == NULL)
    return smg_error("invalid display id", 0);

/* Unpaste display if it is pasted. */

  if(display->pasted)
    if(!smg$unpaste_virtual_display_(&display, &smg_display_root))
      return 0;

/* Set viewport to entire display */

  display->row_vport = 0;
  display->col_vport = 0;
  display->vrows = display->nrows;
  display->vcols = display->ncols;
  return 1;
}
