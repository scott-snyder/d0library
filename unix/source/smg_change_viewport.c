#include <stdlib.h>
#include "smg.h"

long smg$change_viewport_( DISPLAY **dpid, long *row_vport, long *col_vport,
  long *vrows, long *vcols)

/*
C----------------------------------------------------------------------
C-
C-   Name: smg_change_viewport
C-
C-   Purpose:  Change a viewport associated with a virtual display.
C-
C-   Returned value: 0 (false) - Operation failed.
C-                   1 (true)  - Operation succeeded.
C-
C-   Fortran usage:
C-
C-      ok = smg$change_viewport( dpid, row_vport, col_vport, vrows, vcols)
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

  smg_static_data.function_name = "smg$change_viewport";

/* Verify display. */

  display = smg_verify_display(*dpid);
  if(display == NULL)
    return smg_error("invalid display id", 0);

/* Get new data. */

  if(row_vport != NULL)
    display->row_vport = *row_vport - 1;
  if(col_vport != NULL)
    display->col_vport = *col_vport - 1;
  if(vrows != NULL)
    display->vrows = *vrows;
  if(vcols != NULL)
    display->vcols = *vcols;
  return 1;
}
