#include <stdlib.h>
#include "smg.h"

long smg$create_viewport_( DISPLAY **dpid, long *row_vport, long *col_vport,
  long *vrows, long *vcols)

/*
C----------------------------------------------------------------------
C-
C-   Name: smg_create_viewport
C-
C-   Purpose:  Associate a viewport with a virtual display.
C-
C-   Returned value: 0 (false) - Operation failed.
C-                   1 (true)  - Operation succeeded.
C-
C-   Fortran usage:
C-
C-      ok = smg$create_viewport( dpid, row_vport, col_vport, vrows, vcols)
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

  smg_static_data.function_name = "smg$create_viewport";

/* Verify display. */

  display = smg_verify_display(*dpid);
  if(display == NULL)
    return smg_error("invalid display id", 0);

/* Fill in row/column data. */

  display->row_vport = *row_vport - 1;
  display->col_vport = *col_vport - 1;
  display->vrows = *vrows;
  display->vcols = *vcols;
  return 1;
}
