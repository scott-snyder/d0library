#include <stdlib.h>
#include "smg.h"

long smg$get_viewport_char_( DISPLAY **dpid, long *row_vport, long *col_vport,
  long *vrows, long *vcols)

/*
C----------------------------------------------------------------------
C-
C-   Name: smg_get_viewport_char
C-
C-   Purpose:  Get information about a viewport.
C-
C-   Returned value: 0 (false) - Operation failed.
C-                   1 (true)  - Operation succeeded.
C-
C-   Fortran usage:
C-
C-      ok = smg$get_viewport_char( dpid, row_vport, col_vport, vrows, vcols)
C-
C-   Arguments:
C-
C-     dpid (integer*4, read only)       - Display identifier.
C-     row_vport (integer*4, write only) - First of viewport in display.
C-     col_vport (integer*4, write only) - First column of viewport in display.
C-     vrows (integer*4, write only)     - Number of rows in viewport.
C-     vcols (integer*4, write only)     - Number of columns in viewport
C-   
C-   Created  27-Sep-1991  Herbert Greenlee
C-
C----------------------------------------------------------------------
*/

{
  DISPLAY *display;

  smg_static_data.function_name = "smg$get_viewport_char";

/* Verify display. */

  display = smg_verify_display(*dpid);
  if(display == NULL)
    return smg_error("invalid display id", 0);

/* Copy data to calling program. */

  if(row_vport != NULL)
    *row_vport = display->row_vport + 1;
  if(col_vport != NULL)
    *col_vport = display->col_vport + 1;
  if(vrows != NULL)
    *vrows = display->vrows;
  if(vcols != NULL)
    *vcols = display->vcols;
  return 1;
}
