#include "smg.h"

long smg$get_display_attr_( DISPLAY **dpid, long *height, long *width, 
  long *dattr, long *vattr, long *char_set, long *flags)

/*
C-----------------------------------------------------------------------
C-
C-   Name: smg$get_display_attr_
C-
C-   Purpose: Return the position of the virtual cursor in the specified 
C-            display.
C-
C-   Returned value: 0 (false) - Operation failed.
C-                   1 (true)  - Operation succeeded.
C-
C-   Fortran usage:
C-      ok = smg$get_display_attr( dpid, height, width, dattr, vattr,
C-     &       char_set, flags)
C-
C-   Arguments:
C-
C-     dpid (integer*4, read only)      - Display identifier.
C-     height (integer*4, write only)   - Number of rows in display.
C-     width (integer*4, write only)    - Number of columns in display.
C-     dattr (integer*4, write only)    - Display attributes.
C-     vattr (integer*4, write only)    - Video attributes.
C-     char_set (integer*4, write only) - Character set.
C-     flags (integer*4, write only)    - Flags.
C-
C-   Created  26-Sep-1991  Herbert Greenlee
C-
C-----------------------------------------------------------------------
*/

{
  DISPLAY *display;

  smg_static_data.function_name = "smg$get_display_attr";

/* Verify the display. */

  display = smg_verify_display(*dpid);
  if(display == NULL)
    return smg_error("invalid display id", 0);

/* All information requested by this routine is stored in the display 
   structure.  Copy stuff back to calling program. */

  if(height != NULL)
    *height = display->nrows;
  if(width != NULL)
    *width = display->ncols;
  if(dattr != NULL)
    *dattr = display->dattr;
  if(vattr != NULL)
    *vattr != display->vattr;
  if(char_set != NULL)
    *char_set = SMG$C_ASCII;
  if(flags != NULL) {
    *flags = 0;
    if(display->row_vport != 0 || display->col_vport != 0 ||
       display->vrows != display->nrows || display->vcols != display->ncols)
      *flags |= SMG$M_VIEWPORT;
  }
  return 1;
}


