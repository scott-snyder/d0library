#include <stdlib.h>
#include "smg.h"

long smg$repaste_virtual_display_( DISPLAY **dpid, DISPLAY **pbid, long *prow,
  long *pcol, DISPLAY **top_dpid)

/*
C----------------------------------------------------------------------
C-
C-   Name: smg$repaste_virtual_display
C-
C-   Purpose: Repaste an alread pasted display.
C-
C-   Returned value: 0 (false) - Operation failed.
C-                   1 (true)  - Operation succeeded.
C-
C-   Fortran usage:
C-
C-      ok = smg$repaste_virtual_display( dpid, pbid, prow, pcol, top_dpid)
C-
C-   Arguments:
C-
C-     dpid (integer*4, read only)     - Display identifier.
C-     pbid (integer*4, read only)     - Pasteboard identifier.
C-     prow (integer*4, read only)     - Pasteboard row of upper left corner 
C-                                       of display.
C-     pcol (integer*4, read only)     - Pasteboard column of upper left
C-                                       corner of display.
C-     top_dpid (integer*4, read only) - Paste display under this display
C-                                       (if valid and pasted).
C-   
C-   Created  21-Aug-1991  Herbert Greenlee
C-
C-   The row and column follow the VMS convention, which is that the upper
C-   left corner of ab object has (row,column) = (1,1).  This is different
C-   from the curses convention, which is that the upper left corner has
C-   (row,column) = (0,0).
C-
C----------------------------------------------------------------------
*/

{
  DISPLAY *display;

  smg_static_data.function_name = "smg$repaste_virtual_display";

/* Verify the pasteboard and display identifiers. */

  if(*pbid != smg_display_root)
    return smg_error("invalid pasteboard id", 0);
  display = smg_verify_display(*dpid);
  if(display == NULL)
    return smg_error("invalid display id", 0);

/* Error if not already pasted. */

  if(!display->pasted)
    return smg_error("display not pasted", 0);

/* Update data structures. */

  if(!smg_paste_display(dpid, pbid, prow, pcol, top_dpid))
    return smg_error("error returned by smg_paste_display", 0);

/* Update all displays. */

  return smg_update(smg_display_root);
}

