#include <stdlib.h>
#include "smg.h"

long smg$move_virtual_display_( DISPLAY **dpid, DISPLAY **pbid, long *prow,
  long *pcol, DISPLAY **top_dpid)

/*
C----------------------------------------------------------------------
C-
C-   Name: smg$move_virtual_display
C-
C-   Purpose: Move or paste a display.
C-
C-   Returned value: 0 (false) - Operation failed.
C-                   1 (true)  - Operation succeeded.
C-
C-   Fortran usage:
C-
C-      ok = smg$move_virtual_display( dpid, pbid, prow, pcol, top_dpid)
C-
C-   Arguments:
C-
C-     dpid (integer*4, read only)     - Display identifier.
C-     pbid (integer*4, read only)     - Pasteboard identifier.
C-     prow (integer*4, read only)     - Pasteboard row of upper left corner 
C-                                       of display.
C-     pcol (integer*4, read only)     - Pasteboard column of upper left
C-                                       corner of display.
C-     top_dpid (integer*4, read only) - Paste display under this display.
C-                                       Ignored if dpid is already pasted.
C-   
C-   Created  19-Sep-1991  Herbert Greenlee
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

  smg_static_data.function_name = "smg$move_virtual_display";

/* Verify the pasteboard and display identifiers. */

  if(*pbid != smg_display_root)
    return smg_error("invalid pasteboard id", 0);
  display = smg_verify_display(*dpid);
  if(display == NULL)
    return smg_error("invalid display id", 0);

/* Call either smg$paste_virtual_display or smg$repaste_virtual_display 
   depending on whether the current display is already pasted. */

  if(display->pasted)
    return smg$repaste_virtual_display_(dpid, pbid, prow, pcol, &display->next);
  else
    return smg$paste_virtual_display_(dpid, pbid, prow, pcol, top_dpid);
}
