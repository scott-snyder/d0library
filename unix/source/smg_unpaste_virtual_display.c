#include <stdlib.h>
#include "smg.h"

long smg$unpaste_virtual_display_( DISPLAY **dpid, DISPLAY **pbid)

/*
C----------------------------------------------------------------------
C-
C-   Name: smg_unpaste_virtual_display
C-
C-   Purpose: Unpaste an already pasted display.
C-
C-   Returned value: 0 (false) - Operation failed.
C-                   1 (true)  - Operation succeeded.
C-
C-   Fortran usage:
C-
C-      ok - smg$unpaste_virtual_display( dpid, pbid)
C-
C-   Arguments:
C-
C-     dpid (integer*4, read only)     - Display identifier.
C-     pbid (integer*4, read only)     - Pasteboard identifier.
C-
C-   Created  22-Sep-1991  Herbert Greenlee
C-
C----------------------------------------------------------------------
*/

{
  DISPLAY *display, *last_display;

  smg_static_data.function_name = "smg$unpaste_virtual_display";

/* Verify the pasteboard and display identifiers. */

  if(*pbid != smg_display_root)
    return smg_error("invalid pasteboard id", 0);
  display = smg_verify_display(*dpid);
  if(display == NULL)
    return smg_error("invalid display id", 0);

/* Error if not already pasted. */

  if(!display->pasted)
    return smg_error("display not pasted", 0);

/* Find the last display. */

  last_display = smg_display_root;
  while(last_display->next != NULL)
    last_display = last_display->next;

/* Move the specified display from its current position to the last position 
   in the display chain, if it is not already last. */

  if(display != last_display) {
    if(display->prev != NULL)
      display->prev->next = display->next;
    if(display->next != NULL)
      display->next->prev = display->prev;
    display->next = NULL;
    display->prev = last_display;
    last_display->next = display;
  }

/* Mark the display as unpasted. */

  display->pasted = 0;

/* Update all displays. */

  return smg_update(smg_display_root);
}

