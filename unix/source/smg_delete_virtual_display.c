#include <stdlib.h>
#include "smg.h"

long smg$delete_virtual_display_( DISPLAY **dpid)

/*
C----------------------------------------------------------------------
C-
C-   Name: smg_delete_virtual_display
C-
C-   Purpose: Delete a virtual display
C-
C-   Returned value: 0 (false) - Operation failed.
C-                   1 (true)  - Operation succeeded.
C-
C-   Fortran usage:
C-
C-      ok - smg$delete_virtual_display( dpid)
C-
C-   Arguments:
C-
C-     dpid (integer*4, read only)  - Display identifier.
C-   
C-   Created  20-Aug-1991  Herbert Greenlee
C-
C----------------------------------------------------------------------
*/

{
  DISPLAY *display;

  smg_static_data.function_name = "smg$delete_virtual_display";

/* Verify the display.  It is a non-fatal error if the display doesn't 
   exist. */

  display = smg_verify_display(*dpid);
  if(display == NULL)
    return 0;

/* Unpaste the display if it is pasted. */

  if(display->pasted)
    if(!smg$unpaste_virtual_display_(&display, &smg_display_root))
       return 0;

/* Remove the specified display from the chain. */

  if(display->prev != NULL)
    display->prev->next = display->next;
  if(display->next != NULL)
    display->next->prev = display->prev;

/* Deallocate resources associated with this display. */

  if(display->title != NULL)
    free(display->title);
  if(display->brdr != NULL)
    if(delwin(display->brdr) == ERR)
      return smg_error("error returned by delwin", 0);
  if(display->window != NULL)
    if(delwin(display->window) == ERR)
      return smg_error("error returned by delwin", 0);
  free(display);
  return 1;
}
