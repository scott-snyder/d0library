#include <stdlib.h>
#include "smg.h"

long smg_paste_display( DISPLAY **dpid, DISPLAY **pbid, long *prow,
  long *pcol, DISPLAY **top_dpid)

/*
C----------------------------------------------------------------------
C-
C-   Name: smg_paste_display
C-
C-   Purpose: Paste display
C-
C-   Returned value: 0 (false) - Operation failed.
C-                   1 (true)  - Operation succeeded.
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
C-   Created  23-Sep-1991  Herbert Greenlee
C-
C-   This routine is used internally by the smg emulation package.  Its
C-   calling sequence is similar to smg$paste_virtual_display and 
C-   smg$repaste_virtual_display and it is called by both of these.  This
C-   routine handles the updating of the display data structures.  The actual
C-   updating of the virtual terminal is left to smg$paste_virtual_display 
C-   or smg$repaste_virtual_display.
C-
C----------------------------------------------------------------------
*/

{
  DISPLAY *display, *base_display, *top_display;
  int crow, ccol;                   /* Curses row, col of pasted display */
  int xrows, xcols;                 /* Excess rows or columns after move */
  int nrows, ncols;                 /* New row/column dimensions */

  smg_static_data.function_name = "smg_paste_display";

/* Verify the pasteboard and display identifiers. */

  if(*pbid != smg_display_root)
    return smg_error("invalid pasteboard id", 0);
  display = smg_verify_display(*dpid);
  if(display == NULL)
    return smg_error("invalid display id", 0);

/* Find the display which will come before dpid after it is repasted.  This is 
   the last pasted display or the display before top_dpid, whichever comes 
   first.  The root display should always be pasted. */

/* Get the top display. */

  if(top_dpid != NULL)
    top_display = *top_dpid;
  else
    top_display = NULL;

/* Search the display chain. */

  base_display = smg_display_root;
  if(!base_display->pasted)
    return smg_error("internal error 1", 0);     /* Shouldn't ever happen */
  while(base_display != NULL) {
    if(base_display->next == NULL || base_display->next == top_display 
       || !base_display->next->pasted)
      break;
    base_display = base_display->next;
  }
  if(base_display == display)
    base_display = display->prev;                /* Can't be its own base */
  if(base_display == NULL)
    return smg_error("internal error 2", 0);     /* Shouldn't ever happen */

/* Rearrange the linking order of the displays so that display comes after 
   base_display. */

  if(display->prev != NULL)
    display->prev->next = display->next;
  if(display->next != NULL)
    display->next->prev = display->prev;

  display->next = base_display->next;
  display->prev = base_display;

  base_display->next = display;
  if(display->next != NULL)
    display->next->prev = display;

/* Set the new pasted location.  Change to curses coordinates. */

  display->prow = *prow - 1;
  display->pcol = *pcol - 1;
  display->pasted = 1;
  return 1;
}
