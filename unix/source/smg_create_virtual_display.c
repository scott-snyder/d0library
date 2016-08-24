#include <stdlib.h>
#include "smg.h"

long smg$create_virtual_display_( long *nrows, long *ncols, DISPLAY **dpid,
  long *dattr, long *vattr, long *char_set)

/*
C----------------------------------------------------------------------
C-
C-   Name: smg_create_virtual_display
C-
C-   Purpose: Create a virtual display
C-
C-   Returned value: 0 (false) - Operation failed.
C-                   1 (true)  - Operation succeeded.
C-
C-   Fortran usage:
C-
C-      ok = smg$create_virtual_display( nrows, ncols, dpid, dattr, 
C-     &  vattr, char_set)
C-
C-   Arguments:
C-
C-     nrows (integer*4, read only)  - number of rows in display.
C-     ncols (integer*4, read only)  - number of columns in display.
C-     dpid (integer*4, write only)  - Display identifier.
C-     dattr (integer*4, read only)  - Display attributes.
C-     vattr (integer*4, read only)  - Video attributes.
C-     char_set (integer*4, ignored) - Character set
C-   
C-   Created  20-Aug-1991  Herbert Greenlee
C-
C----------------------------------------------------------------------
*/

{
  DISPLAY *display;
  static int num_display = 0;     /* Number of displays (for debugging) */

  smg_static_data.function_name = "smg$create_virtual_display";

/* Find the last display in the chain. */

  display = smg_display_root;
  while(display->next != NULL)
    display = display->next;

/* Allocate a new display and add it at the end of the chain. */

  display->next = malloc (sizeof *display);
  if(display->next == NULL)
    return smg_error("memory allocation failed", 0);
  display->next->prev = display;
  display->next->next = NULL;
  display = display->next;

/* Fill in data for the new display. */

  display->window = NULL;
  display->brdr = NULL;
  display->title = NULL;
  display->nrows = *nrows;
  display->ncols = *ncols;
  display->vrows = display->nrows;
  display->vcols = display->ncols;
  display->row_vport = 0;
  display->col_vport = 0;
  if(dattr == NULL)
    display->dattr = 0;
  else
    display->dattr = *dattr;
  if(vattr == NULL)
    display->vattr = 0;
  else
    display->vattr = *vattr;
  display->pasted = 0;
  display->prow = 0;
  display->pcol = 0;
  display->num = num_display++;
  display->nscroll = 0;

/* Create the curses windows and draw border (if bordered). */

  display->window = newpad(display->nrows, display->ncols);
  if(display->window == NULL)
    return smg_error("error returned by newpad", 0);

/* Set options for this display. */

  if(scrollok(display->window, TRUE) == ERR)
    return smg_error("error returned by scrollok", 0);
  if(keypad(display->window, TRUE) == ERR)
    return smg_error("error returned by keypad", 0);
  if(noecho() == ERR)
    return smg_error("error returned by noecho", 0);
  if(smg_rendition(display, NULL, NULL) == ERR)
    return 0;

/* Copy display pointer back to calling program */

  if(dpid != NULL)
    *dpid = display;
  return 1;
}
