#include <stdlib.h>
#include "smg.h"

long smg$change_virtual_display_(DISPLAY **dpid,  long *nrows, long *ncols, 
  long *dattr, long *vattr, long *char_set)

/*
C----------------------------------------------------------------------
C-
C-   Name: smg$change_virtual_display
C-
C-   Purpose: Change the size or attributes of a virtual display
C-
C-   Returned value: 0 (false) - Operation failed.
C-                   1 (true)  - Operation succeeded.
C-
C-   Fortran usage:
C-
C-      ok = smg$change_virtual_display( nrows, ncols, dpid, dattr, 
C-     &  vattr, char_set)
C-
C-   Arguments:
C-
C-     dpid (integer*4, write only)  - Display identifier.
C-     nrows (integer*4, read only)  - number of rows in display.
C-     ncols (integer*4, read only)  - number of columns in display.
C-     dattr (integer*4, read only)  - Display attributes.
C-     vattr (integer*4, read only)  - Video attributes.
C-     char_set (integer*4, ignored) - Character set
C-   
C-   Created  19-Sep-1991  Herbert Greenlee
C-
C----------------------------------------------------------------------
*/

{
  DISPLAY *display;
  int new_nrows, new_ncols, new_vattr, new_dattr;
  WINDOW *old_window;

  smg_static_data.function_name = "smg$change_virtual_display";

/* Verify display identifier. */

  display = smg_verify_display(*dpid);
  if(display == NULL)
    return smg_error("invalid displayid", 0);

/* Get new parameters.  Use old parameters as defaults. */

  if(nrows != NULL)
    new_nrows = *nrows;
  else
    new_nrows = display->nrows;
  if(ncols != NULL)
    new_ncols = *ncols;
  else 
    new_ncols = display->ncols;
  if(dattr != NULL)
    new_dattr = *dattr;
  else
    new_dattr = display->dattr;
  if(vattr != NULL)
    new_vattr = *vattr;
  else
    new_vattr = display->vattr;

  /* If any parameter has changed, make new text window. */

  if(new_nrows != display->nrows || new_ncols != display->ncols ||
     new_dattr != display->dattr || new_vattr != display->vattr) {
    old_window = display->window;
    display->nrows = new_nrows;
    display->ncols = new_ncols;
    display->dattr = new_dattr;
    display->vattr = new_vattr;

  /* Create new text window. */

    display->window = newpad(display->nrows, display->ncols);
    if(display->window == NULL)
      return smg_error("error returned by newpad", 0);

  /* Set options. */

    if(scrollok(display->window, TRUE) == ERR)
      return smg_error("error returned by scrollok", 0);
    if(keypad(display->window, TRUE) == ERR)
      return smg_error("error returned by keypad", 0);
    if(noecho() == ERR)
      return smg_error("error returned by noecho", 0);
    if(smg_rendition(display, NULL, NULL) == ERR)
      return 0;

  /* Copy text from the old text window to the new one. */

    if(overwrite(old_window, display->window) == ERR)
      return smg_error("error returned by overwrite", 0);

  /* Delete old windows. */

    if(delwin(old_window) == ERR)
      return smg_error("error returned by delwin", 0);

  /* If the changed display is pasted, update the screen. */

    if(display->pasted)
      return smg_update(smg_display_root);
  }

/* Did nothing if we get here. */

  return 1;
}
