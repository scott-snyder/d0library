#include "smg.h"

long smg$erase_display_( DISPLAY **dpid, long *start_row, long *start_col,
  long *end_row, long *end_col)

/*
C-----------------------------------------------------------------------
C-
C-   Name: smg$erase_display_
C-
C-   Purpose: Erase ths specified display
C-
C-   Returned value: 0 (false) - Operation failed.
C-                   1 (true)  - Operation succeeded.
C-
C-   Fortran usage:
C-      ok = smg$erase_display( dpid, text, line_adv, vattr, vattrc, flags,
C-     &  char_set, direction)
C-
C-   Arguments:
C-
C-     dpid (integer*4, read only)      - Display identifier.
C-     start_row (integer*4, read only) - First row to erase.
C-     start_col (integer*4, read only) - First column to erase.
C-     end_row (integer*4, read only)   - Last row to erase.
C-     end_col (integer*4, read only)   - Last column to erase.
C-
C-   Created  22-Aug-1991  Herbert Greenlee
C-
C-----------------------------------------------------------------------
*/

{
  DISPLAY *display;
  WINDOW *erase_region;
  int st_row, st_col, e_row, e_col;

  smg_static_data.function_name = "smg$erase_display";

/* Verify dpid. */

  display = smg_verify_display(*dpid);
  if(display == NULL)
    return smg_error("invalid display id", 0);

/* Get limits or erase region.  Convert to curses coordinates and supply
   defaults.  */

  if(start_row != NULL)
    st_row = *start_row - 1;
  else
    st_row = 0;
  if(start_col != NULL)
    st_col = *start_col - 1;
  else
    st_col = 0;
  if(end_row != NULL)
    e_row = *end_row - 1;
  else
    e_row = display->nrows - 1;
  if(end_col != NULL)
    e_col = *end_col - 1;
  else
    e_col = display->ncols - 1;

/* Decided whether to do a full window erase. */

  if(st_row > 0 || st_col > 0 || e_row < display->nrows - 1 || 
     e_col < display->ncols - 1) {

  /* Here we do a partial erase.  Make a subpad and erase it.  We delete
     the subpad when we are done. */

    erase_region = subpad(display->window, 
			  e_row - st_row + 1, e_col - st_col + 1,
			  st_row, st_col);
    if(erase_region == NULL)
      return smg_error("error returned by subpad", 0);
    if(werase(erase_region) == ERR)
      return smg_error("error returned by werase", 0);
    if(delwin(erase_region) == ERR)
      return smg_error("error returned by delwin", 0);
  }

  else {

  /* Full window erase. */

    if(werase(display->window) == ERR)
      return smg_error("error returned by werase", 0);
  }

/* Update the screen. */

  return smg_update(display);
}
