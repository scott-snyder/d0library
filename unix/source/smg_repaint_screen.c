#include "smg.h"

long smg$repaint_screen_( DISPLAY **pbid)

/*
C----------------------------------------------------------------------
C-
C-   Name: smg$repaint_screen
C-
C-   Purpose: Refresh screen
C-
C-   Returned value  : 0 (false) - Operation failed.
C-                     1 (true)  - Operation succeeded.
C-
C-   Fortran usage:
C-
C-      ok = smg$repaint_screen(pbid)
C-
C-   Arguments: 
C-
C-     pbid (integer*4, read only) - Pasteboard identifier.
C-
C-   Created   16-AUG-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
*/
{
  DISPLAY display;

  smg_static_data.function_name = "smg$repaint_screen";

/* Verify pbid */

  if(*pbid != smg_display_root)
    return smg_error("invalid pasteboard id", 0);

/* Rebuild the screen. */

  if(clearok(smg_display_root->window, TRUE) == ERR)
    return smg_error("error returned by clearok", 0);
  if(prefresh(smg_display_root->window, 0, 0, 0, 0, 
	      smg_display_root->nrows-1, smg_display_root->ncols-1) == ERR)
    return smg_error("error returned by prefresh", 0);
  return smg_update(smg_display_root);
}
