#include <stdlib.h>
#include "smg.h"

long smg$move_text_( DISPLAY **dpid, long *ulrow, long *ulcol,
  long *brrow, long *brcol, DISPLAY **dpid2, long *ulrow2, long *ulcol2,
  long *flags)

/*
C----------------------------------------------------------------------
C-
C-   Name: smg_move_text
C-
C-   Purpose: Move or copy a block of text from one virtual display to 
C-            another.
C-
C-   Returned value: 0 (false) - Operation failed.
C-                   1 (true)  - Operation succeeded.
C-
C-   Fortran usage:
C-
C-      ok = smg$move_text( dpid, pbid, prow, pcol, top_dpid)
C-
C-   Arguments:
C-
C-     dpid (integer*4, read only)   - Source display identifier.
C-     ulrow (integer*4, read only)  - Upper left row of source.
C-     ulcol (integer*4, read only)  - Upper left column of source.
C-     brrow (integer*4, read only)  - Bottom right row of source.
C-     brcol (integer*4, readn only) - Bottom right column of source.
C-     dpid2 (integer*4, read only)  - Destination display identifier.
C-     ulrow2 (integer*4, read only) - Upper left row of destination.
C-     ulcol2 (integer*4, read only) - Upper left column of destination.
C-     flags (integer*4, read only)  - Flags:
C-                                     SMG$M_TEXT_SAVE - Do not erase text
C-                                                       in source.
C-
C-   Created  2-Oct-1991  Herbert Greenlee
C-
C----------------------------------------------------------------------
*/

{
  DISPLAY *display, *display2;
  WINDOW *block;
  int beg_x1, beg_y1, beg_x2, beg_y2, nrows, ncols;  /* Size and position of
                                                        block. */
  int erase_flag;

  smg_static_data.function_name = "smg$move_text";

/* Verify the display identifiers. */

  display = smg_verify_display(*dpid);
  if(display == NULL)
    return smg_error("invalid source display id", 0);
  display2= smg_verify_display(*dpid2);
  if(display2 == NULL)
    return smg_error("invalid destination display id", 0);

/* Get the size and position of the block in the two displays. */

  beg_y1 = *ulrow - 1;
  beg_x1 = *ulcol - 1;
  nrows = *brrow - *ulrow + 1;
  ncols = *brcol - *ulcol + 1;
  getyx(display2->window, beg_y2, beg_x2);
  if(ulrow2 != NULL)
    beg_y2 = *ulrow2 - 1;
  if(ulcol2 != NULL)
    beg_x2 = *ulcol2 - 1;

/* Copy block from source to destination display. */

  if(copywin(display->window, display2->window, beg_y1, beg_x1, beg_y2, beg_x2,
	     nrows-1, ncols-1, 0) == ERR)
    return smg_error("error returned by copywin", 0);

/* Erase block in source display?  This is accomplished by making a subpad
   and erasing the subpad. */

  if(flags != NULL)
    erase_flag = !(*flags & SMG$M_TEXT_SAVE);
  else
    erase_flag = 1;
  if(erase_flag) {
    block = subpad(display->window, nrows, ncols, beg_y1, beg_x1);
    if(block == NULL)
      return smg_error("error returned by subpad", 0);
    if(werase(block) == ERR)
      return smg_error("error returned by werase", 0);
    if(delwin(block) == ERR)
      return smg_error("error returned by delwin", 0);
  }

/* Do a full screen update since we don't know which display is pasted 
   first. */

  return smg_update(smg_display_root);
}

