#include "smg.h"

long smg_update(DISPLAY *display)

/*
C----------------------------------------------------------------------
C-
C-   Name: smg_update
C-
C-   Purpose: Update screen
C-
C-   Returned value: 0 (false) - Operation failed.
C-                   1 (true)  - Operation succeeded.
C-
C-   usage: 
C-     ok = smg_update(display);
C-
C-   Arguments: 
C-
C-   Created   16-AUG-1991   Herbert Greenlee
C-
C-   This c-callable functin is used internally by the smg emulation
C-   package.  This function refreshes the specified display and all
C-   succeeding pasted displays.  If the first display is not pasted,
C-   smg_update returns without doing anything.
C-
C----------------------------------------------------------------------
*/

{
  int err;
  int brows, bcols;      /* Size of existing border window */

/* Verify display. */

  if(smg_verify_display(display) == NULL)
    return smg_error("invalid display id in smg_update", 0);

/* Do nothing if the bottom display is not pasted or we are in batch mode. */

  if( !display->pasted || smg_static_data.batch)
    return 1;

/* Loop over displays upward until we get to an unpasted display or there are
   no more displays. */

  do {
    if( !display->pasted)
      break;

  /* Skip over zero size displays. */

    if(display->nrows * display->ncols != 0) {

    /* If the display is bordered, make an appropriate border window.  An old 
       border window is recycled if possible. */

      if(display->dattr & SMG$M_BORDER) {

      /* Make sure an existing border has the right size.  Delete it if it does
         not. */

	if(display->brdr != NULL) {
	  if(getmaxyx(display->brdr, brows, bcols) == ERR)
	    return smg_error("error returned by getmaxyx in smg_update", 0);
	  if(brows != display->vrows + 2 || bcols != display->vcols + 2) {
	    if(delwin(display->brdr) == ERR)
	      return smg_error("error returned by delwin in smg_update", 0);
	    display->brdr = NULL;
	  }
	}

      /* Make a new border if one does not exist. */

	if(display->brdr == NULL) {
	  display->brdr = newpad(display->vrows + 2, display->vcols + 2);
	  if(display->brdr == NULL)
	    return smg_error("Can't make border pad in smg_update", 0);
	}

      /* Draw a box in the border window and display it. */

	if(box(display->brdr, 0, 0) == ERR)
	  return smg_error("error returned by box in smg_update", 0);
	if(pnoutrefresh(display->brdr, 0, 0, 
			display->prow - 1, 
		        display->pcol - 1, 
		      display->prow + display->vrows + 1,
		      display->pcol + display->vcols + 1
			) == ERR)
	  return smg_error("error returned by pnoutrefresh in smg_update", 0);
      }

    /* Update the display (finally). */

      if(touchwin(display->window) == ERR)
	return smg_error("error returned by touchwin in smg_update", 0);
      if(pnoutrefresh(display->window, 
		      display->row_vport, display->col_vport, 
		      display->prow, display->pcol, 
		      display->prow + display->vrows - 1,
		      display->pcol + display->vcols - 1
		      ) == ERR)
	return smg_error("error returned by pnoutrefresh in smg_update", 0);
    }
  }
  while((display = display->next) != NULL);
  if((err = doupdate()) == ERR)
    return smg_error("error returned by d0update in smg_udpate", 0);
  return 1;
}
