#include "smg.h"

int smg_scroll(DISPLAY *display)

/*
C----------------------------------------------------------------------
C-
C-   Name: smg_scroll
C-
C-   Purpose: Scroll the specified display
C-
C-   Returned value: OK  - Everything went OK
C-                   ERR - Error
C-
C-   Usage:
C-
C-     err = smg_scroll(display);
C-
C-   Arguments:
C-
C-     display - pointer to display to scroll.  The number of lines to scroll
C-               should be stored in display->nscroll.
C-
C-   Created   28-AUG-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
*/

{
  if(display->nscroll != 0)
    if(wscrl(display->window, display->nscroll) == ERR)
      return smg_error("error returned by wscrl in smg_scroll", ERR);
  display->nscroll = 0;
  return OK;
}
  
