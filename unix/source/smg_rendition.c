#include "smg.h"

int smg_rendition(DISPLAY *display, long *vattr, long *vattrc)

/*
C----------------------------------------------------------------------
C-
C-   Name: smg_rendition
C-
C-   Purpose: Set video attributes for the specified window.
C-
C-   Returned value: OK  - Everything OK.
C-                   ERR - Error.
C-
C-   Arguments: 
C-
C-     vattr = SMG video attributes (bit packed).
C-
C-   Created   28-AUG-1991   Herbert Greenlee
C-
C-   Legal attributes are:
C-
C-     SMG$M_BOLD
C-     SMG$M_REVERSE
C-     SMG$M_BLINK
C-     SMG$M_UNDERLINE
C-
C----------------------------------------------------------------------
*/

{

  int smg_attr;
  int attr;

/* Construct new SMG video attribute mask */

  smg_attr = display->vattr;
  if(vattr != NULL)
    smg_attr |= *vattr;
  if(vattrc != NULL)
    smg_attr ^= *vattrc;

/* Construct curses video attributes */

  attr = 0;
  if(smg_attr & SMG$M_BOLD)
    attr |= A_BOLD;
  if(smg_attr & SMG$M_REVERSE)
    attr |= A_REVERSE;
  if(smg_attr & SMG$M_BLINK)
    attr |= A_BLINK;
  if(smg_attr & SMG$M_UNDERLINE)
    attr |= A_UNDERLINE;

/* Set attributes */

  if(wattrset(display->window, attr) == ERR)
    return smg_error("error returned by wattrset in smg_rendition", ERR);
  return OK;
}

