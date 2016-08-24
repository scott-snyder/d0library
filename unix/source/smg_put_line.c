#include <stdlib.h>
#include "smg.h"
#include "unix.h"
#define STRLEN 256

long smg$put_line_( DISPLAY **dpid, char *text, long *line_adv, long *vattr,
  long *vattrc, long *flags, long *char_set, long *direction, long len_text)

/*
C-----------------------------------------------------------------------
C-
C-   Name: smg$put_line_
C-
C-   Purpose: Output one line of text to the specified display.
C-
C-   Returned value: 0 (false) - Operation failed.
C-                   1 (true)  - Operation succeeded.
C-
C-   Fortran usage:
C-      ok = smg$put_line( dpid, text, line_adv, vattr, vattrc, flags,
C-     &  char_set, direction)
C-
C-   Arguments:
C-
C-     dpid (integer*4, read only)     - Display identifier.
C-     text (character, read only)     - Text to output.
C-     line_adv (integer*4, read only) - Number of lines to advance.
C-     vattr (integer*4, read only)    - Video attributes.
C-     vattrc (integer*4, read only)   - Video attributes complement.
C-     flags (integer*4, ignored)      - Line wrap flags.
C-     char_set (integer*4, ignored)   - Character set
C-     direction (integer*4, read only)  - Scroll direction.
C-
C-   Created  21-Aug-1991  Herbert Greenlee
C-
C-----------------------------------------------------------------------
*/

{
  DISPLAY *display;
  char ctext[STRLEN];         /* Null terminated copy of text argument */
  int curx, cury;             /* Initial position of cursor */
  int nline;                  /* Number of lines to advance */

  smg_static_data.function_name = "smg$put_line";

/* Verify the display. */

  display = smg_verify_display(*dpid);
  if(display == NULL)
    return smg_error("invalid display id", 0);

/* Set video attributes */

  if(smg_rendition(display, vattr, vattrc) == ERR)
    return 0;

/* Make a null-terminated copy of text argument. */

  if(text == NULL)
    len_text = 0;
  if(len_text > display->ncols-1)
    len_text = display->ncols-1;
  cstring(text, len_text, ctext, STRLEN);

/* Scoll from previous call (if necessary) */

  if(smg_scroll(display) == ERR)
    return 0;

/* Get the current cursor position */

  if(getyx( display->window, cury, curx) == ERR)
    return smg_error("error returned by getyx", 0);

/* Output text. */

  smg_bell(ctext);
  if(mvwaddstr( display->window, cury, 0, ctext) == ERR)
    return smg_error("error returned by mvaddstr", 0);
  if(touchline( display->window, cury, 1) == ERR)
    return smg_error("error returned by touchline", 0);

/* Get the number of lines to advance. */

  if(line_adv != NULL)
    nline = *line_adv;
  else
    nline = 1;
  if(direction != NULL) {
    if(*direction & SMG$M_UP)
      nline = abs(nline);
    if(*direction & SMG$M_DOWN)
      nline = -abs(nline);
  }

/* Advance cursor.  Calculate deferred scrolling. */

  cury += nline;
  if(cury >= display->nrows) {
    if(wmove(display->window, display->nrows-1, 0) == ERR)
      return smg_error("error returned by wmove", 0);
    display->nscroll = cury - display->nrows + 1;
  }
  else if(cury < 0) {
    if(wmove(display->window, 0, 0) == ERR)
      return smg_error("error returned by wmove", 0);
    display->nscroll = cury;
  }
  else {
    if(wmove(display->window, cury, 0) == ERR)
      return smg_error("error returned by wmove", 0);
  }

/* Update screen */

  return smg_update(display);
}
