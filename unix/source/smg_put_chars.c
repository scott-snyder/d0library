#include "smg.h"
#define STRLEN 256

long smg$put_chars_( DISPLAY **dpid, char *text, long *row, long *col, 
  long *flags, long *vattr, long *vattrc, long *char_set, long len_text)

/*
C-----------------------------------------------------------------------
C-
C-   Name: smg$put_chars_
C-
C-   Purpose: Output text to the specified display.
C-
C-   Returned value: 0 (false) - Operation failed.
C-                   1 (true)  - Operation succeeded.
C-
C-   Fortran usage:
C-      ok = smg$put_chars( dpid, text, row, col, flags, vattr, vattrc,
C-     &  char_set)
C-
C-   Arguments:
C-
C-     dpid (integer*4, read only)   - Display identifier.
C-     text (character, read only)   - Text to output.
C-     row (integer*4, read only)    - Row number.
C-     col (integer*4, read only)    - Column number.
C-     flags (integer*4, ignored)    - Line wrap flags.
C-     vattr (integer*4, read only)  - Video attributes.
C-     vattrc (integer*4, read only) - Video attributes complement.
C-     char_set (integer*4, ignored) - Character set
C-
C-   Created  27-Sep-1991  Herbert Greenlee
C-
C-----------------------------------------------------------------------
*/

{
  DISPLAY *display;
  char ctext[STRLEN];         /* Null terminated copy of text argument */
  int curx, cury;             /* Initial position of cursor */

  smg_static_data.function_name = "smg$put_chars";

/* Verify the display. */

  display = smg_verify_display(*dpid);
  if(display == NULL)
    return smg_error("invalid display id", 0);

/* Set video attributes */

  if(smg_rendition(display, vattr, vattrc) == ERR)
    return 0;

/* Go to the starting position. */

  if(getyx(display->window, cury, curx) == ERR)
    return smg_error("error returned by getyx", 0);
  if(row != NULL)
    cury = *row - 1;
  if(col != NULL)
    curx = *col - 1;

/* Make a null-terminated copy of text argument.  Truncate if it is too long
   to fit in the display. */

  if(text == NULL)
    len_text = 0;
  if(len_text > display->ncols - curx - 1)
    len_text = display->ncols - curx - 1;
  strncpy(ctext, text, len_text);
  ctext[len_text] = '\0';

/* Output text. */

  smg_bell(ctext);
  if(mvwaddstr( display->window, cury, curx, ctext) == ERR)
    return smg_error("error returned by mvaddstr", 0);
  if(touchline( display->window, cury, 1) == ERR)
    return smg_error("error returned by touchline", 0);

/* Update screen */

  return smg_update(display);
}
