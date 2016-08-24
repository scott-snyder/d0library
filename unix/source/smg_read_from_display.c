#include "smg.h"
#include "unix.h"
#define STRLEN 256

long smg$read_from_display_( DISPLAY **dpid, char *text, char *term_string, 
  long *row, long len_text, long len_term_string)

/*
C-----------------------------------------------------------------------
C-
C-   Name: smg$read_from_display_
C-
C-   Purpose: Read text from the specified display.
C-
C-   Returned value: 0 (false) - Operation failed.
C-                   1 (true)  - Operation succeeded.
C-
C-   Fortran usage:
C-
C-      ok = smg$read_from_display( dpid, text, term_string, row)
C-
C-   Arguments:
C-
C-     dpid (integer*4, read only)        - Display identifier.
C-     text (character, write only)       - Returned text.
C-     term_string (character, read only) - Back-terminators (unimplemented).
C-     row (integer*4, read only)         - Row number.
C-
C-   Created   2-Oct-1991  Herbert Greenlee
C-
C-----------------------------------------------------------------------
*/

{
  DISPLAY *display;
  char ctext[STRLEN];         /* Null terminated copy of text argument */
  int inix, iniy;             /* Initial position of cursor */
  int curx, cury;             /* Current position of cursor */
  int stax;                   /* Starting column for read text */
  int i, n;
  int dbl_width;              /* Double width flag */

  smg_static_data.function_name = "smg$read_from_display";

/* Verify the display. */

  display = smg_verify_display(*dpid);
  if(display == NULL)
    return smg_error("invalid display id", 0);

/* Make sure display is up to date. */

  if(!smg_update(display))
    return smg_error("error returned by smg_update", 0);

/* Go to the starting position.  Remember the initial position of the 
   cursor. */

  if(getyx(display->window, iniy, inix) == ERR)
    return smg_error("error returned by getyx", 0);
  if(row == NULL) {
    cury = iniy;
    curx = inix;
  }
  else {
    cury = *row - 1;
    curx = 0;
  }
  stax = curx;

/* Read character into ctext starting at the current cursor position.  Test 
   if this is double width text. */

  n = 0;
  dbl_width = 1;
  while(curx < display->ncols) {
    ctext[n] = mvwinch(display->window, cury, curx) & A_CHARTEXT;
    if( (curx & 1) != 0 && ctext[n] != ' ')
      dbl_width = 0;
    ++n;
    ++curx;
  }
  ctext[n] = '\0';

/* Shrink double width text here. */

  if(dbl_width) {
    for(i = 0; i < (n+1)/2; ++i) {
      if( (stax & 1) == 0)
	ctext[i] = ctext[2*i];
      else
	ctext[i] = ctext[2*i+1];
    }
    ctext[i] = '\0';
  }

/* Copy text back to calling program. */

  if(text != NULL)
    fstring(ctext, text, len_text);

/* Restore the cursor to its initial position. */

  if(wmove(display->window, iniy, inix) == ERR)
    smg_error("error returned by wmove", 0);
  return 1;
}
