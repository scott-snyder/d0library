#include "smg.h"
#define STRLEN 256

long smg$put_chars_wide_( DISPLAY **dpid, char *text, long *row, long *col, 
  long *vattr, long *vattrc, long *char_set, long len_text)

/*
C-----------------------------------------------------------------------
C-
C-   Name: smg$put_chars_wide_
C-
C-   Purpose: Output text to the specified display.
C-
C-   Returned value: 0 (false) - Operation failed.
C-                   1 (true)  - Operation succeeded.
C-
C-   Fortran usage:
C-      ok = smg$put_chars_wide( dpid, text, row, col, vattr, vattrc,
C-     &  char_set)
C-
C-   Arguments:
C-
C-     dpid (integer*4, read only)   - Display identifier.
C-     text (character, read only)   - Text to output.
C-     row (integer*4, read only)    - Row number.
C-     col (integer*4, read only)    - Column number.
C-     vattr (integer*4, read only)  - Video attributes.
C-     vattrc (integer*4, read only) - Video attributes complement.
C-     char_set (integer*4, ignored) - Character set
C-
C-   Created  27-Sep-1991  Herbert Greenlee
C-
C-   The SMG routine outputs double width characters.  Curses does not support
C-   double width text.  This routine simulates double width by inserting
C-   spaces into the text argument.
C-
C-----------------------------------------------------------------------
*/

{
  DISPLAY *display;
  int i,j;
  char local_text[STRLEN];
  long local_col;

  smg_static_data.function_name = "smg$put_chars_wide";

/* Verify the display. */

  display = smg_verify_display(*dpid);
  if(display == NULL)
    return smg_error("invalid display id", 0);

/* Make a double spaced copy of the text argument.  Put text in even numbered
   (curses coordinates) columns only. */

  if(len_text > STRLEN/2)
    len_text = STRLEN/2;
  j = 0;
  for(i=0; i<len_text; ++i) {
    if( ((*col+j) & 1) != 0) { 
      local_text[j++] = text[i];
      local_text[j++] = ' ';
    }
    else {
      local_text[j++] = ' ';
      local_text[j++] = text[i];
    }
  }

/* Pass the call to smg$put_chars. */

  return smg$put_chars_(dpid, local_text, row, col, NULL, vattr, vattrc, 
			char_set, 2*len_text);
}

