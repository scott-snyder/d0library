#include "smg.h"
#define STRLEN 256

long smg$put_with_scroll_( DISPLAY **dpid, char *text, long *direction,
  long *vattr, long *vattrc, long *flags, long *char_set, long len_text)

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
C-      ok = smg$put_with_scroll( dpid, text, direction, vattr, vattrc, flags,
C-     &  char_set)
C-
C-   Arguments:
C-
C-     dpid (integer*4, read only)     - Display identifier.
C-     text (character, read only)     - Text to output.
C-     direction (integer*4, read only)  - Scroll direction.
C-     vattr (integer*4, read only)    - Video attributes.
C-     vattrc (integer*4, read only)   - Video attributes complement.
C-     flags (integer*4, ignored)      - Line wrap flags.
C-     char_set (integer*4, ignored)   - Character set
C-
C-   Created  26-Sep-1991  Herbert Greenlee
C-
C-----------------------------------------------------------------------
*/

{
  DISPLAY *display;

  smg_static_data.function_name = "smg$put_with_scroll";

/* Verify the display. */

  display = smg_verify_display(*dpid);
  if(display == NULL)
    return smg_error("invalid display id", 0);

/* Pass call to smg$put_line. */

  return smg$put_line_(dpid, text, NULL, vattr, vattrc, flags, char_set, 
		       direction, len_text);
}
