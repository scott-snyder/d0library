#include <stdlib.h>
#include "smg.h"

long smg$set_cursor_mode_( DISPLAY **pbid, long *mode)

/*
C----------------------------------------------------------------------
C-
C-   Name: smg$set_cursor_mode
C-
C-   Purpose: End batching of screen updates.
C-
C-   Returned value  : 0 (false) - Operation failed.
C-                     1 (true)  - Operation succeeded.
C-
C-   Fortran usage:
C-
C-      ok = smg$set_cursor_mode(pbid)
C-
C-   Arguments: 
C-
C-     pbid (integer*4, read only) - Pasteboard identifier.
C-     mode (integer*4, read only) - Mode (0 = visible, 1 = invisible)
C-
C-   Created   1-OCT-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
*/

{
  smg_static_data.function_name = "smg$set_cursor_mode";

/* Verify pbid. */

  if(*pbid != smg_display_root)
    return smg_error("invalid pasteboard id", 0);

/* Set cursor visibility. */

  if(*mode == 0)
    curs_set(1);
  else
    curs_set(0);
  return 0;
}
