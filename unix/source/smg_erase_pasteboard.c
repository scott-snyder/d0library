#include "smg.h"

long smg$erase_pasteboard_( DISPLAY **pbid)

/*
C----------------------------------------------------------------------
C-
C-   Name: smg$erase_pasteboard
C-
C-   Purpose: Dummy 
C-
C-   Returned value  : 0 (false) - Operation failed.
C-                     1 (true)  - Operation succeeded.
C-
C-   Fortran usage:
C-
C-      ok = smg$erase_pasteboard(pbid)
C-
C-   Arguments: 
C-
C-     pbid (integer*4, read only)   - Pasteboard identifier.
C-
C-   Created   14-Jul-1992   Herbert Greenlee
C-
C----------------------------------------------------------------------
*/

{
  smg_static_data.function_name = "smg$erase_pasteboard";

/* Verify pbid. */

  if(*pbid != smg_display_root)
    return smg_error("invalid pasteboard id", 0);
  return 1;
}
