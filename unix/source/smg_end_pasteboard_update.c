#include <stdlib.h>
#include "smg.h"

long smg$end_pasteboard_update_( DISPLAY **pbid)

/*
C----------------------------------------------------------------------
C-
C-   Name: smg$end_pasteboard_update
C-
C-   Purpose: End batching of screen updates.
C-
C-   Returned value  : 0 (false) - Operation failed.
C-                     1 (true)  - Operation succeeded.
C-
C-   Fortran usage:
C-
C-      ok = smg$end_pasteboard_update(pbid)
C-
C-   Arguments: 
C-
C-     pbid (integer*4, read only)   - Pasteboard identifier.
C-
C-   Created   1-OCT-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
*/

{
  smg_static_data.function_name = "smg$end_pasteboard_update";

/* Verify pbid. */

  if(*pbid != smg_display_root)
    return smg_error("invalid pasteboard id", 0);

/* Reset the batch flag. */

  smg_static_data.batch = 0;

/* Do a full screen update. */

  return smg_update(smg_display_root);
}
