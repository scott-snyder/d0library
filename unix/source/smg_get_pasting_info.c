#include <stdlib.h>
#include "smg.h"

long smg$get_pasting_info_( DISPLAY **dpid, DISPLAY **pbid, long *pflag,
  long *prow, long *pcol)

/*
C----------------------------------------------------------------------
C-
C-   Name: smg_get_pasting_info
C-
C-   Purpose: Return information about whether and where a display is pasted.
C-
C-   Returned value: 0 (false) - Operation failed.
C-                   1 (true)  - Operation succeeded.
C-
C-   Fortran usage:
C-
C-      ok = smg$get_pasting_info( dpid, pbid, pflag, prow, pcol)
C-
C-   Arguments:
C-
C-     dpid (integer*4, read only)   - Display identifier.
C-     pbid (integer*4, read only)   - Pasteboard identifier.
C-     pflag (logical*4, write only) - Pasted flag
C-     prow (integer*4, read only)   - Pasteboard row of upper left corner 
C-                                     of display.
C-     pcol (integer*4, read only)   - Pasteboard column of upper left
C-                                     corner of display.
C-   
C-   Created  27-Sep-1991  Herbert Greenlee
C-
C----------------------------------------------------------------------
*/

{
  DISPLAY *display;

  smg_static_data.function_name = "smg$get_pasting_info";

/* Verify the pasteboard and display identifiers. */

  if(*pbid != smg_display_root)
    return smg_error("invalid pasteboard id", 0);
  display = smg_verify_display(*dpid);
  if(display == NULL)
    return smg_error("invalid display id", 0);

/* Return information to calling program. */

  if(pflag != NULL)
    *pflag = display->pasted;
  if(prow != NULL)
    *prow = display->prow + 1;
  if(pcol != NULL)
    *pcol = display->pcol + 1;
}

