#include <signal.h>
#include "smg.h"

long smg$disable_unsolicited_input_( DISPLAY **pbid)

/*
C----------------------------------------------------------------------
C-
C-   Name: smg$disable_unsolicited_input
C-
C-   Purpose: Begin batching of screen updates.
C-
C-   Returned value  : 0 (false) - Operation failed.
C-                     1 (true)  - Operation succeeded.
C-
C-   Fortran usage:
C-
C-      ok = smg$disable_unsolicited_input(pbid)
C-
C-   Arguments: 
C-
C-     pbid (integer*4, read only)       - Pasteboard identifier.
C-
C-   Created   7-OCT-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
*/

{
  struct sigaction act;

  smg_static_data.function_name = "smg$disable_unsolicited_input";

/* Verify pbid. */

  if(*pbid != smg_display_root)
    return smg_error("invalid pasteboard id", 0);

/* Disable signal processing on input stream. */

  act.sa_handler = SIG_IGN;
  sigemptyset(&act.sa_mask);
  act.sa_flags = 0;
  if(sigaction(SIGUSR1, &act, NULL) < 0)
    return smg_error("sigaction failed", 0);

/* Reset asynchronous flag. */

  smg_static_data.async = 0;
  return 1;
}
