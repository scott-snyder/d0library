#include "smg.h"

char *smg_function_name = NULL;

long smg_error(char *message, long retcode)

/*
C----------------------------------------------------------------------
C-
C-   Name: smg_error
C-
C-   Purpose: Error handling
C-
C-   Returned value: retcode
C-
C-   usage:
C-
C-     return smg_error("error message",0);
C-
C-   Arguments: 
C-
C-     message - Error message.
C-     retcode - Value to be returned by smg_error.
C-
C-   Created   16-AUG-1991   Herbert Greenlee
C-
C-   This c-callable function is used internally by the smg emulation 
C-   package.  No action is taken if the smg emulation package has not been 
C-   initialized by calling smg$create_pasteboard.  If smg has been 
C-   inititialized, then the following things are done.
C-
C-   1.  The terminal is taken out of visual mode by calling endwin().
C-       Other than this, no attempt is made to terminate curses.
C-
C-   2.  The name of the calling routine stored in smg_function_name
C-       is printed.
C-
C-   3.  The error message argument is printed.
C-
C-   4.  The program exits.
C-
C----------------------------------------------------------------------
*/

{
/* Has SMG been initialized?  If not, return true. */

  if(smg_display_root == NULL)
    return 1;

/* Take the terminal out of visual mode. */

  smg_reset_term_();

/* Print the error message. */

  printf("%s: %s\n", smg_static_data.function_name, message);
  exit(1);
}
