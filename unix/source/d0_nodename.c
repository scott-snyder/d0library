#include "unix.h"
#include <string.h>

void d0_nodename_(char *node, long nodelen)

/*
C----------------------------------------------------------------------
C-
C-   Name: d0_nodename
C-
C-   Purpose: This subroutine is a fortran callable interface to the c 
C-            run-time library routine gethostname.  This fuuction returns
C-            local node name.
C-
C-   Fortran usage:
C-
C-      call d0_nodename(node)
C-
C-   Argument:
C-
C-      node (character, write only) - Local node name
C-
C-   Created   9-Mar-1992   Herbert Greenlee
C-
C----------------------------------------------------------------------
*/

{
  int len;
  char host[STRLEN];
  char *period;

  len = STRLEN;
  gethostname(host, len);

/* Make sure that nodename will fit into the calling program's variable. */

  if(len > nodelen)
    host[nodelen] = '\0';

/* Truncate domain name (if any). */

  period = strchr(host,'.');
  if(period != NULL)
    *period = '\0';

/* Return result to calling program. */

  fstring(host, node, nodelen);
  return;
}
