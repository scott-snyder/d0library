#include "unix.h"
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <netdb.h>

/* Return codes. */

#define SUCCESS  0
#define NOMATCH  1
#define FAILURE  3

int fatmen_find(char *file, int nout, char *outbuf)

/*
 *-
 *-   Purpose and Methods: Find (stage if necessary) a fatmen generic name.
 *-
 *-   Returned value: Status code:
 *-                   0 - success.
 *-                   1 - No match.
 *-                   3 - Failure.
 *-
 *-   Inputs  : file (read only)    - Character string containing generic name.
 *-             nout (read only)    - Size of output buffer.
 *-             outbuf (write only) - Output buffer.
 *-   Outputs :
 *-   Controls:
 *-
 *-   Created   6-May-1994   Herbert Greenlee
 *-
*/

{
  char *host, *user, *password, command[256], line[256], *pt, *phys;
  char hostarg[256], *hostpt;
  int sock, need, found;
  FILE *remote;

/* Get remote host name from env. variable FATMEN_NODE or VAXLIB_NODE. */

  host = getenv("FATMEN_NODE");
  if(host == NULL)
    host = getenv("VAXLIB_NODE");
  if(host == NULL)
    return FAILURE;

/* Search .netrc for default user and password.  This is to allow the host 
   to be specified by alias in .netrc.  Rexec will get the required 
   information from .netrc for fully qualified host names in .netrc. */
 
  user = getusernetrc(host);
  if(user != NULL)
    password = getpassnetrc(host,user);
  else
    password = NULL;

/* Assemble the remote command. */

  strcpy(command, "@d0$unix$vms:fatmen_find \"");
  strcat(command, file);
  strcat(command, "\"");

/* Remote execute. */

  strcpy(hostarg, host);
  hostpt = hostarg;
  sock = rexec (&hostpt, getservbyname ("exec", "tcp")->s_port,
		user, password, command, 0);
  if (sock < 0) {
    fprintf(stderr, "fatmen_find: rexec failed");
    return FAILURE;
  }

/* Initialize outbuf. */

  outbuf[0] = '\0';
  found = 0;

/* Read output from remote command. */

  remote = fdopen(sock, "r");
  if(remote == NULL) {
    fprintf(stderr, "fatmen_find: fdopen failed");
    return FAILURE;
  }
  while (fgets(line, sizeof(line), remote) != NULL) {
    fputs (line,stderr);

  /* See if this line contains a filename.  Store in outbuf. */

    phys = strstr(line, "Physical filename:");
    if(phys != NULL) {
      phys += 17;
      while(!isspace(*phys) && *phys != '\0')
	++phys;
      while(isspace(*phys) && *phys != '\0')
	++phys;
      pt = phys;
      while(!isspace(*pt) && *pt != '\0')
	++pt;
      *pt = '\0';

    /* See if there is room in outbuf. */

      need = strlen(phys) + 1;
      if(need < nout) {
	strcpy(outbuf, phys);
	found = 1;
	break;
      }
    }
  }
  fclose(remote);
  if(found)
    return SUCCESS;
  return NOMATCH;
}
