#include "unix.h"
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <netdb.h>

/* Return codes. */

#define SUCCESS  0
#define NOMATCH  1
#define OVERFLOW 2
#define FAILURE  3

int fatmen_expand_dir(char *file, int nout, char *outbuf)

/*
 *-
 *-   Purpose and Methods: Expand the direcotry portion of a FATMEN generic 
 *-                        name containing wildcards.
 *-
 *-   Returned value: Status code:
 *-                   0 - success.
 *-                   1 - No match.
 *-                   2 - Overflow.
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
  char *host, *user, *password, command[256], line[256], *pt, *gen;
  char head[256], tail[256];
  char hostarg[256], *hostpt;
  int sock, need;
  int ngen = 0;
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

/* Parse the generic name into head (directory) and tail (file) parts. */

  strcpy(head, file);
  gen = head;
  while ((pt = strchr(gen, '/')) != NULL)
    gen = pt+1;
  strcpy(tail, gen);
  if(gen != head)
    *(gen-1) = '\0';

/* Assemble the remote command. */

  strcpy(command, "@d0$unix$vms:fatmen_expand \"");
  strcat(command, head);
  strcat(command, ",DIR\"");

/* Remote execute. */

  strcpy(hostarg, host);
  hostpt = hostarg;
  sock = rexec (&hostpt, getservbyname ("exec", "tcp")->s_port,
		user, password, command, 0);
  if (sock < 0) {
    fprintf(stderr, "fatmen_expand_dir: rexec failed");
    return FAILURE;
  }

/* Initialize outbuf. */

  ngen = 0;
  outbuf[0] = '\0';

/* Read output from remote command. */

  remote = fdopen(sock, "r");
  if(remote == NULL) {
    fprintf(stderr, "fatmen_expand_dir: fdopen failed");
    return FAILURE;
  }
  while (fgets(line, sizeof(line), remote) != NULL) {
    fputs (line,stderr);

  /* See if this line contains a generic name.  Add to outbuf. */

    gen = strstr(line, "//FNAL/D0/");
    if(gen != NULL && strstr(line, "Current Working Directory") == NULL) {
      pt = gen;
      while(!isspace(*pt) && *pt != '\0')
	++pt;
      *pt = '\0';

    /* See if this generic name is alreay in the list. */

      if(strstr(outbuf, gen) == NULL) {

      /* Yes, we want to add this gen. name to outbuf.  See if there is 
         room in outbuf. */

	need = strlen(outbuf) + strlen(gen) + 1;
	if(need < nout) {
	  if(ngen > 0)
	    strcat(outbuf, " ");
	  strcat(outbuf, gen);
	  strcat(outbuf, "/");
	  strcat(outbuf, tail);
	  ++ngen;
	}
	else {
	  ngen = -1;
	  break;
	}
      }
    }
  }
  fclose(remote);
  if(ngen < 0)
    return OVERFLOW;
  if(ngen > 0)
    return SUCCESS;
  return NOMATCH;
}
