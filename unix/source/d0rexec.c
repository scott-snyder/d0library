/*
*******************************************************************************
*
* Name: D0rexec
*
* Purpose: Remote command execution via rexec call (uses .netrc or password
*          prompting.).
*
* Usage:
*
* % rexec <host> [-l <user>] [-p <password>] <command>
*
*******************************************************************************
*/
 
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <netdb.h>
#include "unix.h"
 
int main (int argc, char **argv)
{
  char *host, *user, *password, *command, buf[128];
  int i, j, sock, clen, n;
 
/* Get remote host name from command line argument. */
 
  if (argc < 2)
    goto error;
  host = malloc (strlen (argv[1]) + 1);
  if(host == NULL)
    abort();
  strcpy (host, argv[1]);
 
/* Scan command line for user and password arguments. */
 
  user = NULL;
  password = NULL;
  for(i=2; i<argc-1; ++i) {
    if(!strcmp(argv[i],"-l") && i < argc-1) {
      user = malloc (strlen (argv[i+1]) + 1);
      if(user == NULL)
	abort();
      strcpy(user, argv[i+1]);
      argc -= 2;
      for(j=i; j<argc; ++j)
	argv[j] = argv[j+2];
    }
    if(!strcmp(argv[i],"-p") && i < argc-1) {
      password = malloc (strlen (argv[i+1]) + 1);
      if(password == NULL)
	abort();
      strcpy(password, argv[i+1]);
      argc -= 2;
      for(j=i; j<argc; ++j)
	argv[j] = argv[j+2];
    }
  }
 
/* Search .netrc for default user and password if they were not specified 
   on the command line.  This is to allow the host to be specified by alias
   in .netrc.  Rexec will get the required information from .netrc for 
   fully qualified host names in .netrc. */
 
  if(user == NULL)
    user = getusernetrc(host);
  if(password == NULL && user != NULL)
    password = getpassnetrc(host,user);
 
/* Assemble remaining arguments into remote command. */
 
  if (argc < 3)
    goto error;
  clen = 0;
  for (i=2; i<argc; i++)
    clen += strlen (argv[i]) + 1;
 
  command = malloc (clen);
  if(command == NULL)
    abort();
  *command = '\0';
  for (i=2; i<argc; i++) {
    strcat (command, argv[i]);
    if (i != argc-1) strcat (command, " ");
  }
 
  sock = rexec (&host, getservbyname ("exec", "tcp")->s_port,
                user, password, command, 0);
  if (sock < 0) {
    perror ("rexec fails");
    exit (1);
  }
 
  while ((n = read (sock, buf, sizeof (buf)-1)) > 0) {
    buf[n] = '\0';
    fputs (buf,stdout);
  }
 
  exit(0);
 
/* Come here if there is an error in the argument list.  Print brief
   help message. */
 
 error:
 
  fprintf (stderr, "Usage: rexec host [-l user] [-p password] command\n");
  exit (1);
 
}
