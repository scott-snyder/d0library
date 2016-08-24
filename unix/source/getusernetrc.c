/*
*******************************************************************************
*
* Name: getusernetrc
*
* Purpose: Read remote user from .netrc file for the specified host.  NULL
* pointer returned if not found.
*
* Usage:
*
* char *host, *user;
* user = getpass(host);
*
*******************************************************************************
*/
 
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "unix.h"
 
char *getusernetrc(char *host) {
 
  char *home, netrc_file[256];
  static char user[256];
  char line[256], word[256], *p;
  static FILE *netrc=NULL;
  struct stat statbuf;
  int ier;
  int state, match_host;
 
/* Open user's .netrc file. */
 
  if(netrc == NULL) {
    home = getenv("HOME");
    if( home == NULL )
      return NULL;
    strcpy(netrc_file,home);
    strcat(netrc_file,"/.netrc");
    netrc = fopen(netrc_file,"r");
    if( netrc == NULL )
      return NULL;
 
/* Check status -- make sure file is not readable by others. */
 
    ier = fstat(fileno(netrc), &statbuf);
    if(ier != 0)
      return NULL;
    if(statbuf.st_mode & (S_IRGRP | S_IROTH) ) {
      fprintf(stderr, "User's .netrc file is readable by others\n");
      exit(1);
    }
  }
 
/* Read contents of .netrc. */
 
  rewind(netrc);
  match_host = 0;
  state = 0;
 
/* Loop over lines of input. */
 
  while(fgets(line, sizeof(line)-1, netrc) != NULL) {
    p = line;
 
/* Loop over words of input line. */
 
    for(;;) {
      while(isspace(*p))
	++p;
      if(*p=='\0')
	break;
      if(sscanf(p,"%s",word) <= 0)
	break;
      p += strlen(word);
      if(state == 0) {
	if(!strcmp(word,"default")) {
	  state = 0;
	  match_host = 1;
	}
	if(!strcmp(word,"machine"))
	  state = 1;
	if(!strcmp(word,"login"))
	  state = 2;
	if(!strcmp(word,"password"))
	  state = 3;
	continue;
      }
      if(state == 1) {
	match_host = !strcmp(word,host);
	state = 0;
	continue;
      }
      if(state == 2) {
	if(match_host) {
	  strcpy(user,word);
	  return user;
	}
	state = 0;
	continue;
      }
      if(state == 3) {
	state = 0;
	continue;
      }
    }
  }
  return NULL;
}
