/*
C-----------------------------------------------------------------------
C-
C- This file contains the following public entry points:
C-
C- 1. find_file      - Find the first (or next) of the files in a given stream.
C- 2. find_file_     - Fortran callable
C- 3. find_file_end  - Terminate a file stream & deallocate context string.
C- 4. find_file_end_ - Fortran callable
C-
C- Find_file performs environment variable and csh wildcard expansion of
C- filenames.
C-
C- Go to the function definitions for documentation on the calling sequences.
C-
C-----------------------------------------------------------------------
*/
 
#include "unix.h"
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>
#include <netdb.h>
#include <ctype.h>

#ifndef ARG_MAX
# define ARG_MAX 4096
#endif
 
static void *smalloc(size_t nbytes)

/* Safe malloc.  Guaranteed success on return.  Abort if fail. */

{
  void *ptr;

  ptr = malloc(nbytes);
  if(ptr == NULL) {
    fprintf(stderr, "find_file: malloc failed\n");
    abort();
  }
  return(ptr);
}

static void *srealloc(void *ptr, size_t nbytes)

/* Safe realloc.  Guaranteed success on return.  Abort if fail. */

{
  ptr = realloc(ptr, nbytes);
  if(ptr == NULL) {
    fprintf(stderr, "find_file: realloc failed\n");
    abort();
  }
  return(ptr);
}

static void sfree(void *ptr)

/* Safe free.  Free if not NULL. */

{
  if(ptr != NULL) {
    free(ptr);
  }
  return;
}

static glob_t *newpglob()

/* This routine returns a pointer to a new empty pglob structure.  Use
   freeglob to deallocate. */

{
  glob_t *pglob;

  pglob = smalloc(sizeof(glob_t));
  pglob->gl_pathc = 0;
  pglob->gl_matchc = 0;
  pglob->gl_offs = 0;
  pglob->gl_flags = 0;
  pglob->gl_errfunc = NULL;
  pglob->gl_pathv = NULL;
  return pglob;
}

static void freepath(glob_t *pglob)

/* This routine frees path related memory blocks in a pglob structure and
   zeroes the remaining elements of the structure. */

{
  int i,n;

  if(pglob != NULL) {
    if(pglob->gl_pathv != NULL) {
      n = pglob->gl_pathc;
      for(i=0; i<n; ++i) {
	sfree(pglob->gl_pathv[i]);
      }
      sfree(pglob->gl_pathv);
      pglob->gl_pathv = NULL;
    }
    pglob->gl_pathc = 0;
    pglob->gl_matchc = 0;
    pglob->gl_offs = 0;
    pglob->gl_flags = 0;
    pglob->gl_errfunc = NULL;
  }
  return;
}

static void newpath(glob_t *pglob, char *path)

/* This routine adds a new path to an existing pglob structure. */

{
  int n;

  n = ++pglob->gl_pathc;
  ++pglob->gl_matchc;
  pglob->gl_pathv = srealloc(pglob->gl_pathv, n*sizeof(void*));
  pglob->gl_pathv[n-1] = smalloc(strlen(path)+1);
  strcpy(pglob->gl_pathv[n-1], path);
}

static void freeglob(glob_t *pglob)

/* This routine deallocates a pglob structure. */

{
  if(pglob != NULL) {
    freepath(pglob);
    sfree(pglob);
  }
  return;
}

static context_t *newcontext()

/* This routine returns a pointer to a new empty context element.  Use
   freecontext to deallocate. */

{
  context_t *context;

  context = smalloc(sizeof(context_t));
  context->gl_next = -1;
  context->pattern = NULL;
  context->pglob = newpglob();
  context->next = NULL;
  return context;
}

static void freepattern(context_t *context)

/* This routine frees any allocated space inside of a context element, but
   does not free the context element itself intact.  All data fields are 
   zeroed.  The link to the next context element (context->next) is left
   intact. */

{
  if(context != NULL) {
    context->gl_next = -1;
    sfree(context->pattern);
    freepath(context->pglob);
  }
  return;
}

static void newpattern(context_t *context, char *pattern)

/* This routine stores a new pattern string in an existing context element.
   Any existing pattern string is discarded. */

{
  if(context != NULL) {
    freepattern(context);
    if(pattern == NULL)
      context->pattern = NULL;
    else {
      context->pattern = smalloc(strlen(pattern)+1);
      strcpy(context->pattern, pattern);
    }
  }
  return;
}

static void freecontext(context_t *context)

/* Routine to recursively free all allocated memory blocks in a context
   structure linked list. */

{
  if(context != NULL) {
    freepattern(context);
    freeglob(context->pglob);
    freecontext(context->next);
    sfree(context);
  }
  return;
}

static int numword(char *string)
 
/* This function returns the number of whitespace-delimited words in string. */
 
{
  int n = 0;		/* Word count */
 
  while(isspace(*string))++string;	/* Skip over leading whitespace */
  while(*string!='\0') {
 
  /* Skip next word */
 
    ++n;
    while(!isspace(*string) && *string!='\0')++string;
    while(isspace(*string))++string;
  }
  return n;
}
 
static size_t numcword(char *string)
 
/* This function returns the number of characters in the word starting at
   string */
 
{
  size_t nc=0;		/* Character count. */
  while(!isspace(string[nc]) && string[nc]!='\0')++nc;
  return nc;
}
 
static char *wordpt(char *string, int n)
 
/* This function returns a pointer to word n of string.  If string has fewer
   than n words, the return value points to the string terminator. */
 
{
  while(isspace(*string))++string;	/* Skip leading whitespace */
 
/* Skip n-1 words or until we hit the string terminator. */
 
  while(--n>0 && *string!='\0') {
    while(!isspace(*string) && *string!='\0')++string;
    while(isspace(*string))++string;
  }
  return string;
}
 
static void unglob(context_t *context)

/* Routine to split the leading context element into separate context
   elements for each pattern match. */

{
  context_t *context_tail, *con, *conlast;
  glob_t *lglob;
  int npath;
  int i;

/* Do nothing if the leading context element doesn't have a glob list with
   at least one element. */

  if(context->pglob == NULL)
    return;
  npath = context->pglob->gl_pathc;
  if(npath<=0)
    return;

/* We have a glob list with at least one element.  Promote each glob element 
   to indepedent context elements.  First detach the leading context element
   from the rest of the list, save important information, and zero the
   fields in the leading element. */

  lglob = context->pglob;
  context_tail = context->next;
  context->gl_next = -1;
  sfree(context->pattern);
  context->pattern = NULL;
  context->pglob = newpglob();
  context->next = NULL;

/* Allocate (npath-1) additional context elements. */

  con = context;
  for(i=1; i<npath; ++i) {
    con->next = newcontext();
    con = con->next;
  }

/* Now loop over all of the leading context elements and set the pattern
   from the original glob list. */

  con = context;
  for(i=0; i<npath; ++i) {
    newpattern(con, lglob->gl_pathv[i]);
    conlast = con;
    con = con->next;
  }

/* Reattach the tail of the original context list and clean up. */

  conlast->next = context_tail;
  freeglob(lglob);
  return;
}

static void translate(char *env)
 
/* This function translates the environment string contained in env in situ.
   This function calls getenv and edits the resulting string in various ways.
 
   1.  translate converts a { , } style list to a whitespace-delimited list.
 
   2.  Various default translations are provided for undefined environment
       variables (refer to the body of the routine for details).
 
   3.  If no default translations apply to an undefined environment variable,
       than it is translated as a search list to allow reading of nfs mounted
       VMS files.  For example, the environment variable "calib.dat" would be
       translated as the list "$calib.dat CALIB.DAT"
 
      --- The upper case translation is no longer performed.  ALL 1-MAR-1994
      --- So "calib.dat" gets translated to "$calib.dat"
 
*/
 
{
  char temp[ARG_MAX];
  char *value;		/* Pointer returned by getenv. */
  char *pt, *npt;	/* Temporary pointers */
  size_t i=0, j=0;
 
  value = getenv(env);
  if(value!=NULL && strlen(value)>0) {
    if(strlen(value)>=ARG_MAX) {
      fprintf(stderr, "translate: buffer overflow\n");
      abort();
    }
    strcpy(temp, value);
 
  /* Copy and edit temp back to env */
 
    while(temp[i]!='\0') {
      if(temp[i]==',')
        env[j++] = ' ';
      else if(temp[i]!='{' && temp[i]!='}')
        env[j++] = temp[i];
      ++i;
    }
    env[j] = '\0';
  }
  else {
 
/* Getenv failed.  Default translations handled here */
 
/* Check for subdirectory enviromnemt variables (contain __). */
 
    pt = strstr(env, "__");
    if(pt != NULL) {
      while((npt=strstr(pt+2, "__")) != NULL)
	pt = npt;
      i = pt-env;
      strncpy(temp, env, i);
      strncpy(&env[1], temp, i);
      env[0]='$';
      env[i+1] = '/';
    }
 
/* D0library environment variables (start with d0). */
 
    else if(!strncmp(env, "d0", 2) && strcmp(env, "d0library")) {
      strcpy(temp, "$d0library/");
      strcat(temp, &env[2]);
      strcpy(env, temp);
    }
 
/* Dbl3 server environment variables (start with dbl3serv). */
 
    else if(!strncmp(env, "dbl3serv", 8) && strcmp(env, "dbl3serv")) {
      strcpy(temp, "$dbl3serv/");
      strcat(temp, &env[8]);
      strcpy(env, temp);
    }
 
/* Dbl3 environment variables (start with dbl3). */
 
    else if(!strncmp(env, "dbl3", 4) && strcmp(env, "dbl3")) {
      strcpy(temp, "$dbl3/");
      strcat(temp, &env[4]);
      strcpy(env, temp);
    }
 
/* Production environment variables (start with prod). */
 
    else if(!strncmp(env, "prod", 4) && strcmp(env, "prod")) {
      strcpy(temp, "$prod/");
      strcat(temp, &env[4]);
      strcpy(env, temp);
    }

/* Project disk environment variables (start with prjroot). */

    else if(!strncmp(env, "prjroot", 7) && strcmp(env, "prjroot")) {
      strcpy(temp, "$project_disks/");
      strcat(temp, &env[7]);
      strcpy(env, temp);
    }
 
/* Project disk root environment variable ($project_disks). */

    else if(!strcmp(env, "project_disks")) {
      strcpy(env, "/prj_root");
    }
 
/* Temp disk environment variables (start with tmproot). */

    else if(!strncmp(env, "tmproot", 7) && strcmp(env, "tmproot")) {
      strcpy(temp, "$temp_disks/");
      strcat(temp, &env[7]);
      strcpy(env, temp);
    }
 
/* Temp disk root environment variable ($temp_disks). */

    else if(!strcmp(env, "temp_disks")) {
      strcpy(env, "/tmp_root");
    }
 
/* Any other environment variable -- translate as search list of a)
   untranslated EV with leading dollar and b) upper cased EV. */
 
    else {
      strcpy(temp, "$");
      strcat(temp, env);
/*
   This part is commented out - searching for the upper case file name
   causes the csh echo command to hang (don't know why - ALL 1-MAR-1994)
 
   ---- This part did the upper case translation - is no longer performed
 
      strcat(temp, " ");
      for(i=0;env[i] != '\0';++i)
	env[i] = toupper(env[i]);
      strcat(temp, env);
*/
      strcpy(env, temp);
    }
  }
  return;
}
 
static int globenv(context_t *context)
 
/* This function operates similarly to glob, expanding one environment 
   variable in the leading context element.  The result of the expansion is 
   stored in the pglob substructure.  Multiple expansion is possible. */
 
{
 
  char *head;	        /* head = part of string before EV */
  char *tail;	        /* tail = part of string after EV */
  char env[ARG_MAX];	/* environment variable */
  char *temp;           /* Temporary pointer. */
  char path[STRLEN];    /* Buffer for creating path string. */
  int nchead, nctail, nctemp;
  size_t i, j;	        /* Array indices */
  int n;		/* Number of words in translated EV */
 
/* First parse the pattern into head, environment variable and tail.  An
   environment variable begins with $ (dollar sign) and is terminated by a
   / (slash), } (right brace) or any non-graphic character (i.e. space or
   any control character, including null). */
 
  i=0;
  while(context->pattern[i]!='$' && context->pattern[i]!='\0')++i;

/* If we didn't find an environment varialbe, then copy the pattern to
   the path list. */

  if(context->pattern[i]=='\0')	{
    freepath(context->pglob);
    newpath(context->pglob, context->pattern);
    return 0;
  }

/* Found and env variable.  Save a pointer to the head string. */

  head = context->pattern;
  nchead = i;
 
/* Extract the env variable. */
 
  j = ++i;		/* Remember start of EV */
  while(isgraph(context->pattern[i]) && 
        context->pattern[i] != '/' && 
        context->pattern[i] != '}')++i;
  strncpy(env, context->pattern+j, i-j);
  env[i-j] = '\0';
 
/* Save the tail string. */
 
  tail = context->pattern+i;
  nctail = strlen(tail);
 
/* expand the EV. */
 
  translate(env);
 
/* Construct the result list */
 
  n = numword(env);
  
  freepath(context->pglob);
  for(i=0; i<n; ++i) {
    temp = wordpt(env, i+1);
    nctemp = numcword(temp);
    *path = '\0';
    strncat(path, head, nchead);
    strncat(path, temp, nctemp);
    strncat(path, tail, nctail);
    newpath(context->pglob, path);
  }
  if(n==1 && strcmp(context->pattern,context->pglob->gl_pathv[0])==0)
    return 0;
  return 1;
}
 
static int globfatmen(context_t *context)

/* This function operates similarly to glob by wildcard-expanding the
   directory portion of a fatmen name stored in the leading context element.
   The result of the expansion is stored in the pglob substructure. */

{
  int wild = 0;                     /* Wildcard flag. */
  int wild_dir=0;                   /* Wild directory flag. */
  int fatmen;                       /* Fatmen flag. */
  int match;
  int i;
  char *host, *user, *password;     /* Remote exec information. */
  char command[STRLEN];             /* Remote command buffer. */
  char head[STRLEN], tail[STRLEN];
  char *gen, *pt;
  char hostarg[STRLEN], *hostpt;
  int sock;                         /* Remote exec socket; */
  char line[STRLEN];
  FILE *remote;

/* Does the current pattern contain a fatmen generic name? */

  fatmen = !strncmp(context->pattern, "//FNAL/D0", 9);
  if( !fatmen )
    return 0;

/* Detremine if the current fatmen name contains any file or directory 
   wildcards. */

  i=0;
  while(!wild && context->pattern[i] != '\0') {
    if( context->pattern[i] == '*' || context->pattern[i] == '%' )
      wild = 1;		/* Found a wildcard */
    ++i;
  }
  if(wild && strchr(&context->pattern[i], '/')!=NULL)
    wild_dir = 1;
  freepath(context->pglob);
  if( !wild ) {
    newpath(context->pglob, context->pattern);
    return 0;
  }

/* Preliminaries.  Get remote exec information. 
   Get remote host name from env. variable FATMEN_NODE or VAXLIB_NODE. */

  host = getenv("FATMEN_NODE");
  if(host == NULL)
    host = getenv("VAXLIB_NODE");
  if(host == NULL)
    return 0;

/* Search .netrc for default user and password.  This is to allow the host 
   to be specified by alias in .netrc.  Rexec will get the required 
   information from .netrc for fully qualified host names in .netrc. */
 
  user = getusernetrc(host);
  if(user != NULL)
    password = getpassnetrc(host,user);
  else
    password = NULL;

/* Wildcard expand fatmen directories. */

  if(wild_dir) {

  /* Parse the generic name into head (directory) and tail (file) parts. */

    strcpy(head, context->pattern);
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
      fprintf(stderr, "globfatmen: rexec failed");
      return 0;
    }

  /* Read output from remote command. */

    remote = fdopen(sock, "r");
    if(remote == NULL) {
      fprintf(stderr, "globfatmen: fdopen failed");
      return 0;
    }
    while (fgets(line, sizeof(line), remote) != NULL) {
      fputs (line,stderr);

    /* See if this line contains a generic name. */

      gen = strstr(line, "//FNAL/D0/");
      if(gen != NULL && strstr(line, "Current Working Directory") == NULL) {
	pt = gen;
	while(!isspace(*pt) && *pt != '\0')
	  ++pt;
	*pt = '\0';
	strcpy(head, gen);
	strcat(head, "/");
	strcat(head, tail);

      /* See if this generic name is alreay in the list. */

	match = 0;
	for(i=0; i<context->pglob->gl_pathc; ++i) {
	  if(strstr(context->pglob->gl_pathv[i], head) != NULL) 
	    match = 1;
	}
	if( ! match )
	  newpath(context->pglob, head);
      }
    }
    fclose(remote);
    if(context->pglob->gl_pathc > 0)
      return 1;
  }

/* No wild directories, wildcard expand fatmen filenames. */

  else {

  /* Assemble the remote command. */

    strcpy(command, "@d0$unix$vms:fatmen_expand \"");
    strcat(command, context->pattern);
    strcat(command, "\"");

  /* Remote execute. */

    strcpy(hostarg, host);
    hostpt = hostarg;
    sock = rexec (&hostpt, getservbyname ("exec", "tcp")->s_port,
		  user, password, command, 0);
    if (sock < 0) {
      fprintf(stderr, "globfatmen: rexec failed");
      return 0;
    }

  /* Read output from remote command. */

    remote = fdopen(sock, "r");
    if(remote == NULL) {
      fprintf(stderr, "globfatmen: fdopen failed");
      return 0;
    }
    while (fgets(line, sizeof(line), remote) != NULL) {
      fputs (line,stderr);

    /* See if this line contains a generic name. */

      gen = strstr(line, "//FNAL/D0/");
      if(gen != NULL && strstr(line, "Generic filename") != NULL) {
	pt = gen;
	while(!isspace(*pt) && *pt != '\0')
	  ++pt;
	*pt = '\0';

      /* See if this generic name is alreay in the list. */

	match = 0;
	for(i=0; i<context->pglob->gl_pathc; ++i) {
	  if(strstr(context->pglob->gl_pathv[i], gen) != NULL) 
	    match = 1;
	}
	if( ! match )
	  newpath(context->pglob, gen);
      }
    }
    fclose(remote);
    if(context->pglob->gl_pathc > 0)
      return 1;
  }
  return 0;
}
 
char *find_file(char *ifile, char *rfile, context_t **pcontext)
 
/*
C-----------------------------------------------------------------------
C-
C- Name      : find_file
C-
C- Purpuse   : Translate a UNIX-style filename containing environment
C-             variables into a standard UNIX filename.
C-
C- Returned value: Pointer to resultant filename.  A null string (not a null
C-                 pointer) is returned in case of failure.
C-
C- Arguments:
C-
C- ifile (read only)   - Input filename.
C- rfile (write only)  - Resultant filename.
C- context (modify)    - Pointer to context pointer.
C-
C- Created 1-JUL-1991   Herbert B. Greenlee
C-
C- *context should be set to NULL on the first call and then not modified on
C- subsequent calls in the same file stream.  Find_file_end should be called
C- at the end of the file stream to deallocate the context.
C-
C-----------------------------------------------------------------------
*/
 
{
  context_t *context, *context_tail;  /* Context pointers. */
  char temp[STRLEN];                  /* Temporary buffer for filename. */
  size_t i, j;
  struct stat buf;		      /* Info from stat */
  int staterr;			      /* Error code from stat */
  int fatmen=0;
  int tryfile;                        /* Flag to try (stat) file. */
  int found;                          /* Flag indicating success. */
  int gflgs;                          /* Glob() flags. */
 
/* If the current context is NULL, allocate and initialize a new one.
   (This is the only time we look at ifile). */
 
  context = *pcontext;
  if(context==NULL) {
 
  /*Allocate and initialize a new context structure. */
 
    context = newcontext();
 
  /* Copy ifile to the pattern buffer, editing out any whitespace. */
 
    j = 0;
    for(i=0; ifile[i]!='\0'; ++i)
      if(!isspace(ifile[i]))
	temp[j++] = ifile[i];
    temp[j] = '\0';
    newpattern(context, temp);
  }
 
/* We now have a valid context from this or a previous call.  Loop over
   the present context until we find a file to return, or until the context
   is empty.  Each execution of the loop will modify the context, but
   may of may not produce a candidate file to return. */

  do {
    if(context->gl_next >= context->pglob->gl_pathc) {
      context_tail = context->next;
      context->next = NULL;
      freecontext(context);
      context = context_tail;
      tryfile = 0;
    }
    else if(context->gl_next >= 0) {
      strcpy(temp, context->pglob->gl_pathv[context->gl_next]);
      ++context->gl_next;
      tryfile = 1;
    }

    else {

  /* Current context element is unglobbed if we get here.  Do environment
     variable and wildcard globbing.  Is it a fatmen generic name? */

      fatmen = !strncmp(context->pattern, "//FNAL/D0", 9);
      if( fatmen ) {
	while(globfatmen(context))   /* Recursively glob fatmen names. */
	  unglob(context);
	context->gl_next = 0;
	tryfile = 0;
      }
      else {
	while(globenv(context))      /* Recursively glob env. variables. */
	  unglob(context);
	freepath(context->pglob);
	gflgs = GLOB_QUOTE | GLOB_NOMAGIC | GLOB_ALTNOT;
	glob(context->pattern, gflgs, 0, context->pglob);
	context->gl_next = 0;
	tryfile = 0;
      }
    }

/* See if rfile exists.  Do not check fatmen generic names. */
 
    if(tryfile) {
      strcpy(rfile, temp);
      if( fatmen )
	found = 1;
      else {
	staterr = stat(rfile, &buf);
	found = (staterr == 0);
      }
    }
    else
      found = 0;
  }
  while(!found && context!=NULL);
  if(!found)
    *rfile = '\0';
  *pcontext = context;
  return rfile;
}
 
long find_file_(char *ifile, char *rfile, context_t **pcontext,
  long len_ifile, long len_rfile)
 
/*
C-----------------------------------------------------------------------
C-
C- Name      : find_file
C-
C- Purpuse   : Fortran callable verion of find_file.  Translate a UNIX-style
C-             filename containing environment variables into a standard UNIX
C-             filename.
C-
C- Fortran calling sequence:
C-
C-      LOGICAL FIND_FILE, OK
C-             -or-
C-      INTEGER FIND_FILE, OK
C-      OK = FIND_FILE(IFILE, RFILE, CONTEXT)
C-
C- Returned value: 0 (false) - failure or no more files
C-                 1 (true)  - success
C-
C- Arguments:
C-
C- IFILE (CHARACTER, read only)  - Input filename.
C- RFILE (CHARACTER, write only) - Resultant filename.
C- CONTEXT (INTEGER*4, modify)   - Address of context string.
C-
C- Created 9-JUL-1991   Herbert B. Greenlee
C-
C- Usage:
C-
C- CONTEXT should be set to zero prior to the first call and not modified
C- thereafter.  It is possible to have multiple streams with different context
C- variables.  LIB$FIND_FILE_END deallocates the context.
C-
C-----------------------------------------------------------------------
*/
 
{
  char itemp[STRLEN];	/* Local copy of input string */
  char rtemp[STRLEN];	/* Local copy of output string */
 
/* Call find_file using a local copy of the input string */
 
  find_file(cstring(ifile, len_ifile, itemp, STRLEN), rtemp, pcontext);
 
/* Copy result string back to calling program */
 
  fstring(rtemp, rfile, len_rfile);
  if(strlen(rtemp) != 0)
    return 1;
  else
    return 0;
}
 
void find_file_end(context_t **pcontext)
 
/*
C-----------------------------------------------------------------------
C-
C- Name      : find_file_end
C-
C- Purpuse   : Release the specified context
C-
C- Returned value: None
C-
C- Arguments:
C-
C- context (modify) - Address of context string pointer from previous call.
C-                    Will be set to zero on return.
C-
C- Created 1-JUL-1991   Herbert B. Greenlee
C-
C-----------------------------------------------------------------------
*/
 
{
  freecontext(*pcontext);
  *pcontext = NULL;
  return;
}
 
long find_file_end_(context_t **pcontext)
 
/*
C-----------------------------------------------------------------------
C-
C- Name      : find_file_end_
C-
C- Purpuse   : Release the specified context (Fortran callable).
C-
C- Fortran calling sequence:
C-
C-      CALL FIND_FILE_END(CONTEXT)
C-
C- Returned value: 1 (true)
C-
C- Arguments:
C-
C- CONTEXT (INTEGER*4, modify) - Address of context string.  Will be set to
C-                               zero on return.
C-
C- Created 9-JUL-1991   Herbert B. Greenlee
C-
C-----------------------------------------------------------------------
*/
 
{
  find_file_end(pcontext);
  return 1;
}
