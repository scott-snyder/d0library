/*
C-----------------------------------------------------------------------
C-
C- Name      : vff
C-
C- Purpuse   : This main program interfaces the shell to lib_find_file.
C-             It accepts one or more file specifications as arguments and
C-             writes the the first matching filename to stdout.  If no
C-             arguments are specified, filenames are read from standard
C-             input.
C-
C- Usage:
C-
C-     vff [-q] file(s)
C-
C- Options:
C-
C- -q - Quiet.  Suppress "file not found" messages to standard error.
C-
C- Created 4-JUL-1991   Herbert B. Greenlee
C-
C-----------------------------------------------------------------------
*/
 
#include <stdio.h>
#include <stdlib.h>
#include "unix.h"
 
int main(int argc, char **argv, char **envp) {
  int i, j;
  int read_from_input=0;
  int quiet=0;
  char ifile[STRLEN];   /* Input file */
  char rfile[STRLEN];	/* Result file */
  char filelist[STRLEN];
  context_t *context;

  filelist[0] = '\0';

/* Look for -q option. */
 
  for(i=1; i<argc; ++i) {
    if(!strcmp(argv[i], "-q")) {
      quiet = 1;
      --argc;
      for(j=i; j<argc; ++j)
	strcpy(argv[j], argv[j+1]);
      break;
    }
  }
 
/* Determine input source (arg list or standard input). */
 
  if(argc <= 1)
    read_from_input = 1;
  for(i=1; i<argc || read_from_input; ++i) {
    context = NULL;
    if(read_from_input)
      getword(ifile, STRLEN);
    else
      strcpy(ifile, argv[i]);
 
/* Remember this file (in case of error). */
 
    if(strlen(ifile) + strlen(filelist) + 1 < STRLEN) {
      strcat(filelist, " ");
      strcat(filelist, ifile);
    }
    lib_find_file(ifile, rfile, &context);
    find_file_end(&context);
    if(*rfile != '\0') {
      printf("%s\n",rfile);
      break;
    }
    if(*ifile == '\0')
      break;
  }
  if(*rfile == '\0') {
    if(!quiet)fprintf(stderr, "vff: file not found:%s\n", filelist);
    exit(1);
  }
  else
    exit(0);
}
