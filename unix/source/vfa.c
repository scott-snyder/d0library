/*
C-----------------------------------------------------------------------
C-
C- Name      : vfa
C-
C- Purpuse   : This main program interfaces the shell to lib_find_file.  
C-             It accepts a series of file specifications as arguments 
C-             and writes a whitespace-delimited list of files to standard
C-             output.  If no arguments are specified, filenames are read 
C-             from standard input.
C-
C- Usage:
C-
C-     vfa file(s)
C-
C- Created 4-JUL-1991   Herbert B. Greenlee
C-
C-----------------------------------------------------------------------
*/

#include <stdio.h>
#include <stdlib.h>
#include "unix.h"

int main(int argc, char **argv, char **envp) {
  int i;
  int read_from_input=0;
  char ifile[STRLEN];   /* Input file */
  char rfile[STRLEN];	/* Result file */
  context_t *context;

  if(argc <= 1)
    read_from_input = 1;
  for(i=1; i<argc || read_from_input; ++i) {
    context = NULL;
    if(read_from_input)
      getword(ifile, STRLEN);
    else
      strcpy(ifile, argv[i]);
    do {
      lib_find_file(ifile, rfile, &context);
      if(*rfile != '\0')
        printf("%s\n",rfile);
    }
    while(context != NULL);
    find_file_end(&context);
    if(*ifile == '\0')
      break;
  }
  if(*rfile == '\0')
    exit(1);
  else
    exit(0);
}
