/*
C-----------------------------------------------------------------------
C-
C- Name      : fa
C-
C- Purpuse   : This main program interfaces the shell to find_file.  
C-             It accepts a series of file specifications as arguments 
C-             and writes a whitespace-delimited list of files to standard
C-             output.  If no arguments are specified, filenames are read 
C-             from standard input.
C-
C- Usage:
C-
C-     fa file(s)
C-
C- Created 4-JUL-1991   Herbert B. Greenlee
C-
C-----------------------------------------------------------------------
*/

#include <stdio.h>
#include <stdlib.h>
#include "unix.h"

int main(int argc, char **argv, char **envp) {
  int i, n;
  int read_from_input=0;
  char ifile[STRLEN];   /* Input file */
  char rfile[STRLEN];	/* Result file */

  if(argc <= 1)
    read_from_input = 1;
  for(i=1; i<argc || read_from_input; ++i) {
    if(read_from_input)
      getword(ifile, STRLEN);
    else
      strcpy(ifile, argv[i]);
    if(*ifile == '\0')
      break;
    n = readlink(ifile, rfile, STRLEN);
    if(n > 0 && n < STRLEN) {
      rfile[n] = '\0';
      printf("%s\n",rfile);
    }
    else
      printf("%s\n",ifile);
  }
  exit(0);
}
