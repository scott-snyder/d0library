/*
C-----------------------------------------------------------------------
C-
C- Name      : check_exec
C-
C- Purpuse   : This program reads a list of whitespace-delimited 
C-             filenames, from standard input.  Any file whose first 
C-             two characters are #! is made executable.
C-
C- Created 3-Aug-1992   Herbert B. Greenlee
C-
C-----------------------------------------------------------------------
*/

#include "unix.h"
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>

int main(int argc, char **argv) {

  char filename[STRLEN];
  char firsttwo[3];
  FILE *file;
  struct stat status;
  mode_t execute;

/* Generate execute mode mask. */

  execute = S_IXUSR | S_IXGRP | S_IXOTH;

/* Read in next filename. */

  while(getword(filename, STRLEN) != NULL) {
    
  /* Open the file and read its first two characters.  Print an error
     message and skip this file if any errors occur. */

    file = fopen(filename, "r");
    if(file == NULL) {
      fprintf(stderr, "Check_exec: error opening file %s\n", filename);
      fclose(file);
      continue;
    }
    if(fgets(firsttwo, 3, file) == NULL) {
      fclose(file);
      continue;
    }

  /* Check for #! */

    if(firsttwo[0] == '#' && firsttwo[1] == '!') {

    /* Found a script.  See if it is already executable. */

      if(stat(filename, &status) != 0) {
	fprintf(stderr, "Check_exec: error checking file %s\n", filename);
	fclose(file);
	continue;
      }
      if(~status.st_mode & execute != 0) {
	if(chmod(filename, status.st_mode | execute) == 0)
	  printf("%s made executable\n", filename);
	else
	  fprintf(stderr, "Check_exec: error changing mode for file %s\n", filename);
      }
    }
    fclose(file);
  }
  exit(0);
}
