/*
C-----------------------------------------------------------------------
C-
C- Name      : fn
C-
C- Purpuse   : This main program interfaces the shell to find_file.
C-             It accepts a series of file specifications as arguments
C-             and writes a whitespace-delimited list of files to standard
C-             output.  If no arguments are specified, filenames are read
C-             from standard input.  Only the first file with a given
C-             filename is kept.  The directory portion of the pathname and
C-             the file extension are ignored when looking for the first
C-             filename.
C-
C- Usage:
C-
C-     fn [-d] file(s)
C-
C- Options:
C-
C- -d - Output only duplicate (rather than the first) instance of a file.
C-
C- Created 21-Apr-1992   Herbert B. Greenlee
C-
C-----------------------------------------------------------------------
*/
 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "unix.h"
 
#define XOR(a,b) (((a) && !(b)) || (!(a) && (b)))
 
int main(int argc, char **argv, char **envp) {
  int i, j;
  int read_from_input=0;
  int reverse=0;
  int add_to_list, printit;
  char ifile[STRLEN];   /* Input file */
  char rfile[STRLEN];	/* Result file */
  char temp[STRLEN];
  context_t *context;
  char *filename, *extension, *slash, *period;
  struct file {
    char *name;
    struct file *next;
  };
  struct file *file;
  struct file base;
  int fatmen;
 
/* Initialize file list. */
 
  base.name = NULL;
  base.next = NULL;
 
/* Look for -d option. */
 
  for(i=1; i<argc; ++i) {
    if(!strcmp(argv[i], "-d")) {
      reverse = 1;
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
 
  /* Get next file specification. */
 
    if(read_from_input)
      getword(ifile, STRLEN);
    else
      strcpy(ifile, argv[i]);
 
  /* Expand this file specification by calling find_file. */
 
    do {
      find_file(ifile, rfile, &context);
      if(*rfile != '\0') {
 
    /* Got a file.  Find the name portion of the filename. */
 
	fatmen = !strncmp(rfile, "//FNAL/D0", 9);
	strcpy(temp, rfile);
	filename = temp;
	for(j=0;j<8 || !fatmen;++j) {
	  slash = strchr(filename, '/');
	  if(slash == NULL)
	    break;
	  filename = ++slash;
	}
	extension = filename;
	for(;;) {
	  period = strchr(extension, '.');
	  if(period == NULL)
	    break;
	  extension = ++period;
	}
	if(extension != filename)
	  *extension = '\0';
 
    /* See if this filename matches any name in the list. */
 
	file = &base;
	while( file->name != NULL) {
	  if(!strcmp(file->name, filename))
	    break;
	  file = file->next;
	}
	add_to_list = (file->name == NULL);
	printit = XOR(add_to_list, reverse);
 
    /* Add this filename to list. */
 
	if(add_to_list) {
	  file->name = malloc((strlen(filename)+1) * sizeof(*filename));
	  strcpy(file->name, filename);
	  file->next = malloc(sizeof(*file));
	  file->next->name = NULL;
	  file->next->next = NULL;
	}
 
    /* Print filename. */
 
	if(printit) {
          printf("%s\n",rfile);
	}
      }
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
