/*
//
//   Purpose and Methods: A fortran callable interface to the POSIX stat routine
//     used to find the size of a file in bytes.
//
//   Inputs  :
//   Outputs :
//   Controls:
//
//   Created  28-OCT-1996   John Hobbs
//
*/
#include <stdlib.h>
#ifdef VMS 
# define FILESIZE posix_filesize
# include <stat.h>
#else
# define FILESIZE posix_filesize_
# include <sys/types.h>
# include <sys/stat.h>
# define stat_t struct stat
#endif

FILESIZE (const char *fname, int nameLen ) {
  char   *local_name;
  stat_t buffer;

  /* Make a C-style string from the file name. */
  local_name = malloc(nameLen+1);
  if( local_name == (char *)NULL ) return(-1);
  strncpy(local_name,fname,nameLen);
  local_name[nameLen]='\0';

  /* Make the actual call */
  if( !stat(local_name,&buffer) ) {
    free(local_name);
    return(buffer.st_size);
  }
  else {
    free(local_name);
    return(-2);
  }

}
