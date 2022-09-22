/*
C-----------------------------------------------------------------------
C-
C-
C- This file contains the following public entry points:
C-
C- 1. lib_find_file      - Find the first (or next) of the files in a given
C-                         stream.
C- 2. lib$find_file      - Fortran callable.
C-
C- 3. lib$find_file_end  - Fortran callable interface to find_find_end.
C-
C- Lib_find_file performs filename expanssion using VMS filenames as input.
C- VMS filenames are translated to their UNIX equivalants and then find_file is
C- called.  The following transaltions are peformed.
C-
C- 1.  All characters are translated to lower case.
C-
C- 2.  D0FS logicals referring to specific disks begin with the string
C-     "d0fs_" and end with a colon.  These are translated as follows:
C-
C-     d0fs_xxx$yyy: -> /d0fs/disks/xxx_yyy/
C-
C-     Normally xxx is a node and yyy refers to a specific disk.  Staging
C-     disk ("stage1") are changed to regular disks ("data1")
C-
C- 3.  VMS system and d0 logicals are recognized by the fact that they contain
C-     dollar signs and are terminated by a colon.  They are translated
C-     according to the following rules.
C-
C-     xxx$yyy:      -> $xxxyyy/
C-     xxx$yyy$zzz:  -> $xxxyyy__zzz/
C-
C-     An exception to the above rules are logical names that end in
C-     $root, $hroot or $home.  In these cases, the trailing word ($root,
C-     $hroot or $home) is ignored.  The forms containing double underscore
C-     are normally converted to subdirectories by find_file.  Many
C-     environment variables corresponding to D0 logical names are given
C-     default translations by the routine "translate" in find_file.c,
C-     including those for d0library, production, dbl3, project and temp
C-     disk areas.
C-
C- 4.  VMS directory syntax is translated to UNIX directory syntax:
C-
C-     [xxx.yyy] -> xxx/yyy/
C-     [000000.xxx.yyy] -> xxx/yyy/
C-
C- 5.  VMS wildcards are translated to UNIX wildcards:
C-
C-     * -> *
C-     % -> ?
C-
C- 6.  The version field of VMS filenames, if any, is ignored.
C-
C- 7.  If an initial find fails, and the VAX filename does not include an
C-     extension, then the extension .dat is added by default.  The extension
C-     .dat is not added if a file was found without adding .dat.
C-
C- 8.  UNIX absolute pathnames are not translated.
C-
C- Go to the function definitions for documentation on the calling sequences.
C-
C-----------------------------------------------------------------------
*/
 
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include "unix.h"
 
char *lib_find_file(char *ifile, char *rfile, context_t **context)
 
/*
C-----------------------------------------------------------------------
C-
C- Name      : lib_find_file
C-
C- Purpuse   : Translate a VMS-style filename containing d0 logicals
C-             into a standard UNIX filename.
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
C- Created 1-AUG-1991   Herbert B. Greenlee
C-
C- *context should be set to NULL on the first call to lib_find_file and not
C- modified on subsequent calls in the same file stream.  Find_file_end
C- should be called to deallocate the context after the last file in the
C- stream.
C-
C-----------------------------------------------------------------------
*/
 
{
  char ufile[STRLEN];         /* Buffer to build UNIX filename */
  char vfile[STRLEN];         /* VMS filename */
  char lcfile[STRLEN];        /* L.c. copy of input (ifile) */
  char temp[STRLEN];
  char *cpt, *found, *tail;
  int ni;                     /* Number of character parsed so far */
  int nu;                     /* Number of unix characters so far */
  int i,j,n;
 
  found = NULL;
 
/* Find the first non-whitespace character in the input filename. */
 
  j = 0;
  while(isspace(ifile[j]))
    ++j;
 
/* If this is a UNIX absolute pathname or FATMEN generic name or the context
   has already been allocated, just pass the call to find_file. */
 
  if(ifile[j] == '/' || ifile[j] == '$' || *context != NULL) {
    found = find_file( &ifile[j], rfile, context);
    if(strstr(rfile, "//FNAL/D0") == NULL)
      return found;
    fatmen_find(rfile, STRLEN, vfile);
  }
  else
    strcpy(vfile, ifile);
 
/* Not a UNIX absolute pathname.  Make a lowercase copy of input filename. */
 
  for(i=0; vfile[j]!='\0'; ++i)
    lcfile[i] = tolower(vfile[j++]);
  lcfile[i] = '\0';
 
/* The following section of code translated the VMS pathname to UNIX format.
   The translation is build in the array ufile. */
 
  ni = 0;
  nu = 0;
 
/* Skip over a decnet node name, if any. */
 
  cpt = strstr(&lcfile[ni], "::");
  if(cpt != NULL)
    ni = cpt - &lcfile[-2];
 
  cpt = strchr(&lcfile[ni], ':');
 
/* Look for D0FS disk logicals. */
 
  if(cpt != NULL && !strncmp(&lcfile[ni], "d0fs_", 5)) {
 
  /* Translation starts with root of d0fs files. */
 
    strcpy(&ufile[nu], "/d0fs/disks/");
    nu += 12;
    ni += 5;
 
  /* Copy the d0fs node name up to the terminating dollar or the
     end of the device field.  Terminate with '_'. */
 
    for(; &lcfile[ni]<cpt; ++ni) {
      if(lcfile[ni] == '$') {
	++ni;
	break;
      }
      else
        ufile[nu++] = lcfile[ni];
    }
    ufile[nu++] = '_';
 
  /* Change disk names beginning with "stage" to "data". */
 
    if(!strncmp(&lcfile[ni], "stage", 5)) {
      strncpy(&ufile[nu], "data", 4);
      nu += 4;
      ni += 5;
    }
 
  /* Copy remainder of disk name. */
 
    for(; &lcfile[ni]<cpt; ++ni)
      ufile[nu++] = lcfile[ni];
    ufile[nu++] = '/';    /* Add final directory seperator */
    ++ni;                 /* Skip final colon of device */
  }
 
/* Other VMS logicals (d0library, dbl3) handled here. */
 
  else if(cpt != NULL) {
 
  /* Got a device.  The translation always begins with a dollar sign. */
 
    strcpy(&ufile[nu], "$");
    ++nu;
 
  /* Copy the first word (up to a terminating dollor or the end of the device
     field). */
 
    for(; &lcfile[ni]<cpt; ++ni) {
      if(lcfile[ni] == '$') {
	++ni;
	break;
      }
      else
        ufile[nu++] = lcfile[ni];
    }
 
  /* Ignore root, hroot and home in second $-delimited field of logical. */
 
    if(!strncmp(&lcfile[ni], "root:", 5))
      ni += 4;
    else if(!strncmp(&lcfile[ni], "hroot:", 6))
      ni += 5;
    else if(!strncmp(&lcfile[ni], "home:", 5))
      ni += 4;
 
  /* Copy remaining fields.  Change $ to __.  Ignore $root, $hroot and
     $home.  */
 
    for(; &lcfile[ni]<cpt; ++ni) {
      if(lcfile[ni] == '$') {
        if(!strncmp(&lcfile[ni], "$root:", 6))
          ni += 4;
        else if(!strncmp(&lcfile[ni], "$hroot:", 7))
          ni += 5;
        else if(!strncmp(&lcfile[ni], "$home:", 6))
	  ni += 4;
	else {
	  strcpy(&ufile[nu], "__");
	  nu += 2;
	}
      }
      else
        ufile[nu++] = lcfile[ni];
    }
    ufile[nu++] = '/';    /* Add final directory seperator */
    ++ni;                 /* Skip final colon of device */
  }
 
/* Parse directory field */
 
  cpt = strchr(&lcfile[ni], ']');
  if(cpt != NULL) {
 
    if(lcfile[ni] == '[')
      ++ni;               /* Skip over initial [ */
 
  /* Got a directory.  Skip over an initial "000000." or "000000" , if any. */
 
    if(!strncmp(&lcfile[ni],"000000.",7))
      ni += 7;
    if(!strncmp(&lcfile[ni],"000000",6)) {
      ni += 6;
      if( ufile[nu-1] == '/' )
	--nu;
      else
	lcfile[nu++] = '.';
    }
 
  /* Copy directory fields.  Change . to / and % to ? */
 
    for(; &lcfile[ni]<cpt; ++ni) {
      if(lcfile[ni] == '.')
        ufile[nu++] = '/';
      else if(lcfile[ni] == '%')
        ufile[nu++] = '?';
      else
        ufile[nu++] = lcfile[ni];
    }
    ufile[nu++] = '/';    /* Add final directory separator */
    ++ni;                 /* Skip final ] */
  }
 
/* Copy the name and type fields (up to ; or string terminator).  Change %
   wildcard to ? */
 
  for(; lcfile[ni]!=';' && lcfile[ni]!='\0'; ++ni) {
    if(lcfile[ni] == '%')
      ufile[nu++] = '?';
    else
      ufile[nu++] = lcfile[ni];
  }
  ufile[nu] = '\0';       /* Add final terminator */
 
/* Now we are ready to call find_file.  If find_file has alread been called
   (because of a FATMEN generic name), then do not call it again. */
 
  if(found == NULL)
    found = find_file(ufile, rfile, context);
  else
    strcpy(rfile, ufile);
 
/* If the initial find fails, convert the tail portion of the filename to
   upper case and try again. */
 
  if(*rfile == '\0') {
    strcpy(temp, ufile);
    tail = strrchr(temp, '/');
    if(tail == NULL)
      tail = temp;
    while(*tail != '\0') {
      *tail = toupper(*tail);
      ++tail;
    }
    found = find_file(temp, rfile, context);
  }
 
/* If find still fails and the filename doesn't have an extension,
   add the default extension .dat. */
 
  if(*rfile == '\0') {
    strcpy(temp, ufile);
    tail = strrchr(temp, '/');
    if(tail == NULL)
      tail = temp;
    if(strchr(tail, '.') == NULL) {
      strcat(tail, ".dat");
      found = find_file(temp, rfile, context);
    }
  }
 
/* If find still fails and the filename doesn't have an extension,
   convert the tail portion of the filename to upper case and add the
   default extension .DAT. */
 
  if(*rfile == '\0') {
    strcpy(temp, ufile);
    tail = strrchr(temp, '/');
    if(tail == NULL)
      tail = temp;
    if(strchr(tail, '.') == NULL) {
      strcat(tail, ".dat");
      while(*tail != '\0') {
	*tail = toupper(*tail);
	++tail;
      }
      found = find_file(temp, rfile, context);
    }
  }
 
/* Done. */
 
  return found;
}

/* 
C-
C-    Add a dynamic table of map between index and context pointer address, 
C-    pass an index instead of context pointer address from C to Fortran, 
C-    to solve 64 bit pointer to Fortran integer passing problem 
C-    September 9, 1997 by Dong Zhao
*/

typedef struct contxt_table_struct {
  int index;               /* index of the context pointer address */
  context_t *cpa;          /* context pointer address */
} contxt_table;

static contxt_table **Pcont; /* static pointer to the table */
static int T_size = 0;       /* table size */
 
int lib$find_file_(char *ifile, char *rfile, int *indexp,
  int len_ifile, int len_rfile)
 
/*
C-----------------------------------------------------------------------
C-
C- Name      : lib$find_file
C-
C- Purpuse   : Fortran callable verion of lib$find_file.  Translate a VMS-style
C-             filename containing d0 logicals into a standard UNIX
C-             filename.  This function is intended to be a partial emulation
C-             of the VMS run-time library routine of the same name.
C-
C- Fortran calling sequence:
C-
C-      LOGICAL LIB$FIND_FILE, OK
C-                -or-
C-      INTEGER LIB$FIND_FILE, OK
C-      OK = LIB$FIND_FILE(IFILE, RFILE, CONTEXT)
C-
C- Returned value: 0 (false) - failure or no more files
C-                 1 (true)  - success
C-
C- Arguments:
C-
C- IFILE (CHARACTER, read only)  - Input filename.
C- RFILE (CHARACTER, write only) - Resultant filename.
C- CONTEXT (INTEGER*4, modify)   - Was address of context string.
C-                                 Index of the context address  
C- Modified 9-SEP-1997  Dong Zhao
C- Created 9-JUL-1991   Herbert B. Greenlee
C-
C- Usage:
C-
C- CONTEXT should be set to zero prior to the first call and not modified
C- thereafter.  It is possible to have multiple streams with different context
C- variables. 
C-
C-----------------------------------------------------------------------
*/
 
{
  char itemp[STRLEN];	/* Local copy of input string */
  char rtemp[STRLEN];	/* Local copy of output string */
  context_t **context;

  if ( *indexp == 0 ) {   
    context_t *tmp;             /* temporary pointer to context address */
    tmp = NULL;
    context = &tmp;

    lib_find_file(cstring(ifile, len_ifile, itemp, STRLEN), rtemp, context);

    if ( *context != NULL ) {  /* add a new entry to the table */
      if ( T_size == 0 ) {
	Pcont = ( contxt_table ** )malloc( sizeof( contxt_table * ));
	Pcont[0] = ( contxt_table *)malloc( sizeof( contxt_table ));
	Pcont[0]->index = 1;
      }
      else {
	Pcont=(contxt_table **)realloc(Pcont,(T_size+1)*sizeof(contxt_table *));
	Pcont[T_size] = ( contxt_table *)malloc( sizeof( contxt_table ));
	Pcont[T_size]->index = Pcont[T_size-1]->index + 1;
      }
      Pcont[T_size]->cpa = tmp;
      *indexp = Pcont[T_size]->index;
      T_size++;
    }
  }
  else {   /* look up the table entries */
    int i;
    
    for ( i=0; i<T_size; i++ ) {
      if ( Pcont[i]->index == *indexp  ) {
	context = &(Pcont[i]->cpa);
	break;
      }
      if ( i == T_size - 1 ) {
	fprintf(stderr, "lib$find_file: Wrong context value used for input file\n");
	fprintf(stderr, "               %s\n", ifile);
	rfile="";
	return 0;
      }
    }

    lib_find_file(cstring(ifile, len_ifile, itemp, STRLEN), rtemp, context);
    
  }
      
/* Copy result string back to calling program */
 
  fstring(rtemp, rfile, len_rfile);
  if(strlen(rtemp) != 0)
    return 1;
  else
    return 0;
}
 
int lib$find_file_end_(int *indexp)
 
/*
C-----------------------------------------------------------------------
C-
C- Name      : lib$find_file_end
C-
C- Purpuse   : Release the specified context (Fortran callable).
C-
C- Fortran calling sequence:
C-
C-      CALL LIB$FIND_FILE_END(CONTEXT)
C-
C- Returned value: 1 (true)  - success
C-                 0 (false) - failure on wrong context value
C- Arguments:
C-
C- CONTEXT (INTEGER*4, modify) - Index of context string.  Will be set to
C-                               zero on return.
C-
C- Modified 9-SEP-1997  Dong Zhao
C- Created 9-JUL-1991   Herbert B. Greenlee
C-
C-----------------------------------------------------------------------
*/
 
{
  context_t **context;
  contxt_table **tmpcont;
  int i,j;

  if ( *indexp != 0 ) {
    for ( i=0; i<T_size; i++ ) {
      if ( Pcont[i]->index == *indexp  ) {
	context = &(Pcont[i]->cpa);
	find_file_end(context);
	break;
      }
      if ( i == T_size - 1 ) {
	fprintf(stderr, "lib$find_file_end: wrong context value used\n");
	return 0;
      }
    }
    /* delete a table entry */
    if (T_size <= 0) return 0;
    tmpcont=(contxt_table **)malloc((T_size-1)*sizeof(contxt_table *));
    for ( j=0; j<T_size-1; j++) {
      tmpcont[j] = ( contxt_table * )malloc( sizeof( contxt_table ));
      if ( j < i ) {
	tmpcont[j]->index = Pcont[j]->index;
	tmpcont[j]->cpa = Pcont[j]->cpa;
      }
      else {
	tmpcont[j]->index = Pcont[j+1]->index;
	tmpcont[j]->cpa = Pcont[j+1]->cpa;
      }
    }

    for ( j=0; j<T_size; j++ ) free(Pcont[j]);
    free(Pcont);

    Pcont = tmpcont;
    T_size--;
    *indexp = 0;
  }    
  return 1;
}
