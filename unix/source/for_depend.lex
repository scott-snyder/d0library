 /****************************************************************************
  *
  * Name:    for_depend
  *
  * Purpose: Generate dependencies for VAX fortran.
  *
  * Usage:
  *
  * % for_depend program.for
  *
  ****************************************************************************/
%{
#include "unix.h"
#include <stdlib.h>
#include <string.h>
#define STRLEN 256
#undef output
#define output(c)
char ifile[STRLEN];
char rfile[STRLEN];
char *fch, *lch;
context_t *context;
int nch;
%}
S                       [ \t]+
s                       [ \t]*
%%
^{S}[Ii][Nn][Cc][Ll][Uu][Dd][Ee]{s}'.*'.*\n find_includes();
^[Cc]&{S}[Ii][Nn][Cc][Ll][Uu][Dd][Ee]{s}'.*'.*\n find_includes();
^.*\n ;
%%
 find_includes()
 {
 
 /* Extract the VAX filename from INCLUDE statements.  Any trailing VMS
    switches like '/list' are removed. */
 
   fch = strchr(yytext, '\'') + 1;
   lch = strchr(fch, '/');
   if(lch == NULL)
     lch = strchr(fch, '\'');
   nch = lch - fch;
   strncpy(ifile, fch, nch);
   ifile[nch] = '\0';
   context = NULL;
   lib_find_file(ifile, rfile, &context);
   find_file_end(&context);
   if(*rfile != '\0')
     printf("\\\n  %s", rfile);
 }
