 /****************************************************************************
  *
  * Name:    f_depend
  *
  * Purpose: Generate dependencies for UNIX fortran.
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
void find_includes();
%}
S                       [ \t]+
s                       [ \t]*
%%
^{S}[Ii][Nn][Cc][Ll][Uu][Dd][Ee]{s}'.*'.*\n find_includes();
^.*\n ;
%%
void find_includes()
 {
 
 /* Extract the UNIX filename from INCLUDE statements. */
 
   fch = strchr(yytext, '\'') + 1;
   lch = strchr(fch, '\'');
   nch = lch - fch;
   strncpy(ifile, fch, nch);
   ifile[nch] = '\0';
   context = NULL;
   find_file(ifile, rfile, &context);
   find_file_end(&context);
   if(rfile != NULL && *rfile != '\0')
     printf("\\\n  %s", rfile);
   else
     fprintf(stderr, "error: include file %s not found\n", ifile);
 }

int main()
{
  yylex();
  return 0;
}
