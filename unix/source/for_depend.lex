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
void find_includes();
%}
S                       [ \t]+
s                       [ \t]*
%%
^{S}[Ii][Nn][Cc][Ll][Uu][Dd][Ee]{s}'.*'.*\n find_includes();
^[Cc]&{S}[Ii][Nn][Cc][Ll][Uu][Dd][Ee]{s}'.*'.*\n find_includes();
^.*\n ;
%%
void find_includes()
 {
 
 /* Extract the VAX filename from INCLUDE statements.  Any trailing VMS
    switches like '/list' are removed. */
 
   fch = strchr(yytext, '\'') + 1;
   lch = strchr(fch, '\'');
   char* lch2 = strchr(fch, '/');
   if (lch2 && lch2 < lch) lch = lch2;
   nch = lch - fch;
   strncpy(ifile, fch, nch);
   ifile[nch] = '\0';
   context = NULL;
   lib_find_file(ifile, rfile, &context);
   find_file_end(&context);
   if(*rfile != '\0')
     printf("\\\n  %s", rfile);
   else if (strncmp (ifile, "($", 2) == 0 || strncmp (ifile, "(STR$", 5) == 0)
     // vms library file
     ;
   else if (strncmp (ifile, "MLT$", 4) == 0)
     // farm production system
     ;
   else if (strncmp (ifile, "ELN$", 4) == 0)
     ;
   else if (strcmp (ifile, "pfarms.inc") == 0)
     ;
   else
     fprintf(stderr, "error: include file %s not found\n", ifile);
 }

int main()
{
  yylex();
  return 0;
}
