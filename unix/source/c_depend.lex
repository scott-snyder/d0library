 /****************************************************************************
  *
  * Name:    c_depend
  *
  * Purpose: Generate dependencies for c.
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
#include <ctype.h>
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
^{s}#{s}include{s}["<].*[">].*\n find_includes();
^.*\n ;
%%
void find_includes()
 {
 
 /* Extract the UNIX filename from #include preprocessor statements. */
 
   fch = strchr(yytext, '<');
   if(fch == NULL)
     fch = strchr(yytext, '"');
   ++fch;
   lch = strchr(fch, '>');
   if(lch == NULL)
     lch = strchr(fch, '"');
   nch = lch - fch;
   while(isspace(*fch))
     ++fch;
   if(*fch == '/')
     *ifile = '\0';
   else
     strcpy(ifile, "$d0unix/source/");
   strncat(ifile, fch, nch);
   strcat(ifile, "");
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
