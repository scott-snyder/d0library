%{
#include "unix.h"
#include <stdlib.h>
#include <string.h>
char    cndx[256];
char    outbuf[256];
char    *exclam;
char	*comma;
char    *svdx;
char    *d0log;
char    tmp[40];
char    *parens ="()";
char    *incroot;
context_t *context;
int     flag = 0;
int     n1, n2, i;
char    *s;
%}
D                       [a-zA-Z0-9_\$]+[a-zA-Z0-9_\-\$]*
S                       [ \t]+
s                       [ \t]*
%a 4000
%o 8000
%p 5000
%Start mbl
%%
^{S}[Ii][Nn][Cc][Ll][Uu][Dd][Ee]{s}'.*'.*$ {
 /*
   Change the VAX filename arguments in INCLUDES to their UNIX equivalent
   using lib_find_file.  Any trailing VMS switches like '/list' are
   removed.  The entire string is split at column 14 or 72 if it exceeds 72
   characters.  If the resulting INCLUDE is longer than 144 characters,
   you're on your own.
 */
                          if (strlen (yytext) > sizeof(outbuf) - 30) abort ();
                          strcpy (outbuf, yytext);

                          d0log = strchr(outbuf,'\'') + 1;
                          if(d0log != NULL)
                            strcpy(cndx, d0log);
                          else
                            cndx[0] = '\0';
                          svdx = strchr(cndx,'/');
                          if(svdx != NULL)
                            *svdx = '\0';
                          svdx = strchr(cndx,'\'');
                          if(svdx != NULL)
                            *svdx = '\0';
                          context = NULL;
                          lib_find_file(cndx, d0log, &context);
                          find_file_end(&context);
                          if(*d0log == '\0')
                            strcpy(d0log, cndx);
                          strcat(outbuf,"\'");
 
                        /* Check for initial tab */
 
                          if(outbuf[0]=='\t') {
                            strcpy(cndx, outbuf);
                            strcpy(outbuf, "      ");
                            strcat(outbuf, &cndx[1]);
                          }
                          if(strlen(outbuf)<=72)
                            printf(outbuf) ;
                          else if(strlen(outbuf)<=80) {
                            strcpy(cndx, outbuf);
                            cndx[14] = '\0';
                            printf("%s\n     &%s",cndx, &outbuf[14]);
                          }
                          else {
                            strcpy(cndx, outbuf);
                            cndx[72] = '\0';
                            printf("%s\n     &%s",cndx, &outbuf[72]);
                          }
                        }
^.*!.*$ {
 /* Kill endline comments.  We try to avoid messing up strings with quoted
    exclamation marks in the process. */
 
        n1 = 0;
        n2 = 0;
        s = yytext;
        exclam = strchr(yytext, '!');
        while(*s != '\0') {
          if(*s == '\'') {
            if(s < exclam)
              ++n1;
            else
              ++n2;
          }
          ++s;
        }
        if((n1 % 2) == 0 && n2 == 0)
          *exclam = '\0';
        if(exclam == yytext)
          *yytext = 'C';
        printf("%s",yytext);}
^{S}[\*A-Z0-9]*{S}FUNCTION{S}[A-Z_0-9]+{s}$ {
 /* Patch up function definitions that have no argument list. Function
    definitions of the form:
                         LOGICAL FUNCTION LFUNC
    are replaced with:
                         LOGICAL FUNCTION LFUNC()
 */
                        if (strlen (yytext) > sizeof(outbuf) - 30) abort ();
                        strcpy (outbuf, yytext);

                        while(outbuf[yyleng-1] == ' ') {
                          outbuf[yyleng-1] = '\0';
                          yyleng--;}
                        strcat(outbuf,parens);
                        printf("%s",outbuf);}
^{S}[Cc][Aa][Ll][Ll]{S}[Ee][Zz][Pp][Ii][Cc][Kk]{s}\([^()]*\,[^()]*\).*$ {
 
/* Fix up EZPICK calls with two or more arguments.  */
 
			comma = strchr(yytext, ',');
			if(comma != NULL)
			  strcpy(comma, ")");
			printf("%s",yytext);
}
