%{
#include "string.h"
#include <stdio.h>
#include <ctype.h>
#include <fcntl.h>
#define NUMFLAVOR 11
char    cndx[256];
char    *quote;
char    *svdx;
char    *camp="C&";
int     argd;
char    *argw[NUMFLAVOR];
FILE    *fp;
int     i,c;
int     incflag = 0;
int     inblock = 0;
int     selectf = 0;
int     ignore = 0;
%}
%%
^[ \t]+INnCLUDE[ \t]+'.+'[ \t]*$ {
                   if ( incflag ) {
                     quote = strchr(yytext,'\'');
                     if(quote != NULL)
                       strcpy(cndx, quote+1);
                       quote = strrchr(cndx,'\'');
                       if(quote != NULL)
                         *quote = '\0';
                     else
                       cndx[0] = '\0';
                     if ( (fp = fopen(cndx,"r")) == NULL ) {
                       fprintf(stderr,"\n%s%s\n","Error opening file :",cndx);
                       return;
                     }else{
                       while ( (c = getc(fp)) != EOF )  putchar(c);
                     }
                   }else{
                     printf("%s",yytext);
                   }
                   REJECT;
                 }
^C\&ELSEIF.*$   { if( !ignore ) {
                    if( selectf ) {
                      selectf = 0;
                      ignore = 1;
                    }
                    else {
                      for(i=0 ; i < argd ; i++ ) {
                        if(strstr(yytext,argw[i])!=NULL)  selectf = 1;
                      }
                      printf("%s",yytext);
                    }
                  }
                  printf("%s",yytext);
                }
^C\&ELSE.*$     { if( !ignore ) {
                    if( selectf ) {
                      selectf = 0;
                      ignore = 1;
                    }
                    else
                      selectf = 1;
                  }
                  printf("%s",yytext);
                }
^C\&ENDIF.*$    { inblock = 0;
                  selectf = 0;
                  ignore = 0;
                  printf("%s",yytext);
                }
^C\&IF.*$       { inblock = 1;
                  for(i=0 ; i < argd ; i++ ) {
                    if(strstr(yytext,argw[i])!=NULL)  selectf = 1;
                  }
                  printf("%s",yytext);
                }
^..             { if(!inblock) {
                    printf("%s",yytext);
                  }else{
                    if(selectf) {
                      if(strcasecmp(yytext,"C&") != 0) printf("%s",yytext);
                    }else{
                      if(strcasecmp(yytext,"C&") == 0) {
                        printf("%s",yytext);
                      }else{
                        printf("%s%s",camp,yytext);
                      }
                    }
                  }
                }
%%
main(argc,argv)
int     argc;
char    *argv[];
{
 
char    *param[NUMFLAVOR];
int     valid=0,k=0;
int     j;
 
  param[0]="VAXVMS";param[1]="VAXELN";param[2]="ETA10";param[3]="SIUNIX";
  param[4]="IBMAIX";param[5]="ULTRIX";param[6]="HPUX";param[7]="SUNOS";
  param[8]="ALFOSF";param[9]="LINUX";param[10]="INCLUDE";
  argd = argc;
  for ( i = 0; (i < argc) && (i < NUMFLAVOR+1); i++ ) {
    for ( j = 0; j < NUMFLAVOR; j++ ) {
      if (! strcmp(argv[i], param[j])) {
        argw[k++] = argv[i];
        valid = 1;
        if ( j == NUMFLAVOR-1 )  incflag = 1;
      }
    }
  }
  argd = k;
  if ( valid ) {
    yylex();
  }else{
    fprintf(stderr,"\n%s\n","No valid selection flag specified.");
    fprintf(stderr,"%s\n","Flag must be one of : VAXVMS, SIUNIX, ETA10, VAXELN, IBMAIX, ULTRIX, HPUX, SUNOS, ALFOSF, LINUX");
  }
}
