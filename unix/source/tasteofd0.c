# include "stdio.h"
# define U(x) ((unsigned char)(x))
# define NLSTATE yyprevious=YYNEWLINE
# define BEGIN yybgin = yysvec + 1 +
# define INITIAL 0
# define YYLERR yysvec
# define YYSTATE (yyestate-yysvec-1)
# define YYOPTIM 1
# define YYLMAX 2048
# define output(c) (void)putc(c,yyout)
#if defined(__cplusplus) || defined(__STDC__)

#ifdef __cplusplus
extern "C" {
#endif
	int yylex(void);
	int yyback(int *, int);
	int yyinput(void);
	int yylook(void);
	void yyoutput(int);
	int yyracc(int);
	int yyreject(void);
	void yyunput(int);
#ifndef yyless
	void yyless(long int);
#endif
#ifndef yywrap
	int yywrap(void);
#endif
#ifdef __cplusplus
}
#endif

#endif

# define input() (((yytchar=yysptr>yysbuf?U(*--yysptr):getc(yyin))==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
# define unput(c) {yytchar= (c);if(yytchar=='\n')yylineno--;*yysptr++=yytchar;}
# define yymore() (yymorfg=1)
# define ECHO (void)fprintf(yyout, "%s",yytext)
# define REJECT { nstr = yyreject(); goto yyfussy;}
int yyleng; extern char yytext[];
int yymorfg;
extern char *yysptr, yysbuf[];
int yytchar;
FILE *yyin = {stdin}, *yyout = {stdout};
extern int yylineno;
struct yysvf { 
	struct yywork *yystoff;
	struct yysvf *yyother;
	int *yystops;};
struct yysvf *yyestate;
extern struct yysvf yysvec[], *yybgin;

# line 2 "/d0library/scratch/unix/source/tasteofd0.lex"
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
# define YYNEWLINE 10
yylex(void){
int nstr; extern int yyprevious;
while((nstr = yylook()) >= 0)
yyfussy: switch(nstr){
case 0:
if(yywrap()) return(0); break;
case 1:

# line 21 "/d0library/scratch/unix/source/tasteofd0.lex"
{
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
break;
case 2:

# line 42 "/d0library/scratch/unix/source/tasteofd0.lex"
  { if( !ignore ) {
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
break;
case 3:

# line 56 "/d0library/scratch/unix/source/tasteofd0.lex"
    { if( !ignore ) {
                    if( selectf ) {
                      selectf = 0;
                      ignore = 1;
                    }
                    else
                      selectf = 1;
                  }
                  printf("%s",yytext);
                }
break;
case 4:

# line 66 "/d0library/scratch/unix/source/tasteofd0.lex"
   { inblock = 0;
                  selectf = 0;
                  ignore = 0;
                  printf("%s",yytext);
                }
break;
case 5:

# line 71 "/d0library/scratch/unix/source/tasteofd0.lex"
      { inblock = 1;
                  for(i=0 ; i < argd ; i++ ) {
                    if(strstr(yytext,argw[i])!=NULL)  selectf = 1;
                  }
                  printf("%s",yytext);
                }
break;
case 6:

# line 77 "/d0library/scratch/unix/source/tasteofd0.lex"
            { if(!inblock) {
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
break;
case -1:
break;
default:
(void)fprintf(yyout,"bad switch yylook %d",nstr);
} return(0); }
/* end of yylex */

# line 92 "/d0library/scratch/unix/source/tasteofd0.lex"
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
int yyvstop[] = {
0,

6,
0,

6,
0,

6,
0,

6,
0,

-5,
0,

5,
0,

-3,
0,

-3,
0,

3,
0,

-3,
0,

-4,
0,

-2,
-3,
0,

4,
0,

2,
3,
0,

-1,
0,

1,
0,
0};
# define YYTYPE unsigned char
struct yywork { YYTYPE verify, advance; } yycrank[] = {
0,0,	0,0,	2,3,	0,0,	
3,6,	0,0,	4,6,	0,0,	
0,0,	0,0,	2,4,	2,0,	
3,6,	3,0,	4,7,	4,0,	
5,0,	7,10,	18,18,	0,0,	
24,27,	38,0,	27,27,	34,36,	
30,30,	0,0,	18,18,	18,22,	
24,27,	24,28,	27,27,	27,28,	
30,30,	30,33,	32,32,	0,0,	
0,0,	0,0,	0,0,	37,38,	
7,10,	36,37,	32,32,	32,35,	
5,9,	0,0,	34,36,	37,38,	
37,0,	0,0,	38,39,	39,38,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	39,39,	
39,40,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
2,5,	15,19,	9,13,	14,18,	
17,21,	20,24,	9,14,	21,25,	
25,30,	19,23,	4,8,	8,12,	
13,16,	7,11,	13,17,	26,31,	
16,20,	29,32,	23,26,	31,34,	
0,0,	39,39,	0,0,	0,0,	
24,29,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	12,15,	
0,0};
struct yysvf yysvec[] = {
0,	0,	0,
yycrank+0,	0,		0,	
yycrank+-1,	0,		0,	
yycrank+-3,	0,		0,	
yycrank+-5,	0,		0,	
yycrank+-6,	yysvec+3,	0,	
yycrank+0,	0,		yyvstop+1,
yycrank+8,	0,		yyvstop+3,
yycrank+1,	0,		yyvstop+5,
yycrank+1,	0,		yyvstop+7,
yycrank+0,	yysvec+7,	0,	
yycrank+0,	yysvec+8,	0,	
yycrank+1,	0,		0,	
yycrank+4,	0,		0,	
yycrank+1,	0,		0,	
yycrank+2,	0,		0,	
yycrank+1,	0,		0,	
yycrank+4,	0,		0,	
yycrank+-17,	0,		yyvstop+9,
yycrank+1,	0,		0,	
yycrank+4,	0,		0,	
yycrank+2,	0,		0,	
yycrank+0,	0,		yyvstop+11,
yycrank+1,	0,		0,	
yycrank+-19,	0,		yyvstop+13,
yycrank+6,	0,		0,	
yycrank+15,	0,		0,	
yycrank+-21,	0,		yyvstop+15,
yycrank+0,	0,		yyvstop+17,
yycrank+-15,	yysvec+27,	yyvstop+19,
yycrank+-23,	0,		yyvstop+21,
yycrank+18,	0,		0,	
yycrank+-33,	0,		yyvstop+23,
yycrank+0,	0,		yyvstop+26,
yycrank+14,	0,		0,	
yycrank+0,	0,		yyvstop+28,
yycrank+2,	yysvec+34,	0,	
yycrank+-38,	0,		0,	
yycrank+-11,	yysvec+37,	0,	
yycrank+-50,	0,		yyvstop+31,
yycrank+0,	0,		yyvstop+33,
0,	0,	0};
struct yywork *yytop = yycrank+111;
struct yysvf *yybgin = yysvec+1;
unsigned char yymatch[] = {
00  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,011 ,012 ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
011 ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
0};
char yyextra[] = {
0,1,1,1,1,1,0,0,
0};
#ident	"$Revision$"

int yylineno =1;
# define YYU(x) x
# define NLSTATE yyprevious=YYNEWLINE
char yytext[YYLMAX];
struct yysvf *yylstate [YYLMAX], **yylsp, **yyolsp;
char yysbuf[YYLMAX];
char *yysptr = yysbuf;
int *yyfnd;
extern struct yysvf *yyestate;
int yyprevious = YYNEWLINE;
#if defined(__cplusplus) || defined(__STDC__)
int yylook(void)
#else
yylook()
#endif
{
	register struct yysvf *yystate, **lsp;
	register struct yywork *yyt;
	struct yysvf *yyz;
	int yych, yyfirst;
	struct yywork *yyr;
# ifdef LEXDEBUG
	int debug;
# endif
	char *yylastch;
	/* start off machines */
# ifdef LEXDEBUG
	debug = 0;
# endif
	yyfirst=1;
	if (!yymorfg)
		yylastch = yytext;
	else {
		yymorfg=0;
		yylastch = yytext+yyleng;
		}
	for(;;){
		lsp = yylstate;
		yyestate = yystate = yybgin;
		if (yyprevious==YYNEWLINE) yystate++;
		for (;;){
# ifdef LEXDEBUG
			if(debug)fprintf(yyout,"state %d\n",yystate-yysvec-1);
# endif
			yyt = yystate->yystoff;
			if(yyt == yycrank && !yyfirst){  /* may not be any transitions */
				yyz = yystate->yyother;
				if(yyz == 0)break;
				if(yyz->yystoff == yycrank)break;
				}
			*yylastch++ = yych = input();
#ifndef LONGLINES
			if(yylastch > &yytext[YYLMAX]) {
				fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
				exit(1);
			}
#endif
			yyfirst=0;
		tryagain:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"char ");
				allprint(yych);
				putchar('\n');
				}
# endif
			yyr = yyt;
			if ( (void *)yyt > (void *)yycrank){
				yyt = yyr + yych;
				if (yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
#ifndef LONGLINES
					if(lsp > &yylstate[YYLMAX]) {
						fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
						exit(1);
					}
#endif
					goto contin;
					}
				}
# ifdef YYOPTIM
			else if((void *)yyt < (void *)yycrank) {	/* r < yycrank */
				yyt = yyr = yycrank+(yycrank-yyt);
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"compressed state\n");
# endif
				yyt = yyt + yych;
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
#ifndef LONGLINES
					if(lsp > &yylstate[YYLMAX]) {
						fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
						exit(1);
					}
#endif
					goto contin;
					}
				yyt = yyr + YYU(yymatch[yych]);
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"try fall back character ");
					allprint(YYU(yymatch[yych]));
					putchar('\n');
					}
# endif
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transition */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
#ifndef LONGLINES
					if(lsp > &yylstate[YYLMAX]) {
						fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
						exit(1);
					}
#endif
					goto contin;
					}
				}
			if ((yystate = yystate->yyother) && (yyt= yystate->yystoff) != yycrank){
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"fall back to state %d\n",yystate-yysvec-1);
# endif
				goto tryagain;
				}
# endif
			else
				{unput(*--yylastch);break;}
		contin:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"state %d char ",yystate-yysvec-1);
				allprint(yych);
				putchar('\n');
				}
# endif
			;
			}
# ifdef LEXDEBUG
		if(debug){
			fprintf(yyout,"stopped at %d with ",*(lsp-1)-yysvec-1);
			allprint(yych);
			putchar('\n');
			}
# endif
		while (lsp-- > yylstate){
			*yylastch-- = 0;
			if (*lsp != 0 && (yyfnd= (*lsp)->yystops) && *yyfnd > 0){
				yyolsp = lsp;
				if(yyextra[*yyfnd]){		/* must backup */
					while(yyback((*lsp)->yystops,-*yyfnd) != 1 && lsp > yylstate){
						lsp--;
						unput(*yylastch--);
						}
					}
				yyprevious = YYU(*yylastch);
				yylsp = lsp;
				yyleng = (int)(yylastch-yytext+1);
				yytext[yyleng] = 0;
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"\nmatch ");
					sprint(yytext);
					fprintf(yyout," action %d\n",*yyfnd);
					}
# endif
				return(*yyfnd++);
				}
			unput(*yylastch);
			}
		if (yytext[0] == 0  /* && feof(yyin) */)
			{
			yysptr=yysbuf;
			return(0);
			}
		yyprevious = yytext[0] = input();
		if (yyprevious>0)
			output(yyprevious);
		yylastch=yytext;
# ifdef LEXDEBUG
		if(debug)putchar('\n');
# endif
		}
	}
#if defined(__cplusplus) || defined(__STDC__)
int yyback(int *p, int m)
#else
yyback(p, m)
	int *p;
#endif
{
	if (p==0) return(0);
	while (*p) {
		if (*p++ == m)
			return(1);
	}
	return(0);
}
	/* the following are only used in the lex library */
#if defined(__cplusplus) || defined(__STDC__)
int yyinput(void)
#else
yyinput()
#endif
{
	return(input());
	}
#if defined(__cplusplus) || defined(__STDC__)
void yyoutput(int c)
#else
yyoutput(c)
  int c; 
#endif
{
	output(c);
	}
#if defined(__cplusplus) || defined(__STDC__)
void yyunput(int c)
#else
yyunput(c)
   int c; 
#endif
{
	unput(c);
	}
