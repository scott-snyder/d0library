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

# line 2 "/d0library/scratch/unix/source/for_depend.lex"
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

# line 13 "/d0library/scratch/unix/source/for_depend.lex"
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
# define YYNEWLINE 10
yylex(void){
int nstr; extern int yyprevious;
while((nstr = yylook()) >= 0)
yyfussy: switch(nstr){
case 0:
if(yywrap()) return(0); break;
case 1:

# line 28 "/d0library/scratch/unix/source/for_depend.lex"
find_includes();
break;
case 2:

# line 29 "/d0library/scratch/unix/source/for_depend.lex"
find_includes();
break;
case 3:

# line 30 "/d0library/scratch/unix/source/for_depend.lex"
;
break;
case -1:
break;
default:
(void)fprintf(yyout,"bad switch yylook %d",nstr);
} return(0); }
/* end of yylex */

# line 32 "/d0library/scratch/unix/source/for_depend.lex"
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
int yyvstop[] = {
0,

3,
0,

1,
3,
0,

2,
3,
0,
0};
# define YYTYPE unsigned char
struct yywork { YYTYPE verify, advance; } yycrank[] = {
0,0,	0,0,	2,3,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	2,4,	2,5,	
0,0,	3,3,	0,0,	6,3,	
0,0,	8,10,	7,3,	0,0,	
0,0,	0,0,	9,3,	11,3,	
10,10,	12,3,	13,3,	14,3,	
0,0,	0,0,	0,0,	16,3,	
0,0,	15,3,	0,0,	17,3,	
0,0,	0,0,	18,3,	0,0,	
23,23,	20,3,	0,0,	0,0,	
6,8,	22,3,	0,0,	0,0,	
23,23,	23,25,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	19,19,	
2,6,	2,3,	2,3,	3,3,	
4,3,	6,3,	2,3,	8,3,	
7,3,	2,3,	4,7,	2,3,	
9,11,	11,3,	10,3,	12,3,	
13,3,	14,16,	2,3,	7,9,	
10,12,	16,3,	11,13,	15,3,	
15,17,	17,3,	12,14,	17,19,	
18,3,	19,21,	16,18,	20,3,	
20,22,	21,21,	13,15,	22,3,	
24,24,	22,24,	23,23,	23,23,	
23,23,	21,21,	21,5,	0,0,	
23,23,	0,0,	18,20,	23,23,	
0,0,	23,23,	0,0,	0,0,	
26,26,	0,0,	0,0,	0,0,	
23,23,	19,3,	0,0,	0,0,	
26,26,	0,0,	0,0,	0,0,	
0,0,	27,27,	24,26,	0,0,	
0,0,	0,0,	0,0,	21,23,	
0,0,	27,27,	27,28,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	26,27,	0,0,	
0,0,	0,0,	24,3,	0,0,	
0,0,	0,0,	0,0,	21,21,	
21,21,	21,21,	0,0,	0,0,	
0,0,	21,21,	0,0,	0,0,	
21,21,	0,0,	21,21,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	21,21,	26,26,	26,26,	
26,26,	0,0,	0,0,	0,0,	
26,26,	0,0,	0,0,	26,26,	
0,0,	26,26,	0,0,	27,27,	
27,27,	27,27,	0,0,	0,0,	
26,26,	27,27,	0,0,	0,0,	
27,27,	0,0,	27,27,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	27,27,	0,0,	0,0,	
0,0};
struct yysvf yysvec[] = {
0,	0,	0,
yycrank+0,	0,		0,	
yycrank+-1,	0,		0,	
yycrank+-4,	yysvec+2,	0,	
yycrank+-5,	yysvec+2,	0,	
yycrank+0,	0,		yyvstop+1,
yycrank+-6,	yysvec+2,	0,	
yycrank+-9,	yysvec+2,	0,	
yycrank+-8,	yysvec+2,	0,	
yycrank+-13,	yysvec+2,	0,	
yycrank+-15,	yysvec+2,	0,	
yycrank+-14,	yysvec+2,	0,	
yycrank+-16,	yysvec+2,	0,	
yycrank+-17,	yysvec+2,	0,	
yycrank+-18,	yysvec+2,	0,	
yycrank+-24,	yysvec+2,	0,	
yycrank+-22,	yysvec+2,	0,	
yycrank+-26,	yysvec+2,	0,	
yycrank+-29,	yysvec+2,	0,	
yycrank+-58,	yysvec+2,	0,	
yycrank+-32,	yysvec+2,	0,	
yycrank+-100,	0,		0,	
yycrank+-36,	yysvec+2,	0,	
yycrank+-39,	0,		0,	
yycrank+-95,	yysvec+2,	0,	
yycrank+0,	0,		yyvstop+3,
yycrank+-119,	yysvec+21,	0,	
yycrank+-132,	0,		0,	
yycrank+0,	0,		yyvstop+6,
0,	0,	0};
struct yywork *yytop = yycrank+217;
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
01  ,01  ,01  ,'C' ,'D' ,'E' ,01  ,01  ,
01  ,'I' ,01  ,01  ,'L' ,01  ,'N' ,01  ,
01  ,01  ,01  ,01  ,01  ,'U' ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,'C' ,'D' ,'E' ,01  ,01  ,
01  ,'I' ,01  ,01  ,'L' ,01  ,'N' ,01  ,
01  ,01  ,01  ,01  ,01  ,'U' ,01  ,01  ,
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
0,0,0,0,0,0,0,0,
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
