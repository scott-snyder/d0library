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

# line 2 "/d0library/scratch/unix/source/vmstounix.lex"
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
# define mbl 2
# define YYNEWLINE 10
yylex(void){
int nstr; extern int yyprevious;
while((nstr = yylook()) >= 0)
yyfussy: switch(nstr){
case 0:
if(yywrap()) return(0); break;
case 1:

# line 27 "/d0library/scratch/unix/source/vmstounix.lex"
{
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
break;
case 2:

# line 76 "/d0library/scratch/unix/source/vmstounix.lex"
{
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
break;
case 3:

# line 98 "/d0library/scratch/unix/source/vmstounix.lex"
{
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
break;
case 4:

# line 113 "/d0library/scratch/unix/source/vmstounix.lex"
{
 
/* Fix up EZPICK calls with two or more arguments.  */
 
			comma = strchr(yytext, ',');
			if(comma != NULL)
			  strcpy(comma, ")");
			printf("%s",yytext);
}
break;
case -1:
break;
default:
(void)fprintf(yyout,"bad switch yylook %d",nstr);
} return(0); }
/* end of yylex */
int yyvstop[] = {
0,

-2,
0,

2,
0,

-2,
0,

-1,
0,

-1,
-2,
0,

1,
0,

-3,
0,

-3,
0,

1,
2,
0,

-3,
0,

3,
0,

-3,
0,

-3,
0,

-2,
0,

-3,
0,

2,
0,

-2,
0,

-4,
0,

-3,
0,

-4,
0,

2,
0,

-2,
-4,
0,

4,
0,

-3,
0,

2,
4,
0,

-3,
0,

-3,
0,
0};
# define YYTYPE unsigned char
struct yywork { YYTYPE verify, advance; } yycrank[] = {
0,0,	0,0,	2,5,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	2,6,	2,0,	
4,0,	5,5,	5,0,	0,0,	
13,5,	13,0,	22,5,	22,0,	
0,0,	16,16,	16,0,	0,0,	
12,5,	12,0,	18,5,	18,0,	
0,0,	20,5,	20,0,	0,0,	
30,36,	30,0,	2,7,	4,7,	
0,0,	5,7,	60,66,	60,0,	
13,7,	2,5,	22,7,	2,5,	
0,0,	16,7,	84,0,	84,83,	
12,7,	2,5,	18,7,	28,5,	
28,0,	20,7,	24,5,	24,0,	
30,7,	79,0,	79,0,	26,5,	
26,0,	79,78,	60,7,	0,0,	
34,5,	34,0,	2,5,	0,0,	
2,5,	2,5,	2,5,	6,8,	
6,0,	0,0,	2,5,	28,7,	
2,5,	2,5,	24,7,	2,5,	
12,18,	2,5,	16,22,	26,7,	
0,0,	13,20,	2,5,	20,26,	
34,7,	0,0,	0,0,	2,5,	
0,0,	18,24,	22,28,	6,7,	
2,5,	0,0,	2,5,	0,0,	
2,5,	2,5,	2,5,	0,0,	
6,9,	0,0,	2,5,	7,7,	
2,5,	2,5,	6,9,	2,5,	
12,18,	2,5,	0,0,	7,7,	
7,14,	13,20,	2,5,	20,26,	
28,34,	24,30,	34,40,	2,5,	
0,0,	18,24,	26,32,	6,9,	
0,0,	6,10,	6,9,	6,9,	
35,35,	35,0,	0,0,	6,11,	
0,0,	6,9,	6,9,	0,0,	
6,9,	0,0,	6,9,	0,0,	
45,5,	45,0,	7,7,	6,9,	
7,7,	0,0,	0,0,	0,0,	
6,9,	24,30,	7,7,	0,0,	
35,7,	0,0,	26,32,	36,36,	
36,0,	6,12,	0,0,	32,5,	
32,0,	0,0,	0,0,	6,13,	
45,7,	0,0,	0,0,	7,7,	
0,0,	7,7,	7,7,	7,7,	
8,8,	8,0,	0,0,	7,7,	
0,0,	7,7,	7,7,	36,7,	
7,7,	0,0,	7,7,	32,7,	
40,5,	40,0,	0,0,	7,7,	
35,41,	35,22,	0,0,	0,0,	
7,7,	0,0,	43,5,	43,0,	
8,7,	7,7,	0,0,	7,7,	
0,0,	7,7,	7,7,	7,7,	
45,50,	8,9,	0,0,	7,7,	
40,7,	7,7,	7,7,	8,9,	
7,7,	0,0,	7,7,	36,41,	
9,16,	9,0,	43,7,	7,7,	
35,41,	0,0,	0,0,	0,0,	
7,7,	0,0,	41,5,	41,0,	
8,9,	0,0,	8,10,	8,9,	
8,9,	8,15,	0,0,	32,38,	
8,11,	0,0,	8,9,	8,9,	
9,7,	8,9,	0,0,	8,9,	
0,0,	10,16,	10,0,	36,41,	
8,9,	9,9,	41,7,	0,0,	
0,0,	8,9,	43,48,	9,9,	
0,0,	50,5,	50,0,	40,45,	
0,0,	0,0,	8,12,	0,0,	
46,5,	46,0,	0,0,	32,38,	
8,13,	10,7,	38,5,	38,0,	
9,9,	0,0,	9,9,	9,9,	
9,9,	0,0,	10,9,	0,0,	
9,9,	50,7,	9,9,	9,9,	
10,9,	9,9,	43,48,	9,9,	
46,7,	11,16,	11,0,	0,0,	
9,9,	0,0,	38,7,	0,0,	
0,0,	9,9,	0,0,	55,5,	
55,0,	10,17,	0,0,	10,9,	
10,9,	10,9,	0,0,	41,46,	
0,0,	10,9,	0,0,	10,9,	
10,9,	11,7,	10,9,	0,0,	
10,9,	0,0,	15,16,	15,0,	
0,0,	10,9,	11,9,	55,7,	
0,0,	0,0,	10,9,	50,55,	
11,9,	38,43,	69,69,	69,70,	
0,0,	10,18,	0,0,	46,51,	
0,0,	0,0,	0,0,	41,46,	
0,0,	0,0,	15,7,	0,0,	
0,0,	11,9,	0,0,	11,9,	
11,9,	11,9,	0,0,	15,9,	
0,0,	11,9,	69,7,	11,9,	
11,9,	15,9,	11,19,	0,0,	
11,9,	38,43,	17,16,	17,0,	
0,0,	11,9,	0,0,	46,51,	
55,60,	0,0,	11,9,	0,0,	
48,48,	48,0,	15,9,	0,0,	
15,9,	15,9,	15,9,	0,0,	
51,5,	51,0,	15,9,	0,0,	
15,9,	15,9,	17,7,	15,9,	
0,0,	15,9,	11,20,	19,16,	
19,0,	0,0,	15,21,	17,9,	
48,7,	0,0,	0,0,	15,9,	
0,0,	17,9,	48,53,	0,0,	
51,7,	0,0,	0,0,	0,0,	
56,5,	56,0,	52,52,	52,0,	
0,0,	0,0,	0,0,	19,7,	
0,0,	0,0,	17,9,	0,0,	
17,9,	17,9,	17,9,	0,0,	
19,9,	0,0,	17,9,	0,0,	
17,9,	17,23,	19,9,	17,9,	
56,7,	17,9,	52,7,	21,16,	
21,0,	0,0,	17,9,	0,0,	
52,53,	0,0,	0,0,	17,9,	
51,56,	0,0,	0,0,	19,9,	
0,0,	19,25,	19,9,	19,9,	
0,0,	61,5,	61,0,	19,9,	
0,0,	19,9,	19,9,	21,7,	
19,9,	17,24,	19,9,	0,0,	
23,16,	23,0,	56,61,	19,9,	
21,9,	0,0,	0,0,	52,22,	
19,9,	0,0,	21,9,	0,0,	
51,56,	61,7,	0,0,	0,0,	
0,0,	19,26,	0,0,	67,67,	
67,0,	0,0,	0,0,	0,0,	
23,7,	0,0,	0,0,	21,9,	
0,0,	21,9,	21,9,	21,9,	
0,0,	23,9,	56,61,	21,9,	
0,0,	21,9,	21,9,	23,9,	
21,27,	0,0,	21,9,	67,7,	
25,16,	25,0,	0,0,	21,9,	
0,0,	0,0,	67,72,	67,5,	
21,9,	0,0,	0,0,	61,67,	
23,9,	0,0,	23,9,	23,9,	
23,9,	0,0,	0,0,	0,0,	
23,9,	0,0,	23,9,	23,29,	
25,7,	23,9,	0,0,	23,9,	
0,0,	27,16,	27,0,	0,0,	
23,9,	25,9,	0,0,	0,0,	
0,0,	23,9,	0,0,	25,9,	
0,0,	0,0,	0,0,	61,67,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	27,7,	0,0,	23,30,	
25,9,	0,0,	25,9,	25,9,	
25,9,	0,0,	27,9,	0,0,	
25,9,	0,0,	25,9,	25,31,	
27,9,	25,9,	0,0,	25,9,	
0,0,	29,35,	29,0,	0,0,	
25,9,	0,0,	0,0,	0,0,	
0,0,	25,9,	0,0,	0,0,	
0,0,	27,9,	0,0,	27,33,	
27,9,	27,9,	0,0,	0,0,	
0,0,	27,9,	0,0,	27,9,	
27,9,	29,7,	27,9,	25,32,	
27,9,	0,0,	31,16,	31,0,	
0,0,	27,9,	29,9,	0,0,	
0,0,	0,0,	27,9,	0,0,	
29,9,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	31,7,	0,0,	
0,0,	29,9,	0,0,	29,9,	
29,9,	29,9,	0,0,	31,9,	
0,0,	29,9,	0,0,	29,9,	
29,9,	31,9,	29,9,	0,0,	
29,9,	0,0,	0,0,	0,0,	
0,0,	29,9,	0,0,	0,0,	
0,0,	0,0,	29,9,	33,16,	
33,0,	0,0,	31,9,	0,0,	
31,9,	31,9,	31,9,	0,0,	
0,0,	0,0,	31,9,	0,0,	
31,9,	31,9,	0,0,	31,9,	
0,0,	31,9,	0,0,	0,0,	
0,0,	0,0,	31,37,	33,7,	
0,0,	0,0,	0,0,	31,9,	
37,16,	37,0,	0,0,	0,0,	
33,9,	0,0,	0,0,	0,0,	
0,0,	0,0,	33,9,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
37,7,	0,0,	31,38,	33,9,	
0,0,	33,9,	33,9,	33,9,	
0,0,	37,9,	0,0,	33,9,	
0,0,	33,9,	33,9,	37,9,	
33,9,	0,0,	33,9,	0,0,	
39,16,	39,0,	33,39,	33,9,	
0,0,	0,0,	0,0,	0,0,	
33,9,	0,0,	0,0,	0,0,	
37,9,	0,0,	37,9,	37,42,	
37,9,	0,0,	0,0,	0,0,	
37,9,	0,0,	37,9,	37,9,	
39,7,	37,9,	0,0,	37,9,	
0,0,	42,16,	42,0,	0,0,	
37,9,	39,9,	0,0,	0,0,	
0,0,	37,9,	0,0,	39,9,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	37,43,	
0,0,	0,0,	0,0,	0,0,	
0,0,	42,7,	0,0,	0,0,	
39,9,	0,0,	39,9,	39,9,	
39,9,	0,0,	42,9,	0,0,	
39,44,	0,0,	39,9,	39,9,	
42,9,	39,9,	0,0,	39,9,	
0,0,	44,16,	44,0,	0,0,	
39,9,	0,0,	0,0,	0,0,	
0,0,	39,9,	0,0,	0,0,	
0,0,	42,9,	0,0,	42,9,	
42,9,	42,47,	0,0,	0,0,	
0,0,	42,9,	0,0,	42,9,	
42,9,	44,7,	42,9,	0,0,	
42,9,	0,0,	0,0,	0,0,	
0,0,	42,9,	44,9,	0,0,	
0,0,	0,0,	42,9,	0,0,	
44,9,	0,0,	0,0,	0,0,	
0,0,	47,52,	47,0,	0,0,	
0,0,	42,48,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	44,9,	0,0,	44,9,	
44,9,	44,9,	0,0,	0,0,	
0,0,	44,9,	0,0,	44,9,	
44,9,	47,7,	44,9,	44,49,	
44,9,	0,0,	0,0,	47,53,	
0,0,	44,9,	47,9,	0,0,	
0,0,	0,0,	44,9,	0,0,	
47,9,	0,0,	49,16,	49,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	47,9,	0,0,	47,9,	
47,9,	47,9,	0,0,	0,0,	
0,0,	47,9,	49,7,	47,9,	
47,9,	0,0,	47,9,	0,0,	
47,9,	0,0,	0,0,	49,9,	
0,0,	47,9,	53,53,	0,0,	
0,0,	49,9,	47,9,	0,0,	
0,0,	0,0,	53,53,	53,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	49,9,	0,0,	
49,9,	49,9,	49,9,	0,0,	
0,0,	0,0,	49,9,	0,0,	
49,9,	49,9,	53,57,	49,54,	
0,0,	49,9,	0,0,	0,0,	
53,58,	53,53,	49,9,	53,53,	
0,0,	0,0,	0,0,	49,9,	
0,0,	53,53,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	53,53,	0,0,	
53,53,	53,53,	53,53,	54,59,	
54,0,	0,0,	53,53,	0,0,	
53,53,	53,53,	0,0,	53,53,	
0,0,	53,53,	0,0,	0,0,	
0,0,	0,0,	53,53,	0,0,	
0,0,	0,0,	0,0,	53,53,	
0,0,	0,0,	0,0,	54,7,	
53,53,	0,0,	53,53,	0,0,	
53,53,	53,53,	53,53,	0,0,	
54,9,	0,0,	53,53,	57,57,	
53,53,	53,53,	54,9,	53,53,	
0,0,	53,53,	0,0,	57,57,	
57,14,	0,0,	53,53,	0,0,	
0,0,	0,0,	0,0,	53,53,	
0,0,	0,0,	0,0,	54,9,	
0,0,	54,9,	54,9,	54,9,	
0,0,	0,0,	0,0,	54,9,	
0,0,	54,9,	54,9,	0,0,	
54,9,	0,0,	54,9,	0,0,	
0,0,	57,62,	57,57,	54,9,	
57,57,	0,0,	0,0,	0,0,	
54,9,	0,0,	57,57,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	57,57,	
0,0,	57,57,	57,57,	57,57,	
0,0,	0,0,	0,0,	57,57,	
58,58,	57,57,	57,57,	0,0,	
57,57,	0,0,	57,57,	0,0,	
58,58,	58,63,	0,0,	57,57,	
0,0,	0,0,	0,0,	0,0,	
57,57,	0,0,	0,0,	0,0,	
0,0,	57,57,	0,0,	57,57,	
0,0,	57,57,	57,57,	57,57,	
0,0,	0,0,	0,0,	57,57,	
58,62,	57,57,	57,57,	0,0,	
57,57,	0,0,	57,57,	58,58,	
0,0,	58,58,	0,0,	57,57,	
0,0,	0,0,	0,0,	58,58,	
57,57,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
58,58,	0,0,	58,58,	58,58,	
58,58,	59,59,	59,0,	0,0,	
58,58,	0,0,	58,58,	58,58,	
0,0,	58,58,	0,0,	58,58,	
0,0,	0,0,	0,0,	0,0,	
58,58,	0,0,	0,0,	0,0,	
0,0,	58,58,	0,0,	0,0,	
0,0,	59,7,	58,58,	0,0,	
58,58,	0,0,	58,58,	58,58,	
58,58,	0,0,	0,0,	0,0,	
58,58,	62,62,	58,58,	58,58,	
59,64,	58,58,	0,0,	58,58,	
0,0,	62,62,	62,68,	0,0,	
58,58,	0,0,	0,0,	0,0,	
0,0,	58,58,	0,0,	0,0,	
0,0,	59,64,	0,0,	59,64,	
59,64,	59,64,	59,65,	0,0,	
0,0,	59,64,	0,0,	59,64,	
59,64,	0,0,	59,64,	0,0,	
59,64,	0,0,	0,0,	0,0,	
62,62,	59,64,	62,62,	0,0,	
0,0,	0,0,	59,64,	0,0,	
62,62,	0,0,	0,0,	59,64,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	62,62,	0,0,	62,62,	
62,62,	62,62,	64,69,	64,70,	
0,0,	62,62,	0,0,	62,62,	
62,62,	0,0,	62,62,	0,0,	
62,62,	0,0,	0,0,	0,0,	
0,0,	62,62,	0,0,	0,0,	
0,0,	0,0,	62,62,	0,0,	
0,0,	0,0,	64,7,	62,62,	
0,0,	62,62,	0,0,	62,62,	
62,62,	62,62,	0,0,	65,69,	
65,70,	62,62,	0,0,	62,62,	
62,62,	64,64,	62,62,	0,0,	
62,62,	0,0,	0,0,	0,0,	
0,0,	62,62,	0,0,	0,0,	
0,0,	0,0,	62,62,	0,0,	
0,0,	0,0,	64,64,	65,7,	
64,64,	64,64,	64,64,	0,0,	
0,0,	0,0,	64,64,	0,0,	
64,64,	64,64,	0,0,	64,64,	
0,0,	64,64,	65,64,	0,0,	
66,66,	66,0,	64,64,	0,0,	
0,0,	0,0,	0,0,	64,64,	
0,0,	0,0,	0,0,	0,0,	
64,64,	0,0,	0,0,	65,64,	
0,0,	65,64,	65,64,	65,64,	
0,0,	0,0,	0,0,	65,64,	
66,7,	65,64,	65,64,	0,0,	
65,64,	0,0,	65,64,	71,69,	
71,70,	0,0,	0,0,	65,71,	
0,0,	0,0,	0,0,	66,64,	
65,64,	0,0,	0,0,	0,0,	
0,0,	65,64,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	71,7,	
66,64,	0,0,	66,64,	66,64,	
66,64,	0,0,	0,0,	0,0,	
66,64,	0,0,	66,64,	66,64,	
0,0,	66,64,	71,64,	66,64,	
0,0,	0,0,	0,0,	0,0,	
66,64,	0,0,	0,0,	0,0,	
0,0,	66,64,	0,0,	0,0,	
72,72,	0,0,	66,64,	71,64,	
0,0,	71,64,	71,64,	71,64,	
72,72,	72,74,	0,0,	71,64,	
0,0,	71,64,	71,64,	0,0,	
71,73,	0,0,	71,64,	0,0,	
0,0,	0,0,	0,0,	71,64,	
0,0,	0,0,	0,0,	0,0,	
71,64,	0,0,	0,0,	0,0,	
72,75,	71,64,	0,0,	0,0,	
0,0,	0,0,	0,0,	72,5,	
0,0,	72,72,	0,0,	72,76,	
0,0,	0,0,	0,0,	72,72,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
72,72,	0,0,	72,72,	72,72,	
72,72,	73,69,	73,70,	0,0,	
72,72,	0,0,	72,72,	72,72,	
0,0,	72,72,	0,0,	72,72,	
0,0,	0,0,	0,0,	0,0,	
72,72,	0,0,	0,0,	0,0,	
0,0,	72,72,	0,0,	0,0,	
0,0,	73,7,	72,72,	0,0,	
72,72,	0,0,	72,72,	72,72,	
72,72,	0,0,	0,0,	0,0,	
72,72,	0,0,	72,72,	72,72,	
73,64,	72,72,	0,0,	72,72,	
0,0,	0,0,	0,0,	0,0,	
72,72,	0,0,	0,0,	0,0,	
0,0,	72,72,	74,74,	0,0,	
0,0,	73,64,	0,0,	73,77,	
73,64,	73,64,	74,74,	74,74,	
0,0,	73,64,	0,0,	73,64,	
73,64,	0,0,	73,64,	0,0,	
73,64,	0,0,	0,0,	0,0,	
0,0,	73,64,	0,0,	0,0,	
0,0,	0,0,	73,64,	0,0,	
0,0,	0,0,	0,0,	73,64,	
0,0,	0,0,	0,0,	0,0,	
0,0,	74,0,	74,0,	74,74,	
0,0,	74,78,	0,0,	0,0,	
0,0,	74,74,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	74,74,	0,0,	
74,74,	74,74,	74,74,	0,0,	
0,0,	0,0,	74,74,	0,0,	
74,74,	74,74,	0,0,	74,74,	
0,0,	74,74,	0,0,	0,0,	
0,0,	75,75,	74,74,	0,0,	
0,0,	0,0,	0,0,	74,74,	
0,0,	75,75,	75,79,	0,0,	
74,74,	0,0,	74,74,	0,0,	
74,74,	74,74,	74,74,	0,0,	
0,0,	0,0,	74,74,	0,0,	
74,74,	74,74,	0,0,	74,74,	
0,0,	74,74,	0,0,	0,0,	
0,0,	0,0,	74,74,	0,0,	
0,0,	0,0,	0,0,	74,74,	
75,7,	0,0,	75,75,	0,0,	
75,80,	0,0,	0,0,	0,0,	
75,75,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	75,75,	0,0,	75,75,	
75,75,	75,75,	0,0,	0,0,	
0,0,	75,75,	76,76,	75,75,	
75,75,	0,0,	75,75,	0,0,	
75,75,	0,0,	76,76,	76,78,	
0,0,	75,75,	0,0,	0,0,	
0,0,	0,0,	75,75,	0,0,	
0,0,	0,0,	0,0,	75,75,	
0,0,	75,75,	0,0,	75,75,	
75,75,	75,75,	0,0,	0,0,	
0,0,	75,75,	76,80,	75,75,	
75,75,	0,0,	75,75,	0,0,	
75,75,	76,5,	76,81,	76,76,	
0,0,	75,75,	0,0,	0,0,	
0,0,	76,76,	75,75,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	76,76,	0,0,	
76,76,	76,76,	76,76,	77,69,	
77,70,	0,0,	76,76,	0,0,	
76,76,	76,76,	0,0,	76,76,	
0,0,	76,76,	0,0,	0,0,	
0,0,	0,0,	76,76,	0,0,	
0,0,	0,0,	0,0,	76,76,	
0,0,	0,0,	0,0,	77,7,	
76,76,	0,0,	76,76,	0,0,	
76,76,	76,76,	76,76,	0,0,	
0,0,	0,0,	76,76,	0,0,	
76,76,	76,76,	77,64,	76,76,	
0,0,	76,76,	0,0,	0,0,	
0,0,	0,0,	76,76,	0,0,	
0,0,	0,0,	0,0,	76,76,	
78,78,	0,0,	0,0,	77,64,	
0,0,	77,64,	77,64,	77,64,	
78,78,	78,78,	0,0,	77,64,	
0,0,	77,64,	77,64,	0,0,	
77,64,	0,0,	77,64,	0,0,	
0,0,	0,0,	77,82,	77,64,	
0,0,	0,0,	0,0,	0,0,	
77,64,	0,0,	0,0,	0,0,	
0,0,	77,64,	0,0,	0,0,	
0,0,	0,0,	0,0,	78,0,	
78,83,	78,78,	0,0,	0,0,	
0,0,	0,0,	0,0,	78,78,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
78,78,	0,0,	78,78,	78,78,	
78,78,	0,0,	0,0,	0,0,	
78,78,	80,80,	78,78,	78,78,	
0,0,	78,78,	0,0,	78,78,	
0,0,	80,80,	80,84,	0,0,	
78,78,	0,0,	0,0,	0,0,	
0,0,	78,78,	0,0,	0,0,	
0,0,	0,0,	78,78,	0,0,	
78,78,	0,0,	78,78,	78,78,	
78,78,	0,0,	0,0,	0,0,	
78,78,	0,0,	78,78,	78,78,	
0,0,	78,78,	0,0,	78,78,	
80,7,	80,85,	80,80,	0,0,	
78,78,	0,0,	0,0,	0,0,	
80,80,	78,78,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	80,80,	0,0,	80,80,	
80,80,	80,80,	0,0,	0,0,	
0,0,	80,80,	81,81,	80,80,	
80,80,	0,0,	80,80,	0,0,	
80,80,	0,0,	81,81,	81,86,	
0,0,	80,80,	0,0,	0,0,	
0,0,	0,0,	80,80,	0,0,	
0,0,	0,0,	0,0,	80,80,	
0,0,	80,80,	0,0,	80,80,	
80,80,	80,80,	0,0,	0,0,	
0,0,	80,80,	81,85,	80,80,	
80,80,	0,0,	80,80,	0,0,	
80,80,	81,81,	0,0,	81,81,	
0,0,	80,80,	0,0,	0,0,	
0,0,	81,81,	80,80,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	81,81,	0,0,	
81,81,	81,81,	81,81,	82,69,	
82,70,	0,0,	81,81,	0,0,	
81,81,	81,81,	0,0,	81,81,	
0,0,	81,81,	0,0,	0,0,	
0,0,	0,0,	81,81,	0,0,	
0,0,	0,0,	0,0,	81,81,	
0,0,	0,0,	0,0,	82,7,	
81,81,	0,0,	81,81,	0,0,	
81,81,	81,81,	81,81,	0,0,	
0,0,	0,0,	81,81,	83,83,	
81,81,	81,81,	82,64,	81,81,	
0,0,	81,81,	0,0,	83,83,	
83,86,	0,0,	81,81,	0,0,	
0,0,	0,0,	0,0,	81,81,	
0,0,	0,0,	0,0,	82,64,	
0,0,	82,64,	82,64,	82,64,	
0,0,	0,0,	0,0,	82,87,	
0,0,	82,64,	82,64,	0,0,	
82,64,	0,0,	82,64,	0,0,	
0,0,	0,0,	83,83,	82,64,	
83,83,	0,0,	0,0,	0,0,	
82,64,	0,0,	83,83,	0,0,	
0,0,	82,64,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	83,83,	
0,0,	83,83,	83,83,	83,83,	
0,0,	0,0,	0,0,	83,83,	
85,85,	83,83,	83,83,	0,0,	
83,83,	0,0,	83,83,	0,0,	
85,85,	85,88,	0,0,	83,83,	
0,0,	0,0,	0,0,	0,0,	
83,83,	0,0,	0,0,	0,0,	
0,0,	83,83,	0,0,	83,83,	
0,0,	83,83,	83,83,	83,83,	
0,0,	0,0,	0,0,	83,83,	
0,0,	83,83,	83,83,	0,0,	
83,83,	0,0,	83,83,	85,85,	
0,0,	85,85,	0,0,	83,83,	
0,0,	0,0,	0,0,	85,85,	
83,83,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
85,85,	0,0,	85,85,	85,85,	
85,85,	87,69,	87,70,	0,0,	
85,85,	0,0,	85,85,	85,85,	
0,0,	85,85,	0,0,	85,85,	
0,0,	0,0,	0,0,	0,0,	
85,85,	0,0,	0,0,	0,0,	
0,0,	85,85,	0,0,	0,0,	
0,0,	87,7,	85,85,	0,0,	
85,85,	0,0,	85,85,	85,85,	
85,85,	0,0,	89,69,	89,70,	
85,85,	0,0,	85,85,	85,85,	
87,64,	85,85,	0,0,	85,85,	
0,0,	0,0,	0,0,	0,0,	
85,85,	0,0,	0,0,	0,0,	
0,0,	85,85,	0,0,	0,0,	
0,0,	87,64,	89,7,	87,64,	
87,64,	87,64,	0,0,	0,0,	
0,0,	87,64,	0,0,	87,64,	
87,64,	0,0,	87,64,	87,89,	
87,64,	89,64,	0,0,	90,90,	
90,70,	87,64,	0,0,	0,0,	
0,0,	0,0,	87,64,	0,0,	
0,0,	0,0,	0,0,	87,64,	
0,0,	0,0,	89,64,	0,0,	
89,64,	89,64,	89,64,	0,0,	
0,0,	0,0,	89,64,	90,7,	
89,64,	89,64,	0,0,	89,90,	
0,0,	89,64,	0,0,	0,0,	
0,0,	0,0,	89,64,	0,0,	
0,0,	0,0,	90,64,	89,64,	
0,0,	0,0,	0,0,	0,0,	
89,64,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	90,64,	
0,0,	90,64,	90,64,	90,64,	
0,0,	0,0,	0,0,	90,64,	
0,0,	90,64,	90,64,	0,0,	
90,64,	0,0,	90,64,	0,0,	
0,0,	0,0,	0,0,	90,64,	
0,0,	0,0,	0,0,	0,0,	
90,64,	0,0,	0,0,	0,0,	
0,0,	90,64,	0,0,	0,0,	
0,0};
struct yysvf yysvec[] = {
0,	0,	0,
yycrank+0,	0,		0,	
yycrank+-1,	0,		0,	
yycrank+0,	0,		0,	
yycrank+-2,	yysvec+2,	0,	
yycrank+-4,	yysvec+2,	0,	
yycrank+-62,	yysvec+2,	0,	
yycrank+-106,	0,		yyvstop+1,
yycrank+-167,	yysvec+2,	0,	
yycrank+-211,	yysvec+2,	0,	
yycrank+-240,	yysvec+2,	0,	
yycrank+-284,	yysvec+2,	0,	
yycrank+-15,	yysvec+2,	0,	
yycrank+-7,	yysvec+2,	0,	
yycrank+0,	0,		yyvstop+3,
yycrank+-313,	yysvec+2,	0,	
yycrank+-12,	yysvec+2,	0,	
yycrank+-357,	yysvec+2,	0,	
yycrank+-17,	yysvec+2,	0,	
yycrank+-386,	yysvec+2,	0,	
yycrank+-20,	yysvec+2,	0,	
yycrank+-430,	yysvec+2,	0,	
yycrank+-9,	yysvec+2,	0,	
yycrank+-459,	yysvec+2,	0,	
yycrank+-45,	yysvec+2,	0,	
yycrank+-503,	yysvec+2,	0,	
yycrank+-50,	yysvec+2,	0,	
yycrank+-532,	yysvec+2,	0,	
yycrank+-42,	yysvec+2,	0,	
yycrank+-576,	yysvec+2,	0,	
yycrank+-23,	yysvec+2,	0,	
yycrank+-605,	yysvec+2,	0,	
yycrank+-154,	yysvec+2,	0,	
yycrank+-658,	yysvec+2,	0,	
yycrank+-55,	yysvec+2,	0,	
yycrank+-123,	yysvec+2,	0,	
yycrank+-150,	yysvec+2,	0,	
yycrank+-687,	yysvec+2,	0,	
yycrank+-265,	yysvec+2,	0,	
yycrank+-731,	yysvec+2,	0,	
yycrank+-179,	yysvec+2,	0,	
yycrank+-221,	yysvec+2,	0,	
yycrank+-760,	yysvec+2,	0,	
yycrank+-189,	yysvec+2,	0,	
yycrank+-804,	yysvec+2,	0,	
yycrank+-135,	yysvec+2,	0,	
yycrank+-259,	yysvec+2,	0,	
yycrank+-848,	yysvec+2,	0,	
yycrank+-367,	yysvec+2,	0,	
yycrank+-889,	yysvec+2,	0,	
yycrank+-252,	yysvec+2,	0,	
yycrank+-375,	yysvec+2,	0,	
yycrank+-405,	yysvec+2,	0,	
yycrank+-933,	0,		0,	
yycrank+-994,	yysvec+2,	0,	
yycrank+-294,	yysvec+2,	0,	
yycrank+-403,	yysvec+2,	0,	
yycrank+-1038,	0,		yyvstop+5,
yycrank+-1111,	0,		yyvstop+7,
yycrank+-1172,	yysvec+2,	0,	
yycrank+-29,	yysvec+2,	0,	
yycrank+-448,	yysvec+2,	0,	
yycrank+-1216,	0,		yyvstop+9,
yycrank+0,	0,		yyvstop+12,
yycrank+-1277,	yysvec+2,	yyvstop+14,
yycrank+-1310,	yysvec+2,	yyvstop+16,
yycrank+-1351,	yysvec+2,	0,	
yycrank+-478,	yysvec+2,	0,	
yycrank+0,	0,		yyvstop+18,
yycrank+-325,	yysvec+2,	yyvstop+21,
yycrank+0,	0,		yyvstop+23,
yycrank+-1382,	yysvec+2,	yyvstop+25,
yycrank+-1443,	0,		0,	
yycrank+-1504,	yysvec+2,	yyvstop+27,
yycrank+-1565,	0,		0,	
yycrank+-1648,	0,		yyvstop+29,
yycrank+-1721,	0,		0,	
yycrank+-1782,	yysvec+2,	yyvstop+31,
yycrank+-1843,	0,		0,	
yycrank+-17,	yysvec+74,	yyvstop+33,
yycrank+-1916,	0,		yyvstop+35,
yycrank+-1989,	0,		yyvstop+37,
yycrank+-2050,	yysvec+2,	yyvstop+39,
yycrank+-2094,	0,		yyvstop+41,
yycrank+-6,	yysvec+78,	yyvstop+43,
yycrank+-2167,	0,		yyvstop+45,
yycrank+0,	0,		yyvstop+48,
yycrank+-2228,	yysvec+2,	yyvstop+50,
yycrank+0,	0,		yyvstop+52,
yycrank+-2261,	yysvec+2,	yyvstop+55,
yycrank+-2302,	yysvec+2,	yyvstop+57,
0,	0,	0};
struct yywork *yytop = yycrank+2397;
struct yysvf *yybgin = yysvec+1;
unsigned char yymatch[] = {
00  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,011 ,012 ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
011 ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
'(' ,'(' ,'*' ,01  ,01  ,01  ,01  ,01  ,
'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,
'0' ,'0' ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,'A' ,'0' ,'C' ,'D' ,'E' ,'0' ,'0' ,
'0' ,'I' ,'0' ,'K' ,'L' ,'0' ,'N' ,'0' ,
'P' ,'0' ,'0' ,'0' ,'0' ,'U' ,'0' ,'0' ,
'0' ,'0' ,'Z' ,01  ,01  ,01  ,01  ,'_' ,
01  ,'a' ,01  ,'c' ,'d' ,'e' ,01  ,01  ,
01  ,'i' ,01  ,'k' ,'l' ,01  ,'n' ,01  ,
'p' ,01  ,01  ,01  ,01  ,'u' ,01  ,01  ,
01  ,01  ,'z' ,01  ,01  ,01  ,01  ,01  ,
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
0,1,1,1,1,0,0,0,
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
