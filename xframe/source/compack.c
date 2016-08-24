/* 
        compack.c
         Created           : 26-AUG-1992 by Drew Baden
*/
 
#include <stdio.h>                   /* I/O definitions                       */
 
int getit(char s[], int lim);

/*---------------------------------------------------------------------
    part of my own little compack!
----------------------------------------------------------------------*/
#ifdef D0FLAVOR
void cgetpar_(prompt,line,len,maxline)
#else
void cgetpar(prompt,line,len,maxline)
#endif
char *prompt;
int *len, maxline;
char *line;
{
	/*  typeout prompt */
	
	printf("\n%s :",prompt);

	/*  get the rest of the line */
	
	*len = getit(line, maxline);
	return;
}

/*---------------------------------------------------------------------
  gets line into s, returns length 
  note:  returned is the ACTUAL length. the '\n' is stripped off AND a '\0'
  		 is appended, but the length is ONLY the number of real characters 
  		 (excluding the '\0')
----------------------------------------------------------------------*/
int getit(s,lim)
char s[];
int lim;
{
		int c, i;
		
		for (i=0; i<lim-1 && (c=getchar())!=EOF && c!='\n'; ++i)
			s[i] = c;

		s[i] = '\0';
			
		return(i);
}

 
/*---------------------------------------------------------------------
    part of my own little compack!
----------------------------------------------------------------------*/
#ifdef D0FLAVOR
void strmov_(source,length,target)
#else
void strmov(source,length,target)
#endif
char *source,*target;
int length;
{
	int i;
	for (i=0; i<=length; i++) target[i] = source[i];
	return;
	
}
