/*
        csearch.c
         Created           : 26-AUG-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */
#include <ctype.h>
#include "xframe/source/d0x_c.h"

/*---------------------------------------------------------------------
  search menu - 0=by run, 1=by event, 2=by bank
----------------------------------------------------------------------*/
int number;
char ch[100];
Boolean cfirst = True;

void csearch(w,tag,reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
	char *text;
	int select = *tag;
	int length, status;

	if ( cfirst ) {
		ch[0] = ' ';
		ch[1] = '\0';
		cfirst = False;
	}
	switch (select) {
		case 0:   /* by run */
		  xgetchar("Run number:",ch,ch,&status);
		  length = strlen(ch);
		  squeeze(ch,' ');    /* squeeze out the blanks */
 		  number = atoi(ch);
#ifdef D0FLAVOR
	      fsearch_(&select,ch,&number);
#else
	      fsearch(&select,ch,&number);
#endif
	    break;
		case 1:   /* by event */
		  xgetchar("Event number:",ch,ch,&status);
		  length = strlen(ch);
		  squeeze(ch,' ');    /* squeeze out the blanks */
 		  number = atoi(ch);
#ifdef D0FLAVOR
	      fsearch_(&select,ch,&number);
#else
	      fsearch(&select,ch,&number);
#endif
	    break;
		case 2:             /* by bank */
		  xgetchar("Bank Name:",ch,ch,&status);
		  length = strlen(ch);
		  squeeze(ch,' ');    /* squeeze out the blanks */
	      cupcase(ch);
#ifdef D0FLAVOR
 		  fsearch_(&select,ch,&length);
#else
 		  fsearch(&select,ch,&length);
#endif
		  break;
		case 3:             /* by l1 name */
		  xgetchar("Level 1 Trigger:",ch,ch,&status);
		  length = strlen(ch);
		  squeeze(ch,' ');    /* squeeze out the blanks */
	      cupcase(ch);
#ifdef D0FLAVOR
 		  fsearch_(&select,ch,&length);
#else
 		  fsearch(&select,ch,&length);
#endif
		  break;
		case 4:             /* by l2 name */
		  xgetchar("Level 2 Trigger:",ch,ch,&status);
		  length = strlen(ch);
		  squeeze(ch,' ');    /* squeeze out the blanks */
	      cupcase(ch);
#ifdef D0FLAVOR
 		  fsearch_(&select,ch,&length);
#else
 		  fsearch(&select,ch,&length);
#endif
		  break;
	    default:
	      cerrmsg("Illegal request SEARCH");
	    }


  	return;
}

void squeeze(char *s,char c)   /* delete all c from s */
{
	int i, j;

	for (i = j = 0; s[i] != '\0'; i++ )
	    if (s[i] != c)
	        s[j++] = s[i];

    s[j] = '\0';

}

void cupcase(string)   /* converts from lower to upper */
char string[];
{
	int i;


	for (i=0; string[i] != '\0'; i++)
	{
	   if (islower(string[i])) string[i] = toupper(string[i]);
	}

	return;
}

void clowcase(string)   /* converts upper to lower */
char string[];
{
	int i;

	for (i=0; string[i] != '\0'; i++)
	{
	   if (isupper(string[i])) string[i] = tolower(string[i]);
	}

	return;
}

