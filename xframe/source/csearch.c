/*
        csearch.c
         Created           : 26-AUG-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */
#include <ctype.h>
#include "/d0lib/scratch/xframe/source/d0x_c.h"

/*---------------------------------------------------------------------
  search menu - 0=by run, 1=by event, 2=by bank
----------------------------------------------------------------------*/
csearch(w,tag,reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
	char *text;
	int select = *tag;
	int number;
	char ch[100];

/*
    grab the text, shove into ch, squeeze out blanks
*/
	text = XmTextGetString(search_text);
 	strcpy(ch,text);
 	squeeze(ch,' ');    /* squeeze out the blanks */
/*
    "switch"
*/
	switch (select) {
		case 0:   /* by run */
 		  number = atoi(ch);
#ifdef D0FLAVOR
	      fsearch_(&select,&number);
#else
	      fsearch(&select,&number);
#endif
	    break;
		case 1:   /* by event */
 		  number = atoi(ch);
#ifdef D0FLAVOR
	      fsearch_(&select,&number);
#else
	      fsearch(&select,&number);
#endif
	    break;
		case 2:             /* by bank */
	      cupcase(ch);
#ifdef D0FLAVOR
 		  fsearch_(&select,ch);
#else
 		  fsearch(&select,ch);
#endif
	      ch[4] = '\0';
	      XmTextSetString(bank_text,ch);
		  break;
		case 3:             /* by l1 name */
	      cupcase(ch);
#ifdef D0FLAVOR
 		  fsearch_(&select,ch);
#else
 		  fsearch(&select,ch);
#endif
		  break;
		case 4:             /* by l2 name */
	      cupcase(ch);
#ifdef D0FLAVOR
 		  fsearch_(&select,ch);
#else
 		  fsearch(&select,ch);
#endif
		  break;
	    default:
	      cerrmsg("Illegal request SEARCH");
	    }

/*
    reset to a blank and free up
	text[0] = 0;
	XmTextSetString(search_text,text);
*/
	XtFree(text);

  	return;
}

squeeze(s,c)   /* delete all c from s */
char s[];
char c;
{
	int i, j;

	for (i = j = 0; s[i] != '\0'; i++ )
	    if (s[i] != c)
	        s[j++] = s[i];

    s[j] = '\0';

}

cupcase(string)   /* converts from lower to upper */
char string[];
{
	int i;


	for (i=0; string[i] != '\0'; i++)
	{
	   if (islower(string[i])) string[i] = toupper(string[i]);
	}

	return;
}

clowcase(string)   /* converts upper to lower */
char string[];
{
	int i;

	for (i=0; string[i] != '\0'; i++)
	{
	   if (isupper(string[i])) string[i] = tolower(string[i]);
	}

	return;
}

