/*
        czebra.c
         Created           : 26-AUG-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "xframe/source/d0x_c.h"

/*---------------------------------------------------------------------
  zebra menu - 0=dump, 1=verify, 3=path
----------------------------------------------------------------------*/
char pathtext[100]="HEAD";

void czebra(w,tag,reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
	int select = *tag, reset, status;
	char tstring[100];
	XmString tmp;
	switch (select) {
		case 0:				/* dump */
		  break;
  		case 1:  			/* verify */
  		  break;
  		case 3:				/* path */
  		  xgetchar("ZEBRA PATH name:",pathtext,pathtext,&status);
  		case 2:				/* path called from cfiletype */
  		  strcpy(tstring,"Set Path (");
  		  strcat (tstring,pathtext);
  		  strcat(tstring,")");
  		  tmp = XmStringCreateSimple(tstring);
  		  XtVaSetValues(zpath, XmNlabelString, tmp, NULL);
  		  XmStringFree(tmp);
 		  break;
 		default:
		  printf(" ***Illegal tag presented 'zebra util'***\n");
		  return;
		}
#ifdef D0FLAVOR
	fzebra_(&select,pathtext);
#else
	fzebra(&select,pathtext);
#endif
}
