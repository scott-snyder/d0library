/*
        control.c
         Created           : 26-AUG-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "/d0lib/scratch/xframe/source/d0x_c.h"

/*---------------------------------------------------------------------
  control menu - 0=next, 1=scan, 2=skip
----------------------------------------------------------------------*/
control(w,tag,reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
	char *text;
	int select = *tag;
	int skip;
	switch (select) {
		case 0:				/* next */
		  break;
  		case 1:  			/* scan */
  		  break;
  		case 2:				/* skip */
 		  text = XmTextGetString(skip_text);
 		  skip = atoi(text);
 		  XtFree(text);
  		  break;
 		default:
		  printf(" ***Illegal tag presented 'control'***\n");
		  return;
		}
#ifdef D0FLAVOR
	fcontrol_(&select,&skip);
#else
	fcontrol(&select,&skip);
#endif
  	return;
}
