/*
        cnavigate.c
         Created           : 26-AUG-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "xframe/source/d0x_c.h"

/*---------------------------------------------------------------------
    changes LQ offset in order to navigate thru the zebra structure
----------------------------------------------------------------------*/
void cnavigate(w,tag,reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
	char ch[10], *str;
	int length;
	int select = *tag;

/*
   					get it's value, increment accordinly, store
*/
	str = XmTextGetString(xdbank_text_navigate);
	sscanf(str,"%d",&length);
	XtFree(str);
	length = length + select;
	sprintf(ch,"%d",length);
	XmTextSetString(xdbank_text_navigate,ch);
}

