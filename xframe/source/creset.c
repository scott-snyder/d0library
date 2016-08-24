/*
        creset.c
         Created           : 26-AUG-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "xframe/source/d0x_c.h"

/*---------------------------------------------------------------------
    reset the bank name (navigating)
----------------------------------------------------------------------*/
void
#ifdef D0FLAVOR
creset_(bank,off)
#else
creset(bank,off)
#endif
char *bank;
int *off;
{
	char ch[10];

	sprintf(ch,"%4s",bank);
	ch[4] = '\0';
	XmTextSetString(xdbank_bank_2,ch);

	sprintf(ch,"%d",*off);
	XmTextSetString(xdbank_text_navigate,ch);

}

