/*
        csetlin.c
         Created           : 26-AUG-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "/d0lib/scratch/xframe/source/d0x_c.h"

/*---------------------------------------------------------------------
  sets the value of length of the chain in the window
----------------------------------------------------------------------*/
#ifdef D0FLAVOR
csetlin_(chain)   /* only 3 characters allowed */
#else
csetlin(chain)
#endif
int *chain;
{
	char ch[50];

	sprintf(ch,"Chain length: %d",*chain);
	SetLabel(xdbank_text_length,ch);
/*	XmTextSetString(xdbank_text_length,ch); */
}
