/*
        csetadd.c
         Created           : 26-AUG-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "/d0lib/scratch/xframe/source/d0x_c.h"

/*---------------------------------------------------------------------
    set the address
----------------------------------------------------------------------*/
#ifdef D0FLAVOR
csetadd_(address,chain)
#else
csetadd(address,chain)
#endif
int *address, *chain;
{
	char ch[50];

	sprintf(ch,"Current address: %d   Chain length: %d",*address,*chain);
	XmTextSetString(navigate_text,ch);
	/*	SetLabel(xdbank_text_address,ch);   */
}

