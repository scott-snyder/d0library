/*
        csetadd.c
         Created           : 26-AUG-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "/d0library/scratch/test/xframe/source/d0x_c.h"

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

	sprintf(ch,"Address: %d",*address);
	SetLabel(navaddresslab,ch);
	sprintf(ch,"Chain Length: %d",*chain);
	SetLabel(navchainlab,ch);
}
