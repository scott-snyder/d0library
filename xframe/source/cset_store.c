/*
        cset_store.c
         Created           : 26-AUG-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "/d0lib/scratch/xframe/source/d0x_c.h"

/*---------------------------------------------------------------------
  stores "store" - 0=zebcom, 1=zetstp, 2=geant, 3=zebwrk
----------------------------------------------------------------------*/
cset_store(w,tag,reason)

Widget		w;
int		*tag;
unsigned long	*reason;
{
	int select = *tag;
#ifdef D0FLAVOR
	fset_store_(&select);
#else
	fset_store(&select);
#endif
/*
    if the store is 1 (stp), set STPH in the bank_text widget
*/
	if (select == 1)
	  XmTextSetString(bank_text,"STPH");
  	return;
}
