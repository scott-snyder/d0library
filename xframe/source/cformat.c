/*
        cformat.c
         Created           : 26-AUG-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "/d0library/scratch/test/xframe/source/d0x_c.h"
extern int the_address;
/*---------------------------------------------------------------------
    sets the format (equal to the tag) [auto, float,  etc.]
----------------------------------------------------------------------*/
void cformat(w,tag,reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
	char *bank;
	int select = *tag;
	int dum;
#ifdef D0FLAVOR
	fformat_(&select);
#else
	fformat(&select);
#endif
/*
  get bank name
*/
	bank = XmTextGetString(xdbank_bank_2);
/*
  fd0util routine with tag=-4 
*/
	dum = -4;
#ifdef D0FLAVOR
	fd0util_(&dum,&the_address,bank);
#else
	fd0util(&dum,&the_address,bank);
#endif
	XtFree(bank);
}

#ifdef D0FLAVOR
fbankaddr_(addr)
#else
fbankaddr(addr)
#endif
int *addr;
{
	    the_address = *addr;
}
	
