/*
        cformat.c
         Created           : 26-AUG-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "/d0lib/scratch/xframe/source/d0x_c.h"

/*---------------------------------------------------------------------
    sets the format (equal to the tag) [auto, float,  etc.]
----------------------------------------------------------------------*/
cformat(w,tag,reason)
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
	bank = XmTextGetString(bank_text);
/*
  					now, call the fd0util routine with tag=0 for xdbank
*/
	dum = 0;
#ifdef D0FLAVOR
	fd0util_(&dum,&dum,bank);
#else
	fd0util(&dum,&dum,bank);
#endif
	XtFree(bank);
}
