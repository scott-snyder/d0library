/*
        cselect.c
         Created           : 26-AUG-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "xframe/source/d0x_c.h"

/*---------------------------------------------------------------------
  causes the linear bank number in text window to be read
----------------------------------------------------------------------*/
void cselect(w,tag,reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
	char *str, *bank;
	int	linear;
	int dum;

	str = XmTextGetString(xdbank_text_chain);
	linear = atoi(str);
	XtFree(str);
	/* pass this along to the common */
    dum = 1;
#ifdef D0FLAVOR
	fsetlin_(&linear);
    foenable_(&dum);      /* and enable selection */
#else
	fsetlin(&linear);
    foenable(&dum);
#endif
/*
    				get bank name
*/
	bank = XmTextGetString(xdbank_bank_2);
	   	/* now, call the fd0util routine with tag=0 for xdbank */
	dum = 0;
#ifdef D0FLAVOR
	fd0util_(&dum,&dum,bank);
#else
	fd0util(&dum,&dum,bank);
#endif
	XtFree(bank);
}
