/*
        cagain.c
         Created           : 26-AUG-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "/d0lib/scratch/xframe/source/d0x_c.h"

/*---------------------------------------------------------------------
    go navigates thru the zebra structure
----------------------------------------------------------------------*/
cagain(w,tag,reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
	char *bank, *str;
	int off, dum;

/*
  		  			tag = 0, put data into window
  		  			first get the linear chain number
*/
	str = XmTextGetString(xdbank_text_navigate);
	off = atoi(str);
	XtFree(str);
/*
  	      			and pass it along into common
*/
    dum = 0;
#ifdef D0FLAVOR
    fsetoff_(&off);   /* set offset (navigation) to zero */
    foenable_(&dum);      /* and enable offset since 0 is meaningful */
#else
    fsetoff(&off);
    foenable(&dum);
#endif
/*
    				get bank name
*/
	bank = XmTextGetString(bank_text);
/*
          			and finally, put up the text of data
*/
	dum = 0;
#ifdef D0FLAVOR
	fd0util_(&dum,&dum,bank);
#else
	fd0util(&dum,&dum,bank);
#endif
	XtFree(bank);
}
