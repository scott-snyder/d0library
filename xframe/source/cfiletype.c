/*
        cfiletype.c
         Created           : 26-AUG-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "/d0lib/scratch/xframe/source/d0x_c.h"

/*---------------------------------------------------------------------
    hook to set the file type (FZ, RZ, STP, output)
----------------------------------------------------------------------*/
cfiletype(w,tag,reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
	int select = *tag;
    int mode = 1;
#ifdef D0FLAVOR
 		  ffiletype_(&mode,&select);
#else
 		  ffiletype(&mode,&select);
#endif

/*  don't forget to change the store to reflect this! */
	switch (select) {
		case 0:
		   cset_store(w,&select,reason);
	       XmTextSetString(path_text,"RECO"); /* default path RECO */
		   break;
	    case 1:
		   cset_store(w,&select,reason);
	       XmTextSetString(path_text,"    "); /* default path ? */
		   break;
		default:
		   break;
    	}
}
