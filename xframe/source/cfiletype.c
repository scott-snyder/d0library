/*
        cfiletype.c
         Created           : 26-AUG-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "xframe/source/d0x_c.h"
extern char pathtext[100];
/*---------------------------------------------------------------------
    hook to set the file type (FZ, RZ, STP, output)
----------------------------------------------------------------------*/
void cfiletype(w,tag,reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
	int select = *tag;
    int mode = 1;
    XmString tmp;
    
#ifdef D0FLAVOR
 		  ffiletype_(&mode,&select);
#else
 		  ffiletype(&mode,&select);
#endif

/*  don't forget to change the store to reflect this! */
	switch (select) {
		case 0:
		   cset_store(w,&select,reason);
	       tmp = XmStringCreateSimple("File type: FZ");
	       strcpy(pathtext,"RECO");
		   break;
	    case 1:
		   cset_store(w,&select,reason);
	       tmp = XmStringCreateSimple("File type: STP");
	       strcpy(pathtext,"    ");
		   break;
		default:
		   break;
	}
	XtVaSetValues(fztype, XmNlabelString, tmp, NULL);
	XmStringFree(tmp);
	mode = 2;
	czebra(w,&mode,reason);
}
