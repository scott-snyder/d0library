/*
        cset_store.c
         Created           : 26-AUG-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "/d0library/scratch/test/xframe/source/d0x_c.h"

/*---------------------------------------------------------------------
  stores "store" - 0=zebcom, 1=zetstp, 2=geant, 3=zebwrk
----------------------------------------------------------------------*/
void cset_store(w,tag,reason)

Widget		w;
int		*tag;
unsigned long	*reason;
{
	int select = *tag;
    XmString tmp;
#ifdef D0FLAVOR
	if (select != 5) fset_store_(&select);   /* no tapes in unix */
#else
	fset_store(&select);
#endif
	switch (select) {
		case 0:
			tmp = XmStringCreateSimple("Destination: ZEBCOM");
			XtVaSetValues(fzdestination, XmNlabelString, tmp, NULL);
			break;
		case 1:
			tmp = XmStringCreateSimple("Destination: ZEBSTP");
			XtVaSetValues(fzdestination, XmNlabelString, tmp, NULL);
			break;
		case 2:
			tmp = XmStringCreateSimple("Destination: GEANT");
			XtVaSetValues(fzdestination, XmNlabelString, tmp, NULL);
			break;
		case 3:
			tmp = XmStringCreateSimple("Destination: ZEBWRK");
			XtVaSetValues(fzdestination, XmNlabelString, tmp, NULL);
			break;
		case 4:   /* set for disk */
			tmp = XmStringCreateSimple("Disk/Tape: DISK");
			XtVaSetValues(diskortape, XmNlabelString, tmp, NULL);
			break;
		case 5:   /* set for tape */
#ifdef D0FLAVOR
			tmp = XmStringCreateSimple("Disk/Tape: DISK");
			warning("Tapes not available in UNIX");
#else
			tmp = XmStringCreateSimple("Disk/Tape: TAPE");
#endif
			XtVaSetValues(diskortape, XmNlabelString, tmp, NULL);
			break;
		default:
			break;
	}
	XmStringFree(tmp);
}
