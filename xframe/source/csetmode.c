/* 
        csetmode.c
         Created           : 26-AUG-1992 by Drew Baden
*/
 
#include <stdio.h>                   /* I/O definitions                       */
#include "xframe/source/d0x_c.h"
 
/*---------------------------------------------------------------------
  sets mode (0=exchange, 1=native) - not really needed (D0OPEN)
----------------------------------------------------------------------*/
void csetmode(w,tag,reason)  
Widget w;
int		*tag;
unsigned long	*reason;
{
	int select = *tag;
    XmString tmp;
	
#ifdef D0FLAVOR
	fmode_(&select);
#else
	fmode(&select);
#endif
	switch (select) {
		case 0:
			tmp = XmStringCreateSimple("Mode: EXCHANGE");
			XtVaSetValues(fz_mode, XmNlabelString, tmp, NULL);
			XmStringFree(tmp);
			break;
		case 1:
			tmp = XmStringCreateSimple("Mode: NATIVE");
			XtVaSetValues(fz_mode, XmNlabelString, tmp, NULL);
			XmStringFree(tmp);
			break;
		default:
			break;
	}
  	return;
}
