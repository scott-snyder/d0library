/*
        check_asynch.c
         Created           : 26-AUG-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "xframe/source/d0x_c.h"

#include <X11/Intrinsic.h>

/*---------------------------------------------------------------------
  services X events asynchronously
----------------------------------------------------------------------*/
#ifdef D0FLAVOR
check_asynch_()
#else
check_asynch()
#endif
{
	XtAppContext context;

	context = XtWidgetToApplicationContext(main_text);
	while (XtAppPending(context))
		XtAppProcessEvent(context,XtIMAll);

}
