/*
        cstuffzeb.c
         Created           : 26-AUG-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "/d0lib/scratch/xframe/source/d0x_c.h"

/*---------------------------------------------------------------------
  stuffs zeb file into widget
----------------------------------------------------------------------*/
#ifdef D0FLAVOR
cstuffzeb_(ch,type)
#else
cstuffzeb(ch,type)
#endif
char *ch;
int *type;
{
	int select = *type;
	int pos,size,incr,page;
	Arg wargs[10];
	XmTextPosition  top = 0;
	Widget scrollbar, scrollwindow, textwidget;

	switch (select) {
	case 0:   /*   .zeb file goes here */
		scrollwindow = zebscroll;
		textwidget = xdbank_text_top;
		break;
	case 1:   /*    data goes here */
		scrollwindow = data_scroll;
		textwidget = xdbank_text_bottom;
		break;
	case 2:   /*    obsolete */
		printf(" Illegal tag 2 in CSTUFFZEB - sendmail to DREW");
		break;
	}
/*
  get scrollbar widget id
*/
	XtSetArg(wargs[0], XmNverticalScrollBar, &scrollbar);
	XtGetValues(scrollwindow, wargs, 1);
/*
  unmanage text widget and scrollwindow widget
*/
	XtUnmanageChild(textwidget);
/*
  set text in text widget
*/
	XmTextSetString(textwidget,ch);
/*
  reset scrollbar position
*/
	XtSetArg(wargs[0], XmNvalue, 0);
	XtSetValues(scrollbar, wargs, 1);
/*
  manange scrollwindow and text
*/
	XtManageChild(textwidget);
}
