/* 
        warning.c
         Created           : 14-JAN-1993 by Drew Baden
*/
 
#include <stdio.h>                   /* I/O definitions                       */
#include "/d0lib/scratch/xframe/source/d0x_c.h"
#ifdef D0FLAVOR
#include <Xm/MessageB.h>
#endif

warning(str)
char *str;
{
	XmString text;
	Arg wargs[10];
	Widget dialog;
	
	text = XmStringCreateSimple(str);
	XtSetArg(wargs[0], XmNmessageString, text);
	XtSetArg(wargs[1], XmNtitle, XmStringCreateSimple("Warning"));
	dialog = XmCreateMessageDialog(toplevel_widget,"message",wargs,2);
	XtSetSensitive(
		XmMessageBoxGetChild(dialog,XmDIALOG_HELP_BUTTON), False);
	XtUnmanageChild(
		XmMessageBoxGetChild(dialog,XmDIALOG_CANCEL_BUTTON));
	XtManageChild(dialog);
	XtPopup(XtParent(dialog), XtGrabNone);
}
