/* 
        warning.c
         Created           : 14-JAN-1993 by Drew Baden
*/
 
#include <stdio.h>                   /* I/O definitions                       */
#include "xframe/source/d0x_c.h"
#ifdef D0FLAVOR
#include <Xm/MessageB.h>
#endif

void warning(str)
char *str;
{
	XmString text1,text2;
	Arg wargs[10];
	Widget dialog;
	int n = 0;
	
/*
  the message
*/
	text1 = XmStringCreateSimple(str);
	XtSetArg(wargs[n], XmNmessageString, text1); n++;
/*
  the title
*/
	text2 = XmStringCreateSimple("Warning");
	XtSetArg(wargs[n], XmNdialogTitle, text2); n++;
/*
  start at 800 100
*/
	XtSetArg(wargs[n], XmNdefaultPosition, False); n++;
	XtSetArg(wargs[n], XmNy, 100); n++;
	XtSetArg(wargs[n], XmNx, 800); n++;
/*
  modality
*/
	XtSetArg(wargs[n], XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL); n++;
/*
  create it
*/
	dialog = XmCreateMessageDialog(toplevel_widget,"message",wargs,n);
	XtSetSensitive(
		XmMessageBoxGetChild(dialog,XmDIALOG_HELP_BUTTON), False);
	XtUnmanageChild(
		XmMessageBoxGetChild(dialog,XmDIALOG_CANCEL_BUTTON));
	XtManageChild(dialog);
/*	XRaiseWindow(XtDisplay(toplevel_widget),dialog);*/
	XmStringFree(text1);
	XmStringFree(text2);
}

void
#ifdef D0FLAVOR
fwarning_(str)
#else
fwarning(str)
#endif
char *str;
{
	warning(str);
}
