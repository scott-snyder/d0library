/* 
        xgetchar.c
         Created           :  4-FEB-1993 by Drew Baden
   							  gets a character string using dialogs 
*/
 
#include <stdio.h>                   /* I/O definitions                       */
#include "xframe/source/d0x_c.h"

extern XtAppContext appcontext;
extern Widget toplevel_widget;
Widget askit;
Boolean wait_cancel = False;
Boolean prompt_not_finished = False;
void return_input();
static void cancelit();
void helpit();
void FlushEvents();

void xgetchar(prompt,default_line,result,status)       
char *prompt,*default_line,*result;
int *status;
{
	char message[100], *input_text;
	XmString input_string, text1, text2, text3;
	Arg wargs[10];
	int n = 0;
	
	
/*
  get rid of old widget and reset
*/
	if (askit != NULL) XtDestroyWidget(askit);
	askit = NULL;
/*
  prompt string 
*/
	text1 = XmStringCreateSimple(prompt);
	XtSetArg(wargs[n],XmNselectionLabelString,text1);n++;
	text2 = XmStringCreateSimple("D0X GETPAR");
	XtSetArg(wargs[n],XmNdialogTitle,text2); n++;
/*
  start at 800 100
*/
	XtSetArg(wargs[n], XmNdefaultPosition, False); n++;
	XtSetArg(wargs[n], XmNy, 100); n++;
	XtSetArg(wargs[n], XmNx, 800); n++;
/*
  default string
*/
	text3 = NULL;
    if (strlen(default_line) >0) {
    	text3 = XmStringCreateSimple(default_line);
    	XtSetArg(wargs[n],XmNtextString,text3); n++;
    }
/*
  make widget and add callbacks
*/
    askit = XmCreatePromptDialog(toplevel_widget,"dialog",wargs, n);
    XtAddCallback(askit,XmNokCallback,return_input,NULL);
	XmStringFree(text1);
	XmStringFree(text2);
	if (text3 != NULL) XmStringFree(text3);
/*
  help message
	sprintf(message,"%s and then Hit OK,\n or quit with Cancel !\0",
             prompt);
    XtAddCallback(askit,XmNhelpCallback,helpit,message);
*/
    XtAddCallback(askit,XmNcancelCallback,cancelit,NULL);
/*
  now wait around for loop to finishe
*/
	XtManageChild(askit);
/*	XRaiseWindow(XtDisplay(toplevel_widget),askit);*/
	prompt_not_finished = True;
	wait_cancel = False;
	*status = 0;
	while (prompt_not_finished) FlushEvents();
/*
  ok, check status
*/
	if (wait_cancel) {
		*status = -1;
		return;
	}
/*
  get result
*/
	n = 0;
    XtSetArg(wargs[n],XmNtextString,&input_string); n++;
    XtGetValues(askit,wargs,n);
    XmStringGetLtoR(input_string,XmSTRING_DEFAULT_CHARSET,&input_text);
    strncpy(result,input_text,strlen(input_text));
    result[strlen(input_text)] = '\0';

}

void return_input(w,tag,reason)
Widget w;
int *tag;
unsigned long *reason;
{
	prompt_not_finished = False;
}

static void cancelit(w, tag, reason)
Widget w;
int *tag;
unsigned long *reason;
{
	prompt_not_finished = False;
	wait_cancel = True;
}

void FlushEvents()
{
	XEvent event;
	
	while (XtAppPending(appcontext)) {
		XtAppNextEvent(appcontext, &event);
		XtDispatchEvent(&event);
	}
	
}

void
#ifdef D0FLAVOR
fxgetchar_(prompt,default_line,result,status)
#else
fxgetchar(prompt,default_line,result,status)
#endif
char *prompt,*default_line,*result;
int *status;
{
	xgetchar(prompt,default_line,result,status);
}
