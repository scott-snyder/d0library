/* 
        xgetchar.c
         Created           :  4-FEB-1993 by Drew Baden
   							  gets a character string using dialogs 
*/
 
#include <stdio.h>                   /* I/O definitions                       */
#include "/d0lib/scratch/xframe/source/d0x_c.h"
#include <Xm/SelectioB.h>

Widget askit;
Boolean wait_cancel = False;
Boolean prompt_not_finished = False;
void return_input();
void cancelit();
void helpit();
void FlushEvents();

xgetchar(prompt,default_line,result,status)       
char *prompt,*default_line,*result;
int *status;
{
	char message[100], *input_text;
	XmString input_string;
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
	XtSetArg(wargs[n],XmNselectionLabelString,
		XmStringCreateSimple(prompt)); n++;
/*
  default string
*/
    if (strlen(default_line) != 0) XtSetArg (wargs[n],XmNtextString,
    		XmStringCreateSimple(default_line)); n++;
/*
  make widget and add callbacks
*/
    askit = XmCreatePromptDialog(toplevel_widget,"dialog",wargs, n);
    XtAddCallback(askit,XmNokCallback,return_input,NULL);
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
