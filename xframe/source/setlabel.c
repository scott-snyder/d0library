/*
        setlabel.c
         Created           :  8-SEP-1992 by Drew Baden

   this thing will set a widget label to a string - both passed
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "/d0lib/scratch/xframe/source/d0x_c.h"

SetLabel(w,string)
int *w;
char *string;
{
	Arg arglist[20];
	int narg;

	narg = 0;
	XtSetArg(arglist[narg],XmNlabelString,
	 XmStringCreateLtoR(string,XmSTRING_DEFAULT_CHARSET)); narg++;
	XtSetValues(w,arglist,narg);

}
