/*
        d0_init.c
         Created           : 26-AUG-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "/d0lib/scratch/xframe/source/d0x_c.h"

/*---------------------------------------------------------------------
    on unix, provide entry for DECs initialization
----------------------------------------------------------------------*/
#ifdef D0FLAVOR
DXmInitialize()   /* dummy for unix - initialize dec stuff */
{;}
#endif

/*---------------------------------------------------------------------
    global initialization (ZEBRA, etc.)
----------------------------------------------------------------------*/
d0_init(w,tag,reason)   /*  this routine should initialize all stuff
                            thru a call to mapinit (zebra, etc.) */
int w;
int		*tag;
unsigned long	*reason;

{
	Arg arglist[20];
	XFontStruct *font;
	XmFontList fontlist;
/*
  	set title of the program
    XtSetArg(arglist[0],XmNtitle,"D0X - File Browzer/Zebra Navigator");
*/
	font = XLoadQueryFont(XtDisplay(toplevel_widget),
			"-*-helvetica-medium-o-*-*-*-*-*-*-*-*-*-*");
	fontlist = XmFontListCreate(font,"charset");
    XtSetArg(arglist[0],XmNtitle,"D0X - V1 (Non Illegitimis Carborundum)");
	XtSetArg(arglist[1], XmNfontList, fontlist);
    XtSetValues(toplevel_widget,arglist,2);

/*
    initialize D0 stuff (Zebra...)
*/
#ifdef D0FLAVOR
	mapinit_();
#else
    mapinit();
#endif
}

