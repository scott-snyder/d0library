/*
        d0_init.c
         Created           : 26-AUG-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "/d0library/scratch/test/xframe/source/d0x_c.h"

/*---------------------------------------------------------------------
    on unix, provide entry for DECs initialization
----------------------------------------------------------------------*/
#ifdef D0FLAVOR
void DXmInitialize()   /* dummy for unix - initialize dec stuff */
{;}
#endif

/*---------------------------------------------------------------------
    global initialization (ZEBRA, etc.)
----------------------------------------------------------------------*/

void d0_init(w,tag,reason)   /*  this routine should initialize all stuff
                            thru a call to mapinit (zebra, etc.) */
Widget w;
int		*tag;
unsigned long	*reason;

{
	Arg arglist[20];
	XFontStruct *font;
	XmFontList fontlist;
	int dum1, dum=0;
/*
  	set title of the program
    XtSetArg(arglist[0],XmNtitle,"D0X - File Browzer/Zebra Navigator");
	font = XLoadQueryFont(XtDisplay(toplevel_widget),
			"-*-helvetica-medium-o-*-*-*-*-*-*-*-*-*-*");
	fontlist = XmFontListCreate(font,"charset");
    XtSetArg(arglist[0],XmNtitle,"D0X - V3");
	XtSetArg(arglist[1], XmNfontList, fontlist);
    XtSetValues(toplevel_widget,arglist,2);
*/

/*
    initialize D0 stuff (Zebra...) and call d0xuser_init
*/
#ifdef D0FLAVOR
	mapinit_();
	dum1 = d0xuser_init_(&dum);
#else
    mapinit();
	dum1 = d0xuser_init(&dum);
#endif
}

