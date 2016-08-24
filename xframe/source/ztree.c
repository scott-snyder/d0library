/*
        ztree.c
         Created           : 28-OCT-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */
#include <Xm/PushB.h>
#include <Xm/ArrowB.h>

#include "xframe/source/d0x_c.h"
#include "xframe/source/tree.h"
#include "xframe/source/treep.h"

	
Widget thewidget;
XtIntervalId timer;
int the_address;
static void bankbutton();
static void process_clicks();
extern Widget zebra_tree;
/* parent is the widget of parent (0=new, curtop)
   wnew is the widget created
   name is bank name (string)
   link is offset from pointer
   length is the length of the chain
*/
XFontStruct *zfont;
XmFontList  zfontlist;

Widget ztree(parent,name,link,lenb,address)
Widget parent;
char *name;
int link, lenb, address;
{
	Widget daughter;
	XmString str;
	char temp[80], bname[5];
	int i,n, allowed;
	Arg wargs[10];

    strcpy(bname,name);  bname[4] = 0;
/*
    use fixed character font (makes it smaller)
*/
	if ( zfont == 0 ) {
		zfont = XLoadQueryFont(XtDisplay(toplevel_widget),
"-misc-fixed-medium-r-normal-*-13-120-75-75-c-70-iso8859-1");
/*				"-*-fixed-medium-r-*-*-*-120-*-*-*-*-*-*");*/
		zfontlist = XmFontListCreate(zfont,"charset");
	}
	n = 0;
	XtSetArg(wargs[n], XmNfontList,zfontlist); n++;
/*
  setup push button
*/
	sprintf(temp,"%4s",name);
	if (lenb == 1) {
		sprintf(temp,"%4s,%d",name,link);
		XtSetArg(wargs[n], XmNborderWidth, 1); n++;
	}
	else {
		sprintf(temp,"%4s,%d(%d)",name,link,lenb);
		XtSetArg(wargs[n], XmNborderWidth, 2); n++;
	}
	XtSetArg(wargs[n], XtNsuperNode, parent); n++;
	str = XmStringCreateSimple(temp);
	XtSetArg(wargs[n], XmNlabelString,str); n++;
	XtSetArg(wargs[n], XmNshadowThickness, 0); n++;
	daughter =
    		XtCreateManagedWidget(temp, xmPushButtonWidgetClass,
    				zebra_tree, wargs, n);
    XtAddCallback(daughter, XmNactivateCallback, bankbutton, 
                  (void*)(unsigned long)address);
    XmStringFree(str);
    return(daughter);
}


void undotree(w,tag,reason)   /* called from D0X returning from navigator */
Widget w;
int *tag;
unsigned long *reason;
{
	XtUnmanageChild(zebra_tree);
}

static void bankbutton(w,address,reason)
Widget w;
XmPushButtonCallbackStruct *reason;
int address;
{

	the_address = address;
	thewidget = w;
	if (reason->click_count == 1 ) {
		timer = XtAppAddTimeOut(appcontext,1000,process_clicks,False);
	}
	else if (reason->click_count == 2) {
		XtRemoveTimeOut(timer);
		process_clicks(True);
	}
}


static void process_clicks(double_click)
int double_click;
{
	char *str;
	char bank[5];
	int select, n, dummy;
	int pos,size,incr,page;
	Arg wargs[10];
	Widget parent, scrollbar;
/*
  		get bank name from widget name
*/
	str = XtName(thewidget);
	strcpy(bank,str);
	bank[4] = 0;
/*
  double click means expand/contract
*/
	if ( double_click ) {
/*
  		check if in list - if not, add, if so, subtract
*/
		dummy = -2;
#ifdef D0FLAVOR
		fblist_(&dummy,bank,&select);
#else
		fblist(&dummy,bank,&select);
#endif
/*
  		call cd0util to process
*/
		XtSetArg(wargs[0], XmNverticalScrollBar, &scrollbar);
		XtGetValues(tree_scroll, wargs, 1);
		XmScrollBarGetValues(scrollbar,&pos,&size,&incr,&page);
		XtUnmanageChild(zebra_tree);
		SetWatchCursor(nav_main);
		SetWatchCursor(xdbank_main);
		select = -2;     /* new tree widget */
#ifdef D0FLAVOR
		fd0util_(&select,&the_address,bank);
#else
		fd0util(&select,&the_address,bank);
#endif
		XtManageChild(zebra_tree);
		XmScrollBarSetValues(scrollbar,pos,size,incr,page,True);
		SetDefaultCursor(nav_main);
		SetDefaultCursor(xdbank_main);
	}
/*
  single click means process this bank
*/
	else {
/*
  		call cd0util to process
*/
		select = -3;     /* data */
		SetWatchCursor(nav_main);
		SetWatchCursor(xdbank_main);
#ifdef D0FLAVOR
		fd0util_(&select,&the_address,bank);
#else
		fd0util(&select,&the_address,bank);
#endif
		select = -1;     /* .zeb file */
	    str = XmTextGetString(xdbank_bank_2);
	    dummy = strcmp(str,bank);
#ifdef D0FLAVOR
		if (dummy != 0) fd0util_(&select,&the_address,bank);
#else
		if (dummy != 0) fd0util(&select,&the_address,bank);
#endif
/*
  		set bank name
*/
		XmTextSetString(xdbank_bank_2,bank);
		SetDefaultCursor(nav_main);
		SetDefaultCursor(xdbank_main);
	}
}
