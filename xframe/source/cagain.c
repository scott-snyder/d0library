/*
        cagain.c
         Created           : 26-AUG-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "/d0library/scratch/test/xframe/source/d0x_c.h"

/*---------------------------------------------------------------------
    go navigates thru the zebra structure
----------------------------------------------------------------------*/
void cagain(w,tag,reason)
Widget        w;
int        *tag;
unsigned long    *reason;
{
    char *bank, *bank2, *str;
    int off, dum;
/*
    tag = 0, put data into window
    first get the offset from lq(0)
*/
    str = XmTextGetString(xdbank_text_navigate);
    off = atoi(str);
    XtFree(str);
/*
    and pass it along into common
*/
    dum = 0;
#ifdef D0FLAVOR
    fsetoff_(&off);   /* set offset (navigation) to zero */
    foenable_(&dum);      /* and enable offset since 0 is meaningful */
#else
    fsetoff(&off);
    foenable(&dum);
#endif
/*
    get bank name
*/
    bank = XmTextGetString(xdbank_bank_2);
/*
    set the cursor to "watch"
*/
    SetWatchCursor(nav_main);
    SetWatchCursor(xdbank_main);
/*
    put up the text of data and .zeb
*/
#ifdef D0FLAVOR
    dum = 0;
    fd0util_(&dum,&dum,bank);
    dum = -1;
    bank2 = XmTextGetString(xdbank_bank_2);
    off = strcmp(bank,bank2);
    if (off != 0) fd0util_(&dum,&dum,bank2);
#else
    dum = 0;
    fd0util(&dum,&dum,bank);
    dum = -1;
    bank2 = XmTextGetString(xdbank_bank_2);
    off = strcmp(bank,bank2);
    if (off != 0) fd0util(&dum,&dum,bank2);
#endif
    XtFree(bank);
    XtFree(bank2);
/*
    reset the cursor from "watch"
*/
    SetDefaultCursor(nav_main);
    SetDefaultCursor(xdbank_main);

}
