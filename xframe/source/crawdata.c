/*
        crawdata.c
         Created           :  4-OCT-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "xframe/source/d0x_c.h"
char rawdbank[5];
int nrawcol = 9;
int rawcvalue = 0;

void crawdata(w,tag,reason)
Widget        w;
int        *tag;
unsigned long    *reason;
{
    int select = *tag, status;
    char *str, *bank, *scol, tbank[10];

/*
  tag 0 means change number of columns and redraw
  tag 1 means go 1st crate in list (called from main window)
  tag 2 means go to what is selected in crates window
  tag 3 means go to next crate in list
*/
    switch (select) {
        case 0:
/*
            get number of columns and proceed - -1/0 are same cases
*/
          scol = XmTextGetString(numrow_text);
          if ( strlen(scol) <= 0 ) nrawcol = 9;
          else nrawcol = atoi(scol);
          XtFree(scol);
          break;
        case 1:
/*
    get bank - default is MUD1 for lack of anything better
*/
          if (strlen(rawdbank) == 0) strcpy(rawdbank,"MUD1");
          strcpy(tbank,rawdbank);
          xgetchar("RAW data bank:",tbank,tbank,&status);
          strcpy(rawdbank,tbank);
          cupcase(rawdbank);
          break;
        case 3:
          break;
        case 2:
          str = XmTextGetSelection(crates_text);
          XtFree(str);
          rawcvalue = atoi(str);
          break;
        default:  printf("Illegal tag crawdata!!!\n"); return;
        }

/*
    call fortran subroutine which will get the info and shove it
    into the appropriate text widget
*/
    SetWatchCursor(raw_main);
#ifdef D0FLAVOR
    frawdata_(&select,&rawcvalue,rawdbank,&nrawcol);
#else
    frawdata(&select,&rawcvalue,rawdbank,&nrawcol);
#endif
    SetDefaultCursor(raw_main);
}
