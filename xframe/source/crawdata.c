/*
        crawdata.c
         Created           :  4-OCT-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "/d0lib/scratch/xframe/source/d0x_c.h"

crawdata(w,tag,reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
	int select = *tag, ncol, value = 0;
	char *str, *bank, *scol, tbank[10];

/*
    get bank - default is MUD1 for lack of anything better
*/
	bank = XmTextGetString(bank_text);
	strcpy(tbank,bank);
	XtFree(bank);
	if ( strlen(tbank) == 0 ) strcpy(tbank,"MUD1");
	if ( strlen(tbank) != 4 ) tbank[4] = '\0';
	cupcase(tbank);
	XmTextSetString(bank_text,tbank);
/*
    get number of columns and proceed - -1/0 are same cases
*/
	scol = XmTextGetString(numrow_text);
	if ( strlen(scol) <= 0 ) ncol = 9;
	else ncol = atoi(scol);
	XtFree(scol);
	value = 0;
/*
  tag 0 means change number of columns and redraw
  tag 1 means go 1st crate in list (called from main window)
  tag 2 means go to what is selected in crates window
  tag 3 means go to next crate in list
*/
	switch (select) {
		case 0:
		case 1:
		case 3:
		  break;
		case 2:
		  str = XmTextGetSelection(crates_text);
		  XtFree(str);
		  value = atoi(str);
		  break;
		default:  printf("Illegal tag crawdata!!!\n"); return;
		}

/*
    call fortran subroutine which will get the info and shove it
    into the appropriate text widget
*/
#ifdef D0FLAVOR
	frawdata_(&select,&value,&tbank,&ncol);
#else
	frawdata(&select,&value,&tbank,&ncol);
#endif
}
