/*
        setcurtop.c
         Created           : 27-AUG-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "/d0library/scratch/test/xframe/source/d0x_c.h"

void csetcurtop(w,tag,reason)    /* tag=0, set to main, else to navigator */
Widget w;
int *tag;
unsigned long   *reason;
{
	int select = *tag;

	switch (select) {
		case 0:            /* set curtop to toplevel_widget */
		  curtop = toplevel_widget;
		  curtext = main_text;
		  break;
  	    case 1:
		  curtop = xdbank_main;
		  curtext = navigate_text;
		  break;
		default:
		  printf(" ***Illegal tag presented - set_curtop-->csetcurtop\n");
		  break;
		  }
}
