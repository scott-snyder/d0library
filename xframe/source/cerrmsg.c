/*
        cerrmsg.c
         Created           : 27-AUG-1992 by Drew Baden
  This routine will put up a message in the "message" window.  So you
  have to have one created!  "curtext" is the "current" text
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "xframe/source/d0x_c.h"

#define MAXSTR 8000   /* 80char*100 lines max */
void cerrmsg(text)
char *text;
{
	char new[MAXSTR];
	char *old;
	int len_old, len_new, maxl;


    XmTextSetString(curtext,text);
/*
	* if this is the main window, we scroll... *

	if ( curtext == main_text)
	{

	* add a \n to the end of new text and copy it into new *
	    strcpy(new,text);
	    strcat(new,"\n");
	    * get the text currently in the widget *

	    old = XmTextGetString(curtext);

		* add as much of the old string to the end of the new
		   up to MAXSTR total length *

	    len_old = strlen(old);
	    len_new = strlen(new);
	    maxl = (MAXSTR > len_old + len_new) ? len_old : MAXSTR;
	    strncat(new,old,maxl);
	    XmTextSetString(curtext,new);

	}
	else
	* not the main, must be navigate_text so it's simple... *

	    XmTextSetString(curtext,text);
*/
}

