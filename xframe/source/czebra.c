/*
        czebra.c
         Created           : 26-AUG-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "/d0lib/scratch/xframe/source/d0x_c.h"

/*---------------------------------------------------------------------
  zebra menu - 0=dump, 1=verify, 3=path
----------------------------------------------------------------------*/
czebra(w,tag,reason)

Widget		w;
int		*tag;
unsigned long	*reason;
{
	char *path;
	int select = *tag, reset;
	switch (select) {
		case 0:				/* dump */
		  break;
  		case 1:  			/* verify */
  		  break;
  		case 3:				/* path */
 		  path = XmTextGetString(path_text);
 		  break;
 		default:
		  printf(" ***Illegal tag presented 'zebra util'***\n");
		  return;
		}
#ifdef D0FLAVOR
	fzebra_(&select,path);
#else
	fzebra(&select,path);
#endif
switch (select) {
		case 0:				/* dump */
		  break;
  		case 1:  			/* verify */
  		  break;
  		case 3:				/* path */
	      XmTextSetString(path_text,path);
 		  break;
 		default:
		  printf(" ***Illegal tag presented 'zebra util'***\n");
		  return;
		}

	XtFree(path);
	return;
}
