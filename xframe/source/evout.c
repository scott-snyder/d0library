/* 
        evout.c
         Created           :  3-NOV-1992 by Drew Baden
*/
 
#include <stdio.h>                   /* I/O definitions                       */
#include "/d0lib/scratch/xframe/source/d0x_c.h"

#define KEEP 1
#define DROP 0
int dork = KEEP;
int mode = 1;          /* 0=exchange, 1=native */
char *fname; 			   /* filename of output file */
int isitopen = 0;         /* 0=is close, 1=is open */
int length;
Widget w_list;         /* scrolled list widget */
char *list;            /* for bank list */

evout(w,tag,reason)
int *w, *tag;
unsigned long *reason;
{
	int select = *tag, dummy, n;
	Arg wargs[10];
	switch (select) {
		case 0:            /* set mode exchange */
			mode = 0;  
			break;
		case 1:            /* set mode native */
			mode = 1;
			break;
		case 2:            /* open file */
			if ( isitopen == 1 ) {
				warning("Output file already opened - close first");
				break;
			}
			fname = XmTextGetString(outtext);
			length = strlen(fname);
			if ( length < 1 ) {
				warning("Specify file in text window first!");
				break;
			}
			isitopen = 1;
#ifdef D0FLAVOR
			fevout_(&isitopen,&mode,&length,fname);
#else
			fevout(&isitopen,&mode,&length,fname);
#endif
			break;
		case 3:            /* close file */
			if ( isitopen == 0 ) {
				warning("No output file opened!");
				break;
			}
			fname = XmTextGetString(outtext);
			length = strlen(fname);
			if ( length < 1 ) {
				warning("Specify file in text window first!");
				break;
			}
			isitopen = 0;
#ifdef D0FLAVOR
			fevout_(&isitopen,&mode,&length,fname);
#else
			fevout(&isitopen,&mode,&length,fname);
#endif
			break;
		case 4:          /* single shot */
			if ( isitopen == 0 ) {
				warning("No output file opened!");
				break;
			}
			dummy = 2;
#ifdef D0FLAVOR
			fevout_(&dummy,&mode,&length,fname);
#else
			fevout(&dummy,&mode,&length,fname);
#endif
			break;
		case 5:         /* set flag for all events */
			if ( isitopen == 0 ) {
				warning("No output file opened!");
				break;
			}
			dummy = 3;
#ifdef D0FLAVOR
			fevout_(&dummy,&mode,&length,fname);
#else
			fevout(&dummy,&mode,&length,fname);
#endif
			break;
		case 6:         /* setup list of banks to keep/drop */
			list = XmTextGetString(listbull);
			length = strlen(list);
			if ( length < 1 ) {
				warning("No bank list specified");
				break;
			}
#ifdef D0FLAVOR
			setlist_(&length,list,&dork);
#else
			setlist(&length,list,&dork);
#endif
			break;
		case 7:         /* cancel setup list of banks to keep */
#ifdef D0FLAVOR
			cllist_(&length,list);
#else
			cllist(&length,list);
#endif
			XmTextSetString(listbull,"");
			break;
		case 8:            /* close file */
			if ( isitopen == 1 ) {
			  length = 0;
			  isitopen = 0;
#ifdef D0FLAVOR
			  fevout_(&isitopen,&mode,&length,fname);
#else
			  fevout(&isitopen,&mode,&length,fname);
#endif
			}
			break;
		case 9:         /* drop from list of events */
			dummy = 4;
#ifdef D0FLAVOR
			fevout_(&dummy,&mode,&length,fname);
#else
			fevout(&dummy,&mode,&length,fname);
#endif
			break;
		case 10:        /* select keep */
			dork = KEEP;
			break;
		case 11:        /* select drop */
			dork = DROP;
			break;
		default:
			break;
	}

}
