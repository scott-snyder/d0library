/*
        cd0util.c
         Created           : 26-AUG-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "/d0lib/scratch/xframe/source/d0x_c.h"

extern Widget zebra_tree;
int treestate = 0;    /* 0=text, 1=tree */
/*---------------------------------------------------------------------
  d0util menu -
  0=xdbank, 1=xdaddr, 2=prbank, 3=ezbank, 4=dzform, 5=dbank, 6=daddr
  7=caph stuff 8=toggle tree
----------------------------------------------------------------------*/
cd0util(w,tag,reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
	int addr;
	int select = *tag;
	int dum, linear;
	char *bank, *str, bank2[10];

	switch (select) {
		case -2:      /* cr hit inside "map" window - see if we have
		                 selected a 4-letter bank name */
/*
    obsolete
*/
          printf("Illegal tag=2 cd0util (tell Drew)\n");
          break;
		case -1:      /* navigate via bank in navigate bank text window */
		case 0:       /* navigate via bank in main text window */
		  dum = 0;
#ifdef D0FLAVOR
		  fsetoff_(&dum);   /* set offset (navigation) to zero */
#else
		  fsetoff(&dum);
#endif
/*
          get bank name - if blank, use HEAD
*/
	      if ( select == -2 ) {
	      	bank = XmTextGetSelection(xdbank_text_map);
	      	if ( strlen(bank) != 4 ) return;
	      	strcpy(bank2,bank);
         	cupcase(bank2);
         	XmTextSetString(xdbank_bank_2,bank2);
         	XmTextSetString(bank_text,bank2);
	      	}
		  else if ( select == -1 ) {
		  	bank = XmTextGetString(xdbank_bank_2);
	      	if ( strlen(bank) != 4 ) return;
	      	strcpy(bank2,bank);
         	cupcase(bank2);
         	XmTextSetString(bank_text,bank2);
         	XmTextSetString(xdbank_bank_2,bank2);
		  	}
		  else {
		    bank = XmTextGetString(bank_text);
		  }
	      if ( bank[0] == '\0' )
	         {
	         	bank = "HEAD";
	         	XmTextSetString(bank_text,"HEAD");
	         	XmTextSetString(xdbank_bank_2,"HEAD");
	         }
/*
  		  tag = 0, put data into window

  		  first get the linear chain number
*/
		  str = XmTextGetString(xdbank_text_chain);
		  linear = atoi(str);
/*
  	      and pass it along into common
*/
#ifdef D0FLAVOR
		  fsetlin_(&linear);
#else
		  fsetlin(&linear);
#endif
/*
          put up the text of data (ala dbank)
*/
	      XmTextSetString(navigate_text,"....working.....");
		  dum = 0;
#ifdef D0FLAVOR
 		  fd0util_(&dum,&dum,bank);
#else
 		  fd0util(&dum,&dum,bank);
#endif
/*
  		  tag = -2, make tree widget 
*/
		  dum = -2;
		  if (select == 0)
#ifdef D0FLAVOR
 		  fd0util_(&dum,&dum,bank);
#else
 		  fd0util(&dum,&dum,bank);
#endif
		  XtManageChild(zebra_tree);
/*
  		  tag = -1, put .zeb into window

  	      first, get d0$zeblst
*/
	      str = XmTextGetString(xdbank_text_zeblst);
/*
  		  and pass this along to the common
*/
#ifdef D0FLAVOR
		  fzeblst_(str);
#else
		  fzeblst(str);
#endif
/*
  		  call fd0util to put up the .zeb
*/
  		  dum = -1;
#ifdef D0FLAVOR
 		  fd0util_(&dum,&dum,bank);
#else
 		  fd0util(&dum,&dum,bank);
#endif
		  XtFree(bank);
		  XtFree(str);
		  break;
  		case 1:  			/* xdaddr */
 		  bank = XmTextGetString(bank_text);
 		  addr = atoi(bank);
#ifdef D0FLAVOR
 		  fd0util_(&select,&addr,&addr);
#else
 		  fd0util(&select,&addr,&addr);
#endif
		  XtFree(bank);
  		  break;
  		case 2:  			/* prbank */
 		  bank = XmTextGetString(bank_text);
#ifdef D0FLAVOR
 		  fd0util_(&select,&addr,bank);
#else
 		  fd0util(&select,&addr,bank);
#endif
		  XtFree(bank);
  		  break;
  		case 3:				/* ezbank */
#ifdef D0FLAVOR
  		  fd0util_(&select,&addr,bank);
#else
  		  fd0util(&select,&addr,bank);
#endif
  		  break;
  		case 4:            /* mzform */
 		  bank = XmTextGetString(bank_text);
#ifdef D0FLAVOR
  		  fd0util_(&select,&addr,bank);
#else
  		  fd0util(&select,&addr,bank);
#endif
		  XtFree(bank);
 		  break;
  		case 5:            /* dbank */
#ifdef D0FLAVOR
  		  fd0util_(&select,&addr,bank);
#else
  		  fd0util(&select,&addr,bank);
#endif
 		  break;
  		case 6:            /* dbaddr */
#ifdef D0FLAVOR
  		  fd0util_(&select,&addr,bank);
#else
  		  fd0util(&select,&addr,bank);
#endif
 		  break;
  		case 7:            /* caph stuff */
#ifdef D0FLAVOR
  		  fd0util_(&select,&addr,bank);
#else
  		  fd0util(&select,&addr,bank);
#endif
 		  break;
		case 8:      /* swap tree<-->text OBSOLETE */
		  printf("Obsolete tag 8 in CD0UTIL - tell DREW\n");
		  break;
		case 9:        /* leaving navigator, reset */
			XtDestroyWidget(zebra_tree);
			break;
  	    default:
	      XmTextSetString(navigate_text,
	      	"***Illegal tag presented 'cd0util'***");
		  return;
		}
  	return;
}
