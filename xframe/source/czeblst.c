/*
        czeblst.c
         Created           : 26-AUG-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "xframe/source/d0x_c.h"
extern Widget zebra_tree;
/*---------------------------------------------------------------------
  sets d0$zeblst (or unix equivalent) or en(dis)ables display
----------------------------------------------------------------------*/
void czeblst(w,tag,reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
	char *str, *bank, dbk[4];
	int dum, what, select = *tag, length;
	Arg arglist[20];
	int tval;
	int narg;
	int state;


	switch (select) {
		case 0:          /* sets d0$zeblst string */
	        str = XmTextGetString(xdbank_text_zeblst);
	        length = strlen(str);
	        /* pass this along to the common */
#ifdef D0FLAVOR
		    fzeblst_(str,&length);
#else
			fzeblst(str,&length);
#endif
			XtFree(str);
/*
  			now, call the fd0util routine with tag=1 for getzeblst
*/
			bank = XmTextGetString(xdbank_bank_2);
			dum = -1;
#ifdef D0FLAVOR
			fd0util_(&dum,&dum,bank);
#else
			fd0util(&dum,&dum,bank);
#endif
			XtFree(bank);
			break;

		case 1:         /* enable/disable .zeb*/
/*
  		    get current state
*/
	        narg = 0;
	        what = 0;
#ifdef D0FLAVOR
	        zebstate_(&what,&state);   /* 0,state = fetch, 1=set */
#else
	        zebstate(&what,&state);   /* 0,state = fetch, 1=set */
#endif
	        what = 1;
	        switch (state) {
	        	case 0:          /* it's off - turn on, change state */
	        	   dum = 1;
	        	   tval = True;
#ifdef D0FLAVOR
	               zebstate_(&what,&dum);   /* 0 = fetch, 1=set */
#else
	               zebstate(&what,&dum);   /* 0 = fetch, 1=set */
#endif
/*
                   and get the .zeb file
*/
				   bank = XmTextGetString(xdbank_bank_2);
				   dum = -1;
#ifdef D0FLAVOR
		   		   fd0util_(&dum,&dum,bank);
#else
				   fd0util(&dum,&dum,bank);
#endif
				   XtFree(bank);
				   XmTextSetString(navigate_text,"");
				   XtSetArg(arglist[narg],XmNlabelString,
				   	 XmStringCreateSimple("Enabled")); narg++;
				   XtSetValues(w, arglist, narg);
	        	   break;
	        	case 1:          /* it's on - turn off, change state */
	        	   dum = 0;
	        	   tval = False;
#ifdef D0FLAVOR
	               zebstate_(&what,&dum);   /* 0 = fetch, 1=set */
#else
	               zebstate(&what,&dum);   /* 0 = fetch, 1=set */
#endif
				   XtSetArg(arglist[narg],XmNlabelString,
				   	 XmStringCreateSimple("Disabled")); narg++;
				   XtSetValues(w, arglist, narg);
	        	   break;
	        	}
	        break;
		case 2:         /* set to full tree*/
	        what = 1;
	        dum = 1;
#ifdef D0FLAVOR
			treestate_(&what,&dum);   /* set state to 1 */
#else
			treestate(&what,&dum);   /* set state to 1 */
#endif
/*
  			reset list of banks for partial
*/
			dum = 0;
#ifdef D0FLAVOR
			fblist_(&dum,dbk,&dum);
#else
			fblist(&dum,dbk,&dum);
#endif
/*
    		and put up the tree
*/
			XtUnmanageChild(zebra_tree);
			bank = XmTextGetString(xdbank_bank_2);
			dum = -2;
#ifdef D0FLAVOR
			fd0util_(&dum,&dum,bank);
#else
			fd0util(&dum,&dum,bank);
#endif
			XtFree(bank);
			XtManageChild(zebra_tree);
			break;
		case 3:         /* enable/disable data*/
/*
  		    get current state
*/
	        narg = 0;
	        what = 0;
#ifdef D0FLAVOR
	        datastate_(&what,&state);   /* 0 = fetch, 1=set */
#else
	        datastate(&what,&state);   /* 0 = fetch, 1=set */
#endif
	        what = 1;
	        switch (state) {
	        	case 0:          /* it's off - turn on, change state,
	        	                    change label, fetch new info */
	        	   dum = 1;
	        	   tval = True;
#ifdef D0FLAVOR
	               datastate_(&what,&dum);   /* 0 = fetch, 1=set */
#else
	               datastate(&what,&dum);   /* 0 = fetch, 1=set */
#endif
/*
                   and get the data
*/
				   bank = XmTextGetString(xdbank_bank_2);
				   dum = 0;
#ifdef D0FLAVOR
		   		   fd0util_(&dum,&dum,bank);
#else
				   fd0util(&dum,&dum,bank);
#endif
				   XtFree(bank);
	        	   break;
	        	case 1:          /* it's on - turn off, change state,
	        	                    change label */
	        	   dum = 0;
	        	   tval = False;
#ifdef D0FLAVOR
	               datastate_(&what,&dum);   /* 0 = fetch, 1=set */
#else
                   datastate(&what,&dum);   /* 0 = fetch, 1=set */
#endif
	        	   break;
	        	}
	        break;
		case 4:         /* set to partial tree*/
	        what = 1;
	        dum = 0;
#ifdef D0FLAVOR
			treestate_(&what,&dum);   /* set state to 0 */
#else
			treestate(&what,&dum);   /* set state to 0 */
#endif
/*
    		and put up the tree
*/
			XtUnmanageChild(zebra_tree);
			bank = XmTextGetString(xdbank_bank_2);
			dum = -2;
#ifdef D0FLAVOR
			fd0util_(&dum,&dum,bank);
#else
			fd0util(&dum,&dum,bank);
#endif
			XtFree(bank);
			XtManageChild(zebra_tree);
			break;
	    default:
	    	break;
		}
}
