/*
        cd0util.c
         Created           : 26-AUG-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "/d0library/scratch/test/xframe/source/d0x_c.h"

extern Widget zebra_tree;
/* int treestate = 0;    0=text, 1=tree */
char bank[10] = "HEAD";
/*---------------------------------------------------------------------
  d0util menu -
  0=xdbank, 1=xdaddr, 2=prbank, 3=ezbank, 4=dzform, 5=dbank, 6=daddr
  7=caph stuff 8=toggle tree
----------------------------------------------------------------------*/
void cd0util(w,tag,reason)
Widget w;
int *tag;
unsigned long *reason;
{
    int addr, select = *tag, dum, linear, length, status;
    char *str, bank2[10];

    switch (select) {
        case -1:      /* navigate via bank in navigate bank text window */
        case 0:       /* navigate from NAVIGATE button or etc. (start) */
          dum = 0;
#ifdef D0FLAVOR
          fsetoff_(&dum);   /* set offset (navigation) to zero */
#else
          fsetoff(&dum);
#endif
/*
          use HEAD to start if pushed from main menu
*/
          if ( select == -1 ) {
             str = XmTextGetString(xdbank_bank_2);
             strcpy(bank,str);
             if ( strlen(bank) != 4 ) return;
             strcpy(bank2,bank);
             cupcase(bank2);
             XmTextSetString(xdbank_bank_2,bank2);
             }
          else {
              strcpy(bank,"HEAD");
              XmTextSetString(xdbank_bank_2,"HEAD");
          }
          if ( bank[0] == '\0' ) {
            strcpy(bank,"HEAD");
            XmTextSetString(xdbank_bank_2,"HEAD");
          }
/*
          set the cursor to "watch"
*/
          SetWatchCursor(nav_main);
          SetWatchCursor(xdbank_main);
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
          XmTextSetString(navigate_text," ");
          dum = 0;
          cfd0util(&dum,&dum);
/*
          tag = -2, make tree widget 
*/
          dum = -2;
          if (select == 0)
          cfd0util(&dum,&dum);
          XtManageChild(zebra_tree);
/*
          tag = -1, put .zeb into window

          first, get d0$zeblst
*/
          str = XmTextGetString(xdbank_text_zeblst);
          length = strlen(str);
/*
          and pass this along to the common
*/
#ifdef D0FLAVOR
          fzeblst_(str,&length);
#else
          fzeblst(str,&length);
#endif
/*
          call fd0util to put up the .zeb
*/
          dum = -1;
          cfd0util(&dum,&dum);
          XtFree(str);
/*
          reset the cursor from "watch"
*/
          SetDefaultCursor(nav_main);
          SetDefaultCursor(xdbank_main);
          break;
        case 1:              /* xdaddr OBSOLETE*/
          break;
        case 2:              /* prbank to screen by name */
          xgetchar("Bank name to dump (4 letters only):",bank,bank,&status);
          cfd0util(&select,&addr);
          break;
        case 3:                /* ezbank */
          cfd0util(&select,&addr);
          break;
        case 4:            /* mzform */
          xgetchar("Bank name to check (4 letters only):",bank,bank,&status);
          cfd0util(&select,&addr);
          break;
        case 5:            /* dbank */
          cfd0util(&select,&addr);
          break;
        case 6:            /* dbaddr */
          cfd0util(&select,&addr);
          break;
        case 7:            /* caph stuff */
          cfd0util(&select,&addr);
          break;
        case 8:              /* prbank to file by name */
          xgetchar("Bank name to dump (4 letters only):",bank,bank,&status);
          cfd0util(&select,&addr);
          break;
        case 9:        /* leaving navigator, reset */
          XtDestroyWidget(zebra_tree);
          break;
        case 10:              /* prbank to screen by addr */
          xgetchar("Bank address:",bank,bank,&status);
          addr = atoi(bank);
          xgetchar("Bank name to dump (4 letters only):",bank,bank,&status);
          cfd0util(&select,&addr);
          break;
        case 11:              /* prbank to file by addr */
          xgetchar("Bank address:",bank,bank,&status);
          addr = atoi(bank);
          xgetchar("Bank name to dump (4 letters only):",bank,bank,&status);
          cfd0util(&select,&addr);
          break;
        default:
          XmTextSetString(navigate_text,
              "***Illegal tag presented 'cd0util'***");
          return;
        }
      return;
}

void cfd0util(int* i,int* j)
{
#ifdef D0FLAVOR
    fd0util_(i,j,bank);
#else
    fd0util(i,j,bank);
#endif
}
