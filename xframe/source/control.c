/*
        control.c
         Created           : 26-AUG-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "xframe/source/d0x_c.h"
Widget d0dad_browse;
char skiptext[100] = "1";
char numbit[10] = "0";
int d0xuserarg = 0;
/*---------------------------------------------------------------------
  control menu - 0=next, 1=scan, 2=skip
----------------------------------------------------------------------*/
void control(w,tag,reason)
Widget        w;
int        *tag;
unsigned long    *reason;
{
    int select = *tag, ntrue, nfalse, n;
    int dum,skip,status, drun,dev;
    char ch[200], *rstr, *estr;
    float ntot,xtrue,xfalse,dxtrue,dxfalse;
    double ft;
    XmString tmp, tmp2;
    extern void open_catalog();
    extern void dnomore();
    Widget XmCreateFileSelectionDialog();
    Arg wargs[10];
    
    switch (select) {
        case 0:                /* next */
          SetWatchCursor(bulletin_main);
          cfcontrol(&select,&skip,&skip);
          SetDefaultCursor(bulletin_main);
          break;
        case 1:              /* scan (read all) */
          cfcontrol(&select,&skip,&skip);
          break;
        case 2:                /* skip */
          xgetchar("How many records to skip:",skiptext,skiptext,&status);
          skip = atoi(skiptext);
          cfcontrol(&select,&skip,&skip);
          break;
        case 3:             /* call d0xuser event entry */
          xgetchar("Integer argument:",numbit,numbit,&status);
          sscanf(numbit,"%d",&d0xuserarg);
#ifdef D0FLAVOR
          status = d0xuser_(&d0xuserarg);
#else
          status = d0xuser(&d0xuserarg);
#endif
          return;
        case 8:             /* call d0xuser init entry */
          xgetchar("Integer argument:",numbit,numbit,&status);
          sscanf(numbit,"%d",&d0xuserarg);
#ifdef D0FLAVOR
          status = d0xuser_init_(&d0xuserarg);
#else
          status = d0xuser_init(&d0xuserarg);
#endif
          return;
        case 9:             /* call d0xuser finish entry */
          xgetchar("Integer argument:",numbit,numbit,&status);
          sscanf(numbit,"%d",&d0xuserarg);
#ifdef D0FLAVOR
          status = d0xuser_finish_(&d0xuserarg);
#else
          status = d0xuser_finish(&d0xuserarg);
#endif
          return;
        case 10:             /* call d0xuser talk entry */
          xgetchar("Integer argument:",numbit,numbit,&status);
          sscanf(numbit,"%d",&d0xuserarg);
#ifdef D0FLAVOR
          status = d0xuser_talk_(&d0xuserarg);
#else
          status = d0xuser_talk(&d0xuserarg);
#endif
          return;
        case 4:             /* enable d0xuser */
          skip = 4;
          cfcontrol(&skip,&skip,&skip);
          break;
        case 5:             /* disable d0xuser */
          skip = 3;
          cfcontrol(&skip,&skip,&skip);
          break;
        case 6:             /* clear statistics */
          skip = 5;
          cfcontrol(&skip,&ntrue,&nfalse);
          ch[0] = '\0';
          XmTextSetString(stattext,ch);
          break;
        case 7:             /* calculate statistics */
          skip = 6;
          cfcontrol(&skip,&ntrue,&nfalse);
          if ( (ntrue+nfalse) == 0 ) {
              strcpy(ch,"Not Available");
          }
          else {
              ntot = ntrue + nfalse;
              xtrue = ntrue;
              xtrue = xtrue/ntot;
              xfalse = nfalse;
              xfalse = xfalse/ntot;
              dxtrue = xtrue*(1-xtrue)/ntot;
              ft = dxtrue;
              ft = sqrt(ft);
              dxtrue = ft;
              dxfalse = dxtrue;
              sprintf(ch,
              "True:  %6d  %.3f+- %.3f\nFalse: %6d  %.3f+- %.3f\nTotal: %6d",
                  ntrue,xtrue,dxtrue,nfalse,xfalse,dxfalse,ntrue+nfalse);
          }
          XmTextSetString(stattext,ch);
          break;
        case 11:   /* enable micro-dst conversion */
          skip = 7;
          cfcontrol(&skip,&skip,&skip);
          break;
        case 12:   /* disable micro-dst conversion */
          skip = 8;
          cfcontrol(&skip,&skip,&skip);
          break;
        case 13:   /* enable cafix */
          skip = 9;
          cfcontrol(&skip,&skip,&skip);
          break;
        case 14:   /* disable cafix */
          skip = 10;
          cfcontrol(&skip,&skip,&skip);
        case 15:   /* enable compute_em_quality */
          skip = 11;
          cfcontrol(&skip,&skip,&skip);
          break;
        case 16:   /* disable compute_em_quality */
          skip = 12;
          cfcontrol(&skip,&skip,&skip);
          break;
        case 17:   /* enable compute_mu_quality */
          skip = 13;
          cfcontrol(&skip,&skip,&skip);
          break;
        case 18:   /* disable compute_mu_quality */
          skip = 14;
          cfcontrol(&skip,&skip,&skip);
          break;
        case 19:   /* enable cahits */
          skip = 15;
          cfcontrol(&skip,&skip,&skip);
          break;
        case 20:   /* disable cahits */
          skip = 16;
          cfcontrol(&skip,&skip,&skip);
          break;
        case 30:   /* enable all */
          XtVaSetValues(encahits, XmNset, True, NULL);
          XtVaSetValues(discahits, XmNset, False, NULL);
          XtVaSetValues(enudst, XmNset, True, NULL);
          XtVaSetValues(disudst, XmNset, False, NULL);
          XtVaSetValues(encleanem, XmNset, True, NULL);
          XtVaSetValues(discleanem, XmNset, False, NULL);
          XtVaSetValues(encleanmu, XmNset, True, NULL);
          XtVaSetValues(discleanmu, XmNset, False, NULL);
          XtVaSetValues(encafix, XmNset, True, NULL);
          XtVaSetValues(discafix, XmNset, False, NULL);
          break;
        case 31:   /* disable all */
          XtVaSetValues(encahits, XmNset, False, NULL);
          XtVaSetValues(discahits, XmNset, True, NULL);
          XtVaSetValues(enudst, XmNset, False, NULL);
          XtVaSetValues(disudst, XmNset, True, NULL);
          XtVaSetValues(encleanem, XmNset, False, NULL);
          XtVaSetValues(discleanem, XmNset, True, NULL);
          XtVaSetValues(encleanmu, XmNset, False, NULL);
          XtVaSetValues(discleanmu, XmNset, True, NULL);
          XtVaSetValues(encafix, XmNset, False, NULL);
          XtVaSetValues(discafix, XmNset, True, NULL);
          break;
        case 50:    /* d0dad fetch */
          rstr = XmTextGetString(d0dadrun);
          if (strlen(rstr) < 1) {
             warning("RUN not entered - try again");
             return;
          }
          estr = XmTextGetString(d0dadevent);
          if (strlen(estr) < 1) {
             warning("EVENT not entered - try again");
             return;
          }
          drun = atoi(rstr);
          dev = atoi(estr);
          SetWatchCursor(d0dad_main);
#ifdef D0FLAVOR
          d0xd0dad_(&drun,&dev);
#else
          d0xd0dad(&drun,&dev);
#endif
          SetDefaultCursor(d0dad_main);
          XtFree(rstr);
          XtFree(estr);
          break;
        case 51:    /* open d0dad catalogs */
/*
          set the cursor to "watch"
*/
          SetWatchCursor(bulletin_main);
          if ( d0dad_browse == 0 ) {
             n = 0;
             XtSetArg(wargs[n],XmNdialogTitle, 
                XmStringCreateSimple("D0DAD Catalog Browser"));n++;
#ifdef D0FLAVOR
             tmp = XmStringCreateSimple("$d0dad__catalogs/");
#else
             tmp = XmStringCreateSimple("d0$d0dad$catalogs:");
#endif
             tmp2 = XmStringCreateSimple("*.evtcat");
             XtSetArg(wargs[n],XmNdirectory,tmp); n++;
             XtSetArg(wargs[n],XmNdirMask, tmp2); n++;
             d0dad_browse = XmCreateFileSelectionDialog(toplevel_widget,
                      "filesb",wargs,n);
             XtAddCallback(d0dad_browse, XmNcancelCallback, dnomore, NULL);
             XtAddCallback(d0dad_browse, XmNokCallback, open_catalog, NULL);
          }
/*
          reset the cursor from "watch" and manage browser
*/
          SetDefaultCursor(bulletin_main);
          XtManageChild(d0dad_browse);
          break;
        case 52:    /* return to default run-driven catalogs */
          XmTextSetString(d0dadcattext,
              "Using standard D0 run-dependent catalog...");
          dum = 1;
#ifdef D0FLAVOR
          setd0dad_(&dum,ch,&dum);
#else
          setd0dad(&dum,ch,&dum);
#endif
          break;
        default:
          printf(" ***Illegal tag presented 'control'***\n");
          return;
        }
}

void
dnomore(w,tag,reason)
Widget        w;
XtPointer tag;
XmFileSelectionBoxCallbackStruct *reason;
{
        XtUnmanageChild(d0dad_browse);    
}

    
void
open_catalog(w,tag,reason)
Widget        w;
XtPointer tag;
XmFileSelectionBoxCallbackStruct *reason;
{
    char *filename, ch[400], cf[400];
    int dum, len, type, mode = 0, lfile, tfile;
/*
    get file name
*/
    XmStringGetLtoR(reason->value, XmSTRING_DEFAULT_CHARSET, &filename);
/*
    check if user typed anything
*/
    if ( !*filename) {  /* nothing typed? */
        puts("No file selected.....");
        XtFree(filename);
        XtUnmanageChild(d0dad_browse);    
    }
/*    
    ok, unmanage the file widget and specify the catalog
*/
    XtUnmanageChild(d0dad_browse);    
    strcpy(cf,filename);    
    XtFree(filename);
    XmTextSetString(d0dadcattext,cf);
    len = strlen(cf);
    dum = 0;
#ifdef D0FLAVOR
    setd0dad_(&dum,cf,&len);
#else
    setd0dad(&dum,cf,&len);
#endif
}

void cfcontrol(i,j,k)
int *i, *j, *k;
{
#ifdef D0FLAVOR
    fcontrol_(i,j,k);
#else
    fcontrol(i,j,k);
#endif
}

