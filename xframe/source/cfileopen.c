/*
        cfileopen.c
         Created           : 26-AUG-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "xframe/source/d0x_c.h"

Widget file_browse;
/*---------------------------------------------------------------------
  open files (0=open, 1=close)
----------------------------------------------------------------------*/
void fileopen(w,tag,reason)
Widget w;
int *tag;
unsigned long *reason;
{
    char *filename, ch[400], cf[400];
    int select = *tag;
    int type, mode = 0, n, lfile, tfile;
    Arg wargs[10];
    extern void open_file();
    extern void nomore();
    Widget XmCreateFileSelectionDialog();
    XmString tmp;
    
    switch (select) {
        case 0:                 /* open file - OBSOLETE*/
           filename = XmTextGetString(file_text);
           strcpy(cf,filename);
           lfile = strlen(cf);
#ifdef D0FLAVOR
           xfopen_(cf,&lfile,ch,&tfile);
           ffiletype_(&mode,&type);
#else
           xfopen(cf,&lfile,ch,&tfile);
           ffiletype(&mode,&type);
#endif
          ch[tfile] = '\0';
          squeeze(ch,' ');
          switch (type) {
              case 0:
                SetLabel(fz_label,ch);
                break;
              case 1:
                SetLabel(stp_label,ch);
                break;
              case 2:
                SetLabel(output_label,ch);
                break;
            }
          XtFree(filename);
           break;
          case 1:                  /* close file */
#ifdef D0FLAVOR
            xfclose_();
           ffiletype_(&mode,&type);
#else
            xfclose();
           ffiletype(&mode,&type);
#endif
          switch (type) {
              case 0:
                SetLabel(fz_label,"NO FZ File Open");
                break;
              case 1:
                SetLabel(stp_label,"NO STP File Open");
                break;
              case 2:
                SetLabel(output_label,"NO OUTPUT File Open");
                break;
            }
          break;
        case 2:                /* make a file browser widget */
          if ( file_browse == 0 ) {
              n = 0;
              tmp = XmStringCreateSimple("File Browser");
              XtSetArg(wargs[n], XmNdialogTitle, tmp); n++;
              file_browse = XmCreateFileSelectionDialog(toplevel_widget,
                  "filesb",wargs,n);
              XtAddCallback(file_browse, XmNcancelCallback, nomore, NULL);
              XtAddCallback(file_browse, XmNokCallback, open_file, NULL);
              XmStringFree(tmp);
          }
          XtManageChild(file_browse);
          break;
        default:
          printf(" ***Illegal tag presented 'open files'***\n");
        }
      return;
}

void
open_file(w,tag,reason)
Widget        w;
XtPointer tag;
XmFileSelectionBoxCallbackStruct *reason;
{
    char *filename, ch[400], cf[400];
    int type, mode = 0, lfile, tfile;
    
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
        XtUnmanageChild(file_browse);    
    }
/*    XmTextSetString(file_text,filename);    

    ok, set cursor to "wait", and open the file
*/
    SetWatchCursor(file_browse);
    strcpy(cf,filename);    
    XtFree(filename);
    lfile = strlen(cf);
#ifdef D0FLAVOR
    xfopen_(cf,&lfile,ch,&tfile);
    ffiletype_(&mode,&type);
#else
    xfopen(cf,&lfile,ch,&tfile);
    ffiletype(&mode,&type);
#endif
	SetDefaultCursor(file_browse);
    XtUnmanageChild(file_browse);    
    ch[tfile] = '\0';
    squeeze(ch,' ');
    switch (type) {
        case 0:
            SetLabel(fz_label,ch);
            break;
        case 1:
            SetLabel(stp_label,ch);
            break;
        case 2:
            SetLabel(output_label,ch);
            break;
        default:
            break;
    }
        
}

void
nomore(w,tag,reason)
Widget        w;
XtPointer tag;
XmFileSelectionBoxCallbackStruct *reason;
{
        XtUnmanageChild(file_browse);    
}

void
#ifdef D0FLAVOR
solabel_(count)
#else
solabel(count)
#endif
int *count;
{
    char label[10];
    sprintf(label,"%d",*count);
    SetLabel(ocount,label);
}
