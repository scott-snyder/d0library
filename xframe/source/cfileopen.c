/*
        cfileopen.c
         Created           : 26-AUG-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "/d0lib/scratch/xframe/source/d0x_c.h"

Widget file_browse;
/*---------------------------------------------------------------------
  open files (0=open, 1=close)
----------------------------------------------------------------------*/
fileopen(w,tag,reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
	char *filename, ch[100], cf[100];
	int select = *tag;
	int type, mode = 0, n;
	Arg wargs[10];
	extern void open_file();
	extern void nomore();
	Widget XmCreateFileSelectionDialog();

	switch (select) {
		case 0:                 /* open file */
 		  filename = XmTextGetString(file_text);
 		  strcpy(cf,filename);
#ifdef D0FLAVOR
 		  xfopen_(cf,ch);
 		  ffiletype_(&mode,&type);
#else
 		  xfopen(cf,ch);
 		  ffiletype(&mode,&type);
#endif
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
  		case 1:  		        /* close file */
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
		  	XtSetArg(wargs[n], XmNdialogTitle, 
		  		XmStringCreateSimple("File Browser")); n++;
		  	file_browse = XmCreateFileSelectionDialog(toplevel_widget,
		  	    "filesb",wargs,n);
		  	XtAddCallback(file_browse, XmNcancelCallback, nomore, NULL);
		  	XtAddCallback(file_browse, XmNokCallback, open_file, NULL);
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
Widget		w;
XtPointer tag;
XmFileSelectionBoxCallbackStruct *reason;
{
	char *filename, ch[100], cf[100];
	int type, mode = 0;
	
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
	XmTextSetString(file_text,filename);	
/*
    ok, unmanage the file widget and open the file
*/
	XtUnmanageChild(file_browse);	
	strcpy(cf,filename);	
	XtFree(filename);
#ifdef D0FLAVOR
	xfopen_(cf,ch);
	ffiletype_(&mode,&type);
#else
	xfopen(cf,ch);
	ffiletype(&mode,&type);
#endif
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
Widget		w;
XtPointer tag;
XmFileSelectionBoxCallbackStruct *reason;
{
		XtUnmanageChild(file_browse);	
}
