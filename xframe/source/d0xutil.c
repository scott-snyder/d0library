/* 
        TMP$ROOT232:[DREW_TMP.GAMMA.XFRAME.SOURCE]D0XUTIL.C;
         Created           : 30-JUN-1993 by Drew Baden
*/
 
#include <stdio.h>                   /* I/O definitions                       */
#include "xframe/source/d0x_c.h" 
/*
  some utilities...
*/

/*
  xmtextsetstring from fortran
*/
#ifdef D0FLAVOR
fxmtss_(w,string)
#else
fxmtss(w,string)
#endif
Widget w;
char *string;
{
	XmTextSetString(w,string);
}
