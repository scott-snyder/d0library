/* 
        csetout.c
         Created           : 25-JAN-1993 by Drew Baden
*/
 
#include <stdio.h>                   /* I/O definitions                       */
#include "xframe/source/d0x_c.h"
 
void csetout(w,tag,reason)   
Widget		w;
int		*tag;
unsigned long	*reason;
{
	
#ifdef D0FLAVOR
	fsetout_(w,tag,reason);
#else
	fsetout(w,tag,reason);
#endif

}
