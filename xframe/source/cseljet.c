/* 
        cseljet.c
         Created           : 25-JAN-1993 by Drew Baden
*/
 
#include <stdio.h>                   /* I/O definitions                       */
#include "/d0library/scratch/test/xframe/source/d0x_c.h"
 
void cseljet(w,tag,reason)   
Widget		w;
int		*tag;
unsigned long	*reason;
{
	
#ifdef D0FLAVOR
	seljet_(w,tag,reason);
#else
	seljet(w,tag,reason);
#endif

}

