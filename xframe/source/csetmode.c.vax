/* 
        csetmode.c
         Created           : 26-AUG-1992 by Drew Baden
*/
 
#include <stdio.h>                   /* I/O definitions                       */
 
/*---------------------------------------------------------------------
  sets mode (0=exchange, 1=native) - not really needed (D0OPEN)
----------------------------------------------------------------------*/
csetmode(w,tag,reason)  
int w;
int		*tag;
unsigned long	*reason;
{
	int select = *tag;
	
#ifdef D0FLAVOR
	fmode_(&select);
#else
	fmode(&select);
#endif
  	return;
}
