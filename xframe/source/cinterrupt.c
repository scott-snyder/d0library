/* 
        cinterrupt.c
         Created           : 26-AUG-1992 by Drew Baden
*/
 
#include <stdio.h>                   /* I/O definitions                       */
#ifdef D0FLAVOR
void finterrupt_(int*);
#else
void finterrupt(int*);
#endif
/*---------------------------------------------------------------------
  pushed to interrupt SCAN, SKIP, SEARCH 
----------------------------------------------------------------------*/
void
cinterrupt(w,tag,reason)  
int w;
int		*tag;
unsigned long	*reason;
{
#ifdef D0FLAVOR
	finterrupt_(tag);
#else
	finterrupt(tag);
#endif
}
