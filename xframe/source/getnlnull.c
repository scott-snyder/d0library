/* 
        getnlnull.c
         Created           : 26-AUG-1992 by Drew Baden
*/
 
#include <stdio.h>                   /* I/O definitions                       */
 
/*---------------------------------------------------------------------
  utility to guarantee machine independence of \nl and \0
----------------------------------------------------------------------*/
#ifdef D0FLAVOR
getnlnull_(chnl,chnull)	
#else
getnlnull(chnl,chnull)
#endif
char chnl[],chnull[];
{
	chnl[0] = '\n';
	chnull[0] = '\0';
}
