/*
        cxerrmsg.c
         Created           : 17-SEP-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */

void
#ifdef D0FLAVOR
cxerrmsg_(text)
#else
cxerrmsg(text)  /* used to deal with the fortran vax/unix dreaded underscore */
#endif
char *text;
{
        void cerrmsg(char*);
        
        cerrmsg(text);
}
