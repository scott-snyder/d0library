/*
        fbranch.c
         Created           : 28-OCT-1992 by Drew Baden

   	    starts the cbranch chain
*/

#include <stdio.h>                   /* I/O definitions                       */
#include "/d0library/scratch/test/xframe/source/d0x_c.h"

extern int the_address;
#ifdef D0FLAVOR
fbranch_(parent,store,link,iq,lq,zp)
#else
fbranch(parent,store,link,iq,lq,zp)
#endif
Widget parent;
int iq[], lq[], *zp, *store, link;
{
	the_address = *zp;
	cbranch(parent,store,link,iq,lq,zp);
}
