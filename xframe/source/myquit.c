/* 
        myquit.c
         Created           : 26-JAN-1993 by Drew Baden
*/
 
#include <stdio.h>                   /* I/O definitions                       */
#include "/d0library/scratch/test/xframe/source/d0x_c.h"

void myquit(w,tag,reason)
Widget w;
int *tag;
unsigned long *reason;
{
	int select = 8, dum = 0;
/*
  close output file if opened
*/
	evout(w,&select,reason);
/*
  call d0xuser_finish routine
*/
#ifdef D0FLAVOR
	select = d0xuser_finish_(&dum);
#else
	select = d0xuser_finish(&dum);
#endif
/*
  and exit
*/
	exit(0);	
}
