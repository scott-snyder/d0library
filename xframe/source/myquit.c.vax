/* 
        myquit.c
         Created           : 26-JAN-1993 by Drew Baden
*/
 
#include <stdio.h>                   /* I/O definitions                       */
 
myquit(w,tag,reason)
int *w, *tag;
unsigned long *reason;
{
	int select = 8;
/*
  close output file if opened
*/
	evout(w,&select,reason);
/*
  and exit
*/
	exit(0);	
}
