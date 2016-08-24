/*
        rawstat.c
         Created           :  4-OCT-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "xframe/source/d0x_c.h"

void
#ifdef D0FLAVOR
rawstat_(str)
#else
rawstat(str)
#endif
char *str;
{
	char ch[200];

	strcpy(ch,str);
	SetLabel(raw_status,ch);
}
