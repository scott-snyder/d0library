/*
        rawstat.c
         Created           :  4-OCT-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "/d0lib/scratch/xframe/source/d0x_c.h"

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
