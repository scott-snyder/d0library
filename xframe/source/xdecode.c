/*
        xdecode.c
         Created           :  4-OCT-1992 by Drew Baden

tag = 0: go from hex to decimal
      1: go from decimal to hex
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "xframe/source/d0x_c.h"

void xdecode(w,tag,reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
	int value, andor, vres;
	int select = *tag;
	char *str;
	char source[20], result[20];

	str = XmTextGetString(decode_text);
	strcpy(source,str);
	XtFree(str);
	if ( strlen(source) <1 ) return;
	cupcase(source);

	switch (select) {

		case 0:		/* hex to decimal conversion */
			sscanf(source,"%X",&value);
			sprintf(result,"%u",value);
			XmTextSetString(decode_text,result);
			break;
		case 1:     /* decimal to hex conversion */
			value = atoi(source);
			sprintf(result,"%X",value);
			XmTextSetString(decode_text,result);
			break;
		case 2:     /* do an and for joey */
			sscanf(source,"%X",&value);  /* get the source*/
			str = XmTextGetString(andortext);
			strcpy(source,str);
			XtFree(str);
			if ( strlen(source) <1 ) return;
			cupcase(source);
			sscanf(source,"%X",&andor);
			vres = value & andor;
			sprintf(result,"%X",vres);
			XmTextSetString(andorresult,result);
			break;
		case 3:     /* do an or for joey */
			sscanf(source,"%X",&value);  /* get the source*/
			str = XmTextGetString(andortext);
			strcpy(source,str);
			XtFree(str);
			if ( strlen(source) <1 ) return;
			cupcase(source);
			sscanf(source,"%X",&andor);
			vres = value | andor;
			sprintf(result,"%X",vres);
			XmTextSetString(andorresult,result);
			break;
		default:
			printf("Illegal tag in XDECODE!!!\n");
			return;
		}
}
