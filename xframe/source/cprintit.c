/*
        cprintit.c
         Created           : 20-OCT-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */
#include "/d0library/scratch/test/xframe/source/d0x_c.h"

extern int the_address;
FILE *fipo;
void cprintit(w,tag,reason)  /* prints out from widgets - xdbank and raw */
Widget		w;
int		*tag;
unsigned long	*reason;
{
	int select = *tag, dum, status;
	char *str, filename[100], which[30];

	switch (select) {
		case 0:    /* print from xdbank */
			xgetchar("Name of file to create: "," ",filename,&status);
			fipo = fopen(filename,"w");
			str = XmTextGetString(xdbank_bank_2);
		    fprintf(fipo,"\nData for bank: '%s'",str);
		    XtFree(str);
		    str = XmTextGetString(xdbank_text_bottom);
		    fprintf(fipo,"%s",str);
		    XtFree(str);
		    fclose(fipo);
			break;
		case 1:    /* print from raw browser */
			xgetchar("Name of file to create: "," ",filename,&status);
			fipo = fopen(filename,"w");
			str = XmTextGetString(raw_header);
			fprintf(fipo,"%s",str);
		    XtFree(str);
			str = XmTextGetString(raw_trailer);
			fprintf(fipo,"%s",str);
		    XtFree(str);
			fprintf(fipo,"Data:\n");
			str = XmTextGetString(raw_text);
			fprintf(fipo,"%s",str);
		    XtFree(str);
		    fclose(fipo);
			break;
		case 2:    /* print from xdbank but do prxxxx */
			status = 8;
			str = XmTextGetString(xdbank_bank_2);
#ifdef D0FLAVOR
			fd0util_(&status,&the_address,str);
#else
			fd0util(&status,&the_address,str);
#endif
			break;
		default:
			break;
		}

}
