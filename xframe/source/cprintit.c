/*
        cprintit.c
         Created           : 20-OCT-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */
#include "/d0lib/scratch/xframe/source/d0x_c.h"

int iamopen = 0;
FILE *fpo, *fopen();
cprintit(w,tag,reason)  /* prints out from widgets - xdbank and raw */
Widget		w;
int		*tag;
unsigned long	*reason;
{
	int select = *tag, dum;
	char *str, filename[100], which[30];

	if ( iamopen == 0 ) {
		printf("Name of file to create: ");
		gets(filename);
  		fpo = fopen(filename,"w");
		iamopen = 1;
	}
	switch (select) {
		case 0:    /* print from xdbank */
			str = XmTextGetString(bank_text);
		    fprintf(fpo,"\nData for bank: '%s'",str);
		    XtFree(str);
		    str = XmTextGetString(xdbank_text_bottom);
		    fprintf(fpo,"%s",str);
		    XtFree(str);
			break;
		case 1:    /* print from raw browser */
			str = XmTextGetString(raw_header);
			fprintf(fpo,"%s",str);
		    XtFree(str);
			str = XmTextGetString(raw_trailer);
			fprintf(fpo,"%s",str);
		    XtFree(str);
			fprintf(fpo,"Data:\n");
			str = XmTextGetString(raw_text);
			fprintf(fpo,"%s",str);
		    XtFree(str);
			break;
		default:
			break;
		}

	printf("Close file? (Y/N,<CR>=Y) :");
	while (1) {
		gets(which);
		dum = NULL;
		switch (which[0]) {
			case '\n':
			case 'Y':
			case 'y':
				iamopen = 0;
				fclose(fpo);
				printf("...Ok, file now closed...\n\n");
				dum = 1;
				break;
			case 'N':
			case 'n':
				printf("...File remains open..\n\n");
				dum = 1;
				break;
			case 0:
				break;
			default:
				printf("\nsorry, enter Y/N,<CR>=Y:");
				break;
		}
		if ( dum != NULL ) break;
	}
}
