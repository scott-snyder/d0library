/*
        ccrate.c
         Created           :  4-OCT-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "/d0lib/scratch/xframe/source/d0x_c.h"

#define MAXBUFF 40000

#ifdef D0FLAVOR
ccrate_(crate,chans,list,total,ncol,
#else
ccrate(crate,chans,list,total,ncol,
#endif
header,hlength,trailer,tlength,data,length)
int *crate, *chans, *list, *total, *ncol;
int *header, *hlength, *trailer, *tlength, *data, *length;
{
	int nword;
	int the_crate = *crate;
	int num_col = *ncol;
	int n, m, npt, kcol = 0, len;
	char buffer[MAXBUFF], tbuff[100];
	XmTextPosition   top = 0;
/*
    fill miscellaneous label and text widget(s)
*/
	nword = *length;
	sprintf(tbuff,"Crate %d (out of %d) has %d data words",
		the_crate,*total,nword);
	SetLabel(crate_label,tbuff);
/*
    shove header into text widget
*/
	nword = *hlength;
	strcpy(buffer," Header:\n");
	npt = 9;
	for ( n=0; n<nword; n++ ) {
		sprintf(tbuff,"%8x\n",header[n]);
		cupcase(tbuff);
		len = strlen(tbuff);
		for ( m=0; m<len & npt<MAXBUFF; m++ ) buffer[npt++] = tbuff[m];
	}
	buffer[npt++] = '\0';
	XmTextSetString(raw_header,buffer);
	XmTextSetCursorPosition(raw_header,top);
	XmTextShowPosition(raw_header,XmTextGetLastPosition(raw_header));
/*
    shove list of crates into text widget (backwards)
*/
	nword = *total;
	strcpy(buffer,"Crate #Chan\n");
	npt = 12;
	for ( n=0; n<nword; n++ ) {
		sprintf(tbuff,"%5d %5d\n",list[nword-n-1],chans[nword-n-1]);
		cupcase(tbuff);
    	len = strlen(tbuff);
    	for ( m=0; m<len & npt<MAXBUFF; m++ ) buffer[npt++] = tbuff[m];
    }
    buffer[npt++] = '\0';
    XmTextSetString(crates_text,buffer);
	XmTextSetCursorPosition(crates_text,top);
	XmTextShowPosition(crates_text,XmTextGetLastPosition(crates_text));
/*
    shove data into text widget -
    build text of rows with 'ncol' columns
*/
	nword = *length;
	npt = 0;
	if ( nword > 0 ) {
		for ( n=0, npt=0; n<nword; n++ ) {
			sprintf(tbuff," %8x",data[n]);
			cupcase(tbuff);
			len = strlen(tbuff);
			for ( m=0; m<len & npt<MAXBUFF-1; m++ )
				buffer[npt++] = tbuff[m];
			if ( npt < MAXBUFF-1 ) {
				kcol++;
				if ( kcol == num_col ) { kcol = 0;	buffer[npt++] = '\n'; }
			}
		}
	}
	buffer[npt++] = '\0';
	XmTextSetString(raw_text,buffer);
	XmTextSetCursorPosition(raw_text,top);
	XmTextShowPosition(raw_text,XmTextGetLastPosition(raw_text));
/*
    shove trailer info into text widget
*/
	nword = *tlength;
	strcpy(buffer,"Trailer:\n");
	npt = 9;
	for ( n=0; n<nword; n++ ) {
		sprintf(tbuff,"%8x\n",trailer[n]);
		cupcase(tbuff);
		len = strlen(tbuff);
		for ( m=0; m<len & npt<MAXBUFF; m++ ) buffer[npt++] = tbuff[m];
	}
	buffer[npt++] = '\0';
	XmTextSetString(raw_trailer,buffer);
	XmTextSetCursorPosition(raw_trailer,top);
	XmTextShowPosition(raw_trailer,XmTextGetLastPosition(raw_trailer));
}
