/* 
        d0caeh.c
         Created           : 16-JUN-1995 by Drew Baden
   fills in texts (in CAEH) window for CAEH bank

*/
 
#include <stdio.h>                   /* I/O definitions                       */
#include "xframe/source/d0x_c.h"
#define ALL 0
#define PTR 1
int caehtype = ALL;

void d0caeh(w,tag,reason)
Widget        w;
int        *tag;
unsigned long    *reason;
{
    int select = *tag, nchan,dummy,status,addr;
    char *cbuff,cnl[5], filename[120];
    int etamin,etamax,phimin,phimax,laymin,laymax,iscale;
    float etmin,etmax;                                        
    FILE *fpo;
    
    switch (select) {
        case 0:    /* get CAEH dump, put it into text caeh_text */
          strcpy(cnl,"   \n");
          /*  get eta,phi,layer min and et thresholds */
          cbuff = XmTextGetString(etamin_text);
          etamin = atoi(cbuff);
          cbuff = XmTextGetString(etamax_text);
          etamax = atoi(cbuff);
          cbuff = XmTextGetString(phimin_text);
          phimin = atoi(cbuff);
          cbuff = XmTextGetString(phimax_text);
          phimax = atoi(cbuff);
          cbuff = XmTextGetString(laymin_text);
          laymin = atoi(cbuff);
          cbuff = XmTextGetString(laymax_text);
          laymax = atoi(cbuff);
          cbuff = XmTextGetString(etmin_text);
          sscanf(cbuff,"%f",&etmin);
          cbuff = XmTextGetString(etmax_text);
          sscanf(cbuff,"%f",&etmax);
          dummy = 0;
          if (caehtype == PTR) {
          	cbuff = XmTextGetString(cachcaeh_text);
          	if (strlen(cbuff)<1) {
          		warning("NULL Address not allowed - try again");
          		return;
          	}
          	addr = atoi(cbuff);
          }
#ifdef D0FLAVOR
          picaehn_(&caehtype,&addr,&nchan,&etamin,&etamax,&phimin,&phimax,
          	&laymin,&laymax,&etmin,&etmax);
          if (nchan == 0) {
          	warning("NO CAEH channels - probably there is no CAEH bank");
          	return;
          }
          cbuff = (char *) 
          	malloc(sizeof(char)*(220+57*nchan));
          picaeh_(&caehtype,&addr,cnl,cbuff,&etamin,&etamax,&phimin,&phimax,
          	&laymin,&laymax,&etmin,&etmax);
#else          
          picaehn(&caehtype,&addr,&nchan,&etamin,&etamax,&phimin,&phimax,
          	&laymin,&laymax,&etmin,&etmax);
          if (nchan == 0) {
          	warning("NO CAEH channels - probably there is no CAEH bank");
          	return;
          }
          cbuff = (char *) 
          	malloc(sizeof(char)*(220+57*nchan));
          picaeh(&caehtype,&addr,cnl,cbuff,&etamin,&etamax,&phimin,&phimax,
          	&laymin,&laymax,&etmin,&etmax);
#endif
          SetWatchCursor(caeh_bull);
          XmTextSetString(caeh_text,cbuff);
          free(cbuff);
          SetDefaultCursor(caeh_bull);
          break;
        case 1:   /* help */
          XmTextSetString(physhelp,
"ACCESS to CAEH bank:\n\
Set the THRESHOLD via the slider.  This threshold is compared to the\n\
transverse energy (ET) value for each readout\n\n\
Set minimum and maximum ETA[-37,37], PHI[1,64], and LAYER[1,17] via\n\
typing into appropriate text windwow.\n\
Note:  comparisons are made using greater-than-or-equal-to.  This\n\
means that you can pick out a particular readout tower by setting\n\
the min and max to be equal.\n\n\
Here are the layer specifications:\n\n\
1-7   EM Layers\n\
8-10  Massless gap\n\
11-14 Fine Hadronic\n\
15-17 Coarse Hadronic\n\n\
If you want to look at the readout which is connected to a particular\n\
object (JETS, PELC, PPHO, CACL), then set the toggle to CACH/JPTS and\n\
enter the bank pointer in the appropriate text window.  Fetch will \n\
still apply the same criteria as above, but to only those hits in the\n\
CACH or JPTS bank.\n\n\
PRINT will prompt you for a file name and print the list of towers.");
          break;
        case 2:   /* use all caeh */
          caehtype = ALL;
          break;
        case 3:   /* use pointer to CACH or JPTS bank */
          caehtype = PTR;
          break;
        case 4:   /* print contents */
          xgetchar("Name of output file to create:"," ",
                filename,&status);
          fpo = fopen(filename,"w");
          SetWatchCursor(caeh_bull);
          cbuff = XmTextGetString(caeh_text);
          fprintf(fpo,"%s",cbuff);
          XtFree(cbuff);
          SetDefaultCursor(caeh_bull);
          break;
        case 5:   /* set defaults */
          XmTextSetString(etmin_text,"-1000");
          XmTextSetString(etmax_text,"1000");
          XmTextSetString(etamin_text,"-37");
          XmTextSetString(etamax_text,"38");
          XmTextSetString(phimin_text,"1");
          XmTextSetString(phimax_text,"64");
          XmTextSetString(laymin_text,"1");
          XmTextSetString(laymax_text,"17");
          break;
        default:
          break;
    }
}
