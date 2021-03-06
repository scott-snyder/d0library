/* 
        d0xquery.c
         Created           : 16-APR-1993 by Drew Baden

   this  routine  handles  the  setting up  the  queries a  query  consists of a
definition of a requirement, and if the requirement is implemented. e.q.
   
   a definition is based on a word in a bank.  e.g., the et of electrons
   from the PELC bank can be defined as
   
   ETELE==PELC(6)|REAL|
   
   note that for some banks you need another attribute to specify (e.g.
   JETS needs cone, PNUT needs type).  the et of jets is then
   
   ETJETS==JETS[0.7](6)|REAL|
   
   a requirement on this can be set. the syntax will look like:
   
   ETJETS>20
   
   this defines a requirement on a bank (specific JETS bank has et>20).
   you can specify how many banks you want to pass by:
   
   {ETJETS>20}=2
   
   To add banks to the list, do the following:
   1. make a new definition (look below, e.g. #define PELC 0)
   2. add to the *bankname pointers
   3. add a callback to the new widget which calls setquery with the
      proper tag (edit the code below).  setquery is called by squery,
      so in the .UIL file you put a callback to squery.
   4. edit fxhist.for file (subroutine fxhist) and setreq.for 
      (subroutine reqvar and subroutine reqblist)
*/
 
#include <stdio.h>                   /* I/O definitions                       */
#include <ctype.h>
#include <math.h>
#include "xframe/source/d0x_c.h"
#include <Xm/FileSB.h>
#define NL '\n'
unsigned long *uldumq;
Widget wdumq;
void scntfc(double,double*,int*);
Boolean sunique(Widget, char*);
Boolean getdefstring(char*,char*);
Boolean getdefbank(char*,char*);
Boolean getopval2(char*,int*,char*);
Boolean getrdef(char*,char*,int);
Boolean decodeitem(char*,char*,int,char*,char*,int*,int*,int*,char*);
/*
  update timer stuff
*/
Boolean timer_enabled = False;
XtIntervalId update_timer;
int itime = 0, itime_tag = 62;
/*
  the following stuff is for drawing the histogram
*/
#define POSID 20
Boolean DOHIST = True;
Boolean DOERRORS = True;
Boolean DOLINEAR = True;
XFontStruct *qfont;
XmFontList  qfontlist;
GC gc;
XGCValues gcv;
XtGCMask qgcmask;
Display *hdisplay;
Window hwindow;
Position xx1,yy1,xx2,yy2;
#define GAUSS 0
#define EXPO 1
#define POLY 2
int fittype = GAUSS;
Boolean fitoverlay = False;
Boolean dofit = False;
int npar = 0;
float xpar[50], sigpar[50], chi2;
/*
  current selection from list
*/
int *pos_list, pos_count;
XmString *itemlist;
/*
  current histogram
*/
int idnext = 1000;
int nidnext = 100;
int hid, hidn, nbins, lentitle, nentries, dummy;
float fbin,lbin,store=0.0, hval[500], herr[500];
float hsumm, maxb, mean, sigma, minb;
char title[100], title2[100], whatstring[100];
int lentitle2, hid2, nbins2, nentries2;
float fbin2,lbin2,store2=0.0, hval2[500], herr2[500];
float hsumm2, maxb2, mean2, sigma2, minb2;
Boolean super2 = False;

Widget reqfile;
XmStringCharSet charset = XmSTRING_DEFAULT_CHARSET;
int ndefs = 0;        /* number of definitions in place */
int n1reqs = 0;       /* number of requirements */
int nreqs = 0;        /* number of requirements on banks */

#define HISTOGRAM 1
#define NTUPLE 2
int viewtype = 0;

#define POSITIVE -1
#define NEGATIVE 0
int doreqtf = POSITIVE;     /* -1=true, 0=false, set to 0 for negating */

#define AND -1
#define OR 0
int doandor = AND;    /* apply as AND or OR */

#define STOP -1
#define NOSTOP 0
int dostop = STOP;    /* stop if req set and met? (no means probably we
                         are outputting events instead */

/* list of objects */
#define PELC 0
#define PPHO 1
#define JETS 2
#define PNUT 3
#define PMUO 4
#define VERT 5
#define HEAD 6
#define HMTE 7
#define PTAU 8
#define XXXX 9
char *bankname[] = 
{"PELC","PPHO","JETS","PNUT","PMUO","VERT","HEAD","HMTE","PTAU","XXXX"};
int object = PELC;

/* offset */
int offset = 0;

/* datatype */
#define REAL 0
#define INTEGER 1
#define HEX 2
#define BOOLEAN 3
#define CHAR4 4
char *datatypename[] = {"REAL   ","INTEGER","HEX    ","BOOLEAN","CHAR4  "};
int datatype = REAL;

/* operation */
#define ISLT 0
#define ISLE 1
#define ISEQ  2
#define ISGE 3
#define ISGT 4
#define ISTRUE 5
#define ISFALSE 6
#define ISNE 7
char *theop[] = {"< ","<=","= ",">=","> ","T ","F ","!="};
int operation = ISGT;
int operation2 = ISGT;

FILE *fqpo;
char filename[100];

void d0xquery(w,tag,reason)
Widget		w;
int		*tag;
XmListCallbackStruct *reason;
{
	int select = *tag, nitem, ipnut, pound, equal, rlen, slen;
	int i,j,k,n, pos_count, *pos_list, status, dummy, nitemd;
	int len,len1,len2, numi;
	char dbank[5], dval[10], dval2[10], dspec[10], cbank[5];
	int doffset,dtype,dop,dop2;
	char *choice, *cdefined, *theval, string[100], cslist[100], *single;
	char ccone[10], *singledef, reqstring[100], defstring[100];
	char rdef[100], ddef[100], req1string[100];
	float fval, cone;
	Arg wargs[10];
	Boolean foundit;
	XmString *itemlist, *itemdeflist, *reqlist, tmp;
	extern void nomoref();
	extern void openreq();
	
	switch (select) {
		case 0:              /* implement definition */
			/*
			    must have name and value defined first
			*/
			cdefined = XmTextGetString(defined);
			if (strlen(cdefined) < 1 ) {
				warning("Please define NAME (1st box) first");
				XtFree(cdefined);
				break;
			}
			/*
			  convert to uppercase and shove back into text widget
			*/
			strcpy(cslist,cdefined);  
			cupcase(cslist);
			XmTextSetString(defined,cslist);
			/*
			  see if this definition already exists
			*/
			if ( !sunique(deflist,cslist) ) {
				warning("DEFINITION not unique - try another");
				XtFree(cdefined);
				return;
			}
			/*
			  get offset value
			*/
			XmScaleGetValue(boffset,&offset);
			/*
			  if this is in the list of specials, ask for the special
			  parameter (and build title)
			*/
			switch (object) {
				case JETS:
					xgetchar("Jet cone or 0=NN or 5=Kt - Negate for JNEP ",
						"0.7",ccone,&status);
					sscanf(ccone,"%f",&cone);
					sprintf(string,"%s==%s[%.2f](%d)|%s",cslist,
						bankname[object],cone,offset,
						datatypename[datatype]);
					break;
				case PNUT:
					xgetchar("Which PNUT? (integer) ","2",ccone,&status);
					sscanf(ccone,"%d",&ipnut);
					sprintf(string,"%s==%s[%d](%d)|%s",cslist,
						bankname[object],ipnut,offset,
						datatypename[datatype]);
					break;
				case HMTE:
					xgetchar("HMTE from PELC (1) or PPHO (0) ","1",ccone,
							&status);
					sscanf(ccone,"%d",&ipnut);
					sprintf(string,"%s==%s[%d](%d)|%s",cslist,
						bankname[object],ipnut,offset,
						datatypename[datatype]);
					break;
				case XXXX:        /* generic */
					sprintf(string,"%s==%s[*](%d)|%s",cslist,bankname[object],
							offset,datatypename[datatype]);
					break;
				default:
					sprintf(string,"%s==%s(%d)|%s",cslist,bankname[object],
							offset,datatypename[datatype]);
					break;
			}
			/*
			  strip off the trailing blanks and add "|"
			*/
			squeeze(string,' ');
			strcat(string,"|");
			/*
			    add it to list
			*/
			XtFree(cdefined);
			tmp = XmStringCreateSimple(string);
			ndefs++;
			XmListAddItemUnselected(deflist,tmp,ndefs);
			XmStringFree(tmp);
			break;
		case 1:              /* reset objects */
			if (ndefs < 1) break;
			XmListDeleteAllItems(deflist);
			ndefs = 0;
			break;
		case 2:              /* delete definition */
			if (XmListGetSelectedPos(deflist,&pos_list,&pos_count)) {
				/*
				  loop over selected items, delete position
				*/
				for (i=0; i<pos_count; i++) {
					XmListDeletePos(deflist,*pos_list+i);
					ndefs--;
				}
				XtFree((char*)pos_list);
			}
			break;
		case 5:            /* add definition to req1 list */
			/*
			  check that there is a match in operation/value
			*/
			if ( (operation != ISTRUE) && (operation != ISFALSE) ) {
				theval = XmTextGetString(thevalue);
				if (strlen(theval) < 1) {
					warning("Select VALUE to specify requirement");
					XtFree(theval);
					return;
				}
				sscanf(theval,"%f",&fval);
			}
			if (XmListGetSelectedPos(deflist,&pos_list,&pos_count)) {
				/*
				  only one at a time please
				*/
				if ( pos_count > 1 ) {
					warning("Please, only ONE at a time!");
					return;
				}
				/*
				  get list of items - should contain only one item
				*/
				XtVaGetValues(deflist, XmNitems, &itemlist, NULL);
				/*
				  build it here
				*/
				XmStringGetLtoR(itemlist[*pos_list-1],charset,&choice);
				/*
				  keep only definition part
				*/
				strcpy(string,choice);
				equal = strcx(string,"=");
				string[equal] = '\0';
				/*
				  value needed?
				*/
				if ( datatype == BOOLEAN ) {
					/*
					  make sure the operation is true or false
					*/
					if ( (operation != ISTRUE) && (operation != ISFALSE) ) {
						warning
						  ("Must select TRUE or FALSE for BOOLEAN datatype");
						XtFree(cdefined);
						return;
					}
				}
				else {
					/*
					  make sure a value is chosed
					*/
					theval = XmTextGetString(thevalue);
					if (strlen(theval) < 1 ) {
						warning("Select VALUE for requirement");
						XtFree(cdefined);
						XtFree(theval);
						return;
					}
					/*
					  make sure the operation is NOT true or false
					  */
					if ( (operation == ISTRUE) || (operation == ISFALSE) ) {
						warning("TRUE or FALSE only allowed BOOLEAN datatype");
						XtFree(cdefined);
						return;
					}
				}
				/*
				  build the rest of the title
				*/
				sprintf(cslist,"%s",theop[operation]);
				strcat(string,cslist);
				if ( (operation != ISTRUE) && (operation != ISFALSE) ) {
					sscanf(theval,"%f",&fval);
					switch (datatype) {
						case REAL:
							sprintf(cslist,"%.2f",fval);
							break;
						case INTEGER:
							sprintf(cslist,"%d",(int) fval);
							break;
						case HEX:
							sprintf(cslist,"%x",(int) fval);
							break;
						case CHAR4:
							sprintf(cslist,"%4c",(int) fval);
							break;
					}
				}
				strcat(string,cslist);
				/*
				  make sure it's unique
				*/
				if ( !sunique(req1list,string) ) {
					warning("REQUIREMENT condition NOT unique - try again");
					return;
				}
				/*
				  ok, add it
				*/
				n1reqs++;
				tmp = XmStringCreateSimple(string);
				XmListAddItemUnselected(req1list,tmp,nreqs);
				XmStringFree(tmp);
			}
			else {
				warning("Nothing SELECTED?");
			}
			break;
		case 6:        /* delete definition from req1list */
			if (XmListGetSelectedPos(req1list,&pos_list,&pos_count)) {
				for (i=0; i<pos_count; i++) {
					XmListDeletePos(req1list,*pos_list+i);
					n1reqs--;
				}
				XtFree((char*)pos_list);
			}
			break;
		case 7:        /* reset req1list */
			if (n1reqs < 1) return;
			XmListDeleteAllItems(req1list);
			n1reqs = 0;
			break;
		case 11:  /* assuming that some req1 are selected, move it here 
		             into requirements list widget */
			/*
			  check that there is a match in operation/value
			*/
			if ( (operation2 != ISTRUE) && (operation2 != ISFALSE) ) {
				theval = XmTextGetString(reqvalue);
				if (strlen(theval) < 1) {
					warning("Select VALUE to specify requirement");
					XtFree(theval);
					return;
				}
				sscanf(theval,"%f",&fval);
			}
			if (!XmListGetSelectedPos(req1list,&pos_list,&pos_count)) {
				warning("Nothing SELECTED?");
				return;
			}
			/*
			  build a new string
			*/
			strcpy(cslist,"{");
			/*
			  get list of items - can contain any number of items
			*/
			XtVaGetValues(req1list, XmNitems, &itemlist, NULL);
			/*
			  require that ALL have same bank name - use 1st one
			*/
			XmStringGetLtoR(itemlist[*pos_list-1],charset,&choice);
			strcpy(string,choice);
			strcat(cslist,choice);
			/*
			  get rid of any trailing NL characters
			*/
			slen = strlen(cslist);
			if (cslist[slen] == NL) cslist[slen] = '\0';
			/*
			  dig out definitions
			*/
			if ( !getdefstring(string,defstring) ) {
				warning
				  ("Inconsistency - cannot find DEF string - tell DREW");
				return;
			}
			if ( !getdefbank(defstring,dbank) ) {
				warning
					  ("Inconsistency - cannot find bank name - tell DREW");
				return;
			 }
			 strcat(cslist,"&");
			 for (i=1; i<pos_count; i++) {
			 	XmStringGetLtoR(itemlist[pos_list[i]-1],charset,&choice);
			 	strcpy(string,choice);
			 	/*
			 	  get rid of any trailing NL characters
			 	*/
			 	slen = strlen(string);
			 	if (string[slen] == NL) string[slen] = '\0';
			 	if ( !getdefstring(string,defstring) ) {
			 	  warning
					  ("Inconsistency - cannot find DEF string(2) - tell DREW");
				  return;
				}
				if ( !getdefbank(defstring,cbank) ) {
				  warning
				  	  ("Inconsistency - cannot find bank name(2) - tell DREW");
				  return;
				}
				dummy = strcmp(cbank,dbank);
				if (dummy != 0) {
				  warning("Cannot combine - these have different names!");
				  return;
				}
				strcat(cslist,choice);
				strcat(cslist,"&");
			}
			/*
			  ok, things look consistent - finish it here
			*/
			dummy = strlen(cslist);
			cslist[dummy-1] = '}';
			sprintf(choice,"%s",theop[operation2]);
			strcat(cslist,choice);
			if ( (operation2 != ISTRUE) && (operation2 != ISFALSE) ) {
				sprintf(choice,"%d",(int) fval);
				strcat(cslist,choice);
			}
			/*
			  make sure it's unique
			*/
			if ( !sunique(reqlists,cslist) ) {
				warning("REQUIREMENT condition NOT unique - try again");
				return;
			}
			/*
			  ok, add it
			*/
			nreqs++;
			tmp = XmStringCreateSimple(cslist);
			XmListAddItemUnselected(reqlists,tmp,nreqs);
			XmStringFree(tmp);
			/*
			  and delesect all
			*/
			XmListDeselectAllItems(req1list);
			break;			
		case 12:  /* reset requirement list and cancel everything */
			if (nreqs < 1) break;
			XmListDeleteAllItems(reqlists);
			nreqs = 0;
			dummy = 0;
#ifdef D0FLAVOR
			setreq_(&dummy,&dummy);
#else
			setreq(&dummy,&dummy);
#endif
			XmTextSetString(reqtext,"NONE");
			break;
		case 13:  /* delete item */
			if (XmListGetSelectedPos(reqlists,&pos_list,&pos_count)) {
				for (i=0; i<pos_count; i++) {
					XmListDeletePos(reqlists,*pos_list+i);
					nreqs--;
				}
				XtFree((char*)pos_list);
			}
			break;
		case 14:   /* get .zeb file */
#ifdef D0FLAVOR
			freqzeb_
#else
			freqzeb
#endif
				(reqzeb,bankname[object]);
			break;
		case 15:   /* help */
			XmTextSetString(qhelp,
"\
The QUERY EDITOR can be used to do the following:\n\n\
1. define a variable based on a specific value in a specific bank\n\
2. create and fill a histogram of the value\n\
3. create and fill a 1-column ntuple of the value\n\
4. build a 'query' on the data.\n\n\
To create the variable you have to specify:\n\n\
0. defined name (arbitrary but non-blank)\n\
1. bank name from among the list. GENERIC means generic, you will be prompted\n\
   for the name of the bank.  Beware that if you choose a 'GENERIC' bank,  no\n\
   ZEBRA structure will be  assumed - ZEBRA routines will be used  to get the\n\
   occurances of this bank.\n\
2. special attributes of the bank (e.g. cone size for JETS, pnut type for\n\
   PNUT, PELC/PPHO for HMTE)\n\
3. offset from iq(pointer) (hit the '(.ZEB)' button to see the corresponding\n\
   .ZEB file for help on where each variable lives in the bank)\n\
4. data type (real, integer, etc.).\n\n\
You specify these  in the window  under the label  'Build Definition'. After\n\
specifying, hit the 'Add' button  underneith the scroll window to create the\n\
new variable. It  will appear in the  scrolled list. To  delete any variable\n\
from the list, select and hit the  'Delete' button. To clear the entire list\n\
hit the 'Reset' button.  GENERIC banks will have a [*] in the definition.\n\n\
Histograms or 1-d  ntuples can be  created from the  variable. Highlight the\n\
one  you want  and  hit  'Create'  under  either   'Histogram...'  or 'Tuple\n\
Controls'. You  will be prompted for  the appropriate  attributes. Note that\n\
you can  require the  histogram or tuple be  filled before  or after cuts by\n\
hitting the appropriate  buttons in the  'attributes' window which comes up.\n\
Histogram ids begin at 1000 for  histograms filled before cuts, and 2000 for\n\
those that are filled after cuts. The  ids are 100 and 200 for tuples. After\n\
'accept' D0X is  flagged to do what  you requested on  reading in of events.\n\
You can view  the histogram  by hitting the  'View' button  under 'Histogram\n\
Controls', or you can  view a histogram of the  ntuple column by hitting the\n\
'View' button  under 'Tuple  Controls'. Note that you  can dynamically rebin\n\
the histogram made from the tuple for optimization (no cheating!). The reset\n\
of the buttons under the '*Controls' are self explanatory.\n\n\
Each  created  variable is  found in  a bank.  You can  build a  'query', or\n\
condition  requirement, on the event  based on a number  of variables in the\n\
bank. This requirement will be on each bank of whatever name you have chosen\n\
for the variables, AND on the number of banks that pass the requirement. For\n\
instance,  you can  require  that there be  at least  one PELC  bank with an\n\
electron  with  ET  larger than  20 and  ETA  between +-  1.  You  build the\n\
definition of the electron ET by choosing a name (call it ETELE) and hitting\n\
the ADD button. It should look like this:\n\n\
ETELE==PELC(7)|REAL|\n\n\
For the ETA, if you use ETAELE it would look like this:\n\n\
ETAELE==PELC(9)|REAL|\n\n\
There are 3  requirements here:  ETELE>20, ETAELE>-1,  ETAELE<1.  To require\n\
ETELE>20, you select the definition ETELE in the definition list, select the\n\
operation you want (>, <, =, etc.)  and the value just under the label 'List\n\
of Requirements of  Definition:' (upper RH  corner) and hit the 'Add' button\n\
just below. You should see\n\n\
ETELE>20\n\n\
appear in the  requirements list. Similarly, you  make two more requirements\n\
using ETAELE. The following should appear in the list under ETELE>20:\n\n\
ETAELE>-1\n\
ETAELE<1\n\n\
To select that there be at least one bank which satisfies ALL of these three\n\
requirements,  you   select each of  the above  requirements,  select '>' in\n\
below the  label 'Reqs  satisfied per  event', fill in '0'  in the text area\n\
just to the  right of this,  and hit the  next 'Add' button  underneath. You\n\
should then see\n\n\
{ETELE>20&ETAELE>-1&ETAELE<1}>0\n\n\
Note  that  you can  only  AND   requirements  per  bank, and  all  of these\n\
requirement must  be on a definition  that has the same bank name or you are\n\
sunk. All events with at  least 1 PELC bank  passing the requirement will be\n\
selected when you  'Implement' button  to implement, or  'Cancel' to cancel,\n\
You can do the following for events satisfying the requirement you have just\n\
built using the row of buttons in the bottom RH corner.\n\n\
1. STOP or CONTINUE.  STOP is useful if you want to use the PHYSICS window\n\
   to look at this event.  CONTINUE is used if you plan on stripping this\n\
   event to disk if it passes.\n\
2. You can build multiple requirements and either .OR. or .AND. them all\n\
   together.\n\
3. You can require that the a PASS is based on PASSING the requirement\n\
   (Positive) or FAILING the requirement (Negative).\n\n\
The banks which you can cut on are  the familiar 'physics' banks PELC, PMUO,\n\
etc.  If  you  want  more,  send  e-mail.  Note  that  for PNUT  you will be\n\
prompted for the pnut 'type' (1,2,3,etc.), and for JETS you will be prompted\n\
for the  cone size  (.3, .5,  .7, or 0 for  NN). If  you make  the cone size\n\
negative (e.g. -0.7),  then you will get JETS  with cone of 0.7 unless there\n\
exists a JNEP bank  underneath, in  which case you will  get the JNEP. So by\n\
cutting  on  the  ET in  the  JETS  bank  with  cone of  -0.7,  you  will be\n\
effectively cutting on  only those jets which do  not overlap with PELC/PPHO\n\
candidates.\n\n\
To output  events, go into  the 'Output'  window (selected  in the 'Windows'\n\
menu at  the top  of the  main D0X  window)  and open  the file  and set the\n\
appropriate conditions.\n\n\
The stuff in the scrolled lists can be saved to disk read back in from ASCII\n\
files. You can edit these files by hand, but there is a specific syntax that\n\
you are advised to try not to screw with!\n");
			break;
		case 20:   /* save defs and reqs to disk */
			/*
			  open file
			*/
			xgetchar("Name of output file to create:"," ",
				filename,&status);
			fqpo = fopen(filename,"w");
			/*
			  do def list
			*/
			XtVaGetValues(deflist, XmNitems, &itemlist, 
				XmNitemCount, &nitem, NULL);
			fprintf(fqpo,"!DEFINITIONS LIST: has %d elements\n",nitem);
			for (i=0; i<nitem; i++) {
				XmStringGetLtoR(itemlist[i], charset, &single);
				/*
				  get rid of histograms
				*/
				strcpy(string,single);
				pound = strcx(string,"#");
				if (pound != -1) string[pound] = '\0';
				fprintf(fqpo,"%s\n",string);
			}
			/*
			  do req1 list
			*/
			XtVaGetValues(req1list, XmNitems, &itemlist, 
				XmNitemCount, &nitem, NULL);
			fprintf(fqpo,"!BANK REQUIREMENT LIST: has %d elements\n",nitem);
			for (i=0; i<nitem; i++) {
				XmStringGetLtoR(itemlist[i], charset, &single);
				fprintf(fqpo,"%s\n",single);
			}
			/*
			  do #banks req list
			*/
			XtVaGetValues(reqlists, XmNitems, &itemlist, 
				XmNitemCount, &nitem, NULL);
			fprintf(fqpo,"!#BANKS REQUIREMENT LIST: has %d elements\n",nitem);
			for (i=0; i<nitem; i++) {
				XmStringGetLtoR(itemlist[i], charset, &single);
				fprintf(fqpo,"%s\n",single);
			}
			/*
			  specify "other" stuff 
			*/
			if ( doreqtf == POSITIVE ) fprintf(fqpo,"$POSITIVE:\n");
			else fprintf(fqpo,"$NEGATIVE:\n");
			if ( doandor == AND ) fprintf(fqpo,"$AND:\n");
			else fprintf(fqpo,"$OR:\n");
			if ( dostop == STOP ) fprintf(fqpo,"$STOP:\n");
			else fprintf(fqpo,"$CONTINUE:\n");
			fprintf(fqpo,"!DONE");
			/*
			  all done - close it
			*/
			fclose(fqpo);
			break;
		case 21:   /* read defs and reqs from disk */
			/*
			  open file
			*/
			if (reqfile == NULL) {
				n = 0;
				tmp = XmStringCreateSimple("File Browser");
				XtSetArg(wargs[n], XmNdialogTitle, tmp); n++;
				reqfile = XmCreateFileSelectionDialog(toplevel_widget,
					"filesb",wargs,n);
				XtAddCallback(reqfile, XmNcancelCallback, nomoref, NULL);
				XtAddCallback(reqfile, XmNokCallback, openreq, NULL);
				XmStringFree(tmp);
			}
			XtManageChild(reqfile);
			break;
		case 30:   /* implement requirements */
			/*
			  first, cancel old requirements
			*/
			dummy = 0;
#ifdef D0FLAVOR
			setreq_(&dummy,&dummy);
#else
			setreq(&dummy,&dummy);
#endif
			/*
			  dig them out of the reqlists list widget - the form is:
			  {NAME1>A&NAME2>B...}>C
			  
			  NAME1, NAME2 etc. must be a definition which uses the
			  same bank (this is checked for in case 11 above).
			  
			  first we collect the definitions used, and pass them
			  to the driver.  they are ordered by some index.  then we
			  pass to it the requirements on banks, each of which is an
			  AND over the requirements on the definitions
			  
			*/
			XtVaGetValues(reqlists, XmNitemCount, &nitem, 
					XmNitems, &reqlist, NULL);
			if ( nitem != nreqs ) {
				warning("Inconsistency in REQ item count - tell DREW");
				return;
			}
			/* 
			   loop over each bank requirement, dig out definition and
			   requirement on it
			*/
			for (i=0; i<nitem; i++) {
				XmStringGetLtoR(reqlist[i],charset,&choice);
				strcpy(reqstring,choice);
				n = i + 1;
				/*
				  get op and value for this requirement
				*/
				if (!getopval2(reqstring,&dop2,dval2)) {
					warning("Inconsistency, 2nd op - tell DREW");
					return;
				}
				len2 = strlen(dval2);
				for (k=0,numi=0; k<strlen(reqstring); k++)
					if (reqstring[k] == '&') numi++;
				numi++;
				/*
				  loop over all of these, decode each one into it's
				  parts, and pass along
				*/
				for (j=1; j<=numi; j++) {
					if (!getrdef(reqstring,req1string,j)) {
						warning("Inconsistency inner loop(2) - tell DREW");
						return;
					}
					/*
					  dig out definition name
					*/
					if (!getdefstring(req1string,defstring)) {
						warning("Inconsistency inner loop(3) - tell DREW");
						return;
					}
					/*
					  decode into "atomic" parts and load it in 
					*/
					if ( !decodeitem(req1string,defstring,0,
						dbank,dspec,&doffset,&dtype,&dop,dval) ) {
						warning("INTERNAL error, inconsistency, tell DREW");
						return;
					}
					len = strlen(dspec);
					len1 = strlen(dval);
#ifdef D0FLAVOR
				    loadreq_
#else
				    loadreq
#endif
					(&doreqtf,&doandor,&dostop,&n,
					 dbank,dspec,&len,&doffset,&dtype,&dop,dval,&len1);
				}
				/*
				  and load this entire requirement e.g. {...}>2 
				*/
#ifdef D0FLAVOR
				loadtreq_
#else
				loadtreq
#endif
				(&dop2,dval2,&len2,&n);
			}
			/*
			  implement it
			*/
			dummy = -1;
#ifdef D0FLAVOR
			setreq_(&dummy,&dummy);
#else
			setreq(&dummy,&dummy);
#endif
			XmTextSetString(reqtext,"IMPLEMENTED");
			break;
		case 31:   /* cancel requirements */
			dummy = 0;
#ifdef D0FLAVOR
			setreq_(&dummy,&dummy);
#else
			setreq(&dummy,&dummy);
#endif
			XmTextSetString(reqtext,"NONE");
			break;
		default:
			break;
		}
}

void setquery(w,tag,reason)
Widget		w;
int		*tag;
unsigned long	*reason;
{
	int select = *tag, which, status;
	char cc[10];
	
    switch (select) {
		case 0:           /* set pelc object */
			object = PELC;
			break;
		case 1:           /* set ppho object */
			object = PPHO;
			break;
		case 2:           /* set jets object */
			object = JETS;
			break;
		case 3:           /* set pnut object */
			object = PNUT;
			break;
		case 4:           /* set pmuo object */
			object = PMUO;
			break;
		case 5:           /* set vert object */
			object = VERT;
			break;
		case 6:           /* set head object */
			object = HEAD;
			break;
		case 7:           /* set hmte object */
			object = HMTE;
			break;
		case 8:           /* GENERIC */
			object = XXXX;
			xgetchar("Enter Bank name"," ",cc,&status);
			squeeze(cc,' ');
			cupcase(cc);
			bankname[XXXX] = (char *) malloc(4);
			strncpy(bankname[XXXX],cc,4);
			break;
		case 9:           /* set ptau object */
			object = PTAU;
			break;
		case 10:		  	/* data type real */
			datatype = REAL;
			break;
		case 11:			/* data type integer */
			datatype = INTEGER;
			break;
		case 12:			/* data type hex */
			datatype = HEX;
			break;
		case 13:			/* data type boolean */
			datatype = BOOLEAN;
			break;
		case 14:			/* data type char4 */
			datatype = CHAR4;
			break;
		case 20:			/* operation .lt. */
			operation = ISLT;
			break;
		case 21:			/* operation .le. */
			operation = ISLE;
			break;
		case 22:			/* operation .eq. */
			operation = ISEQ;
			break;
		case 23:			/* operation .ge. */
			operation = ISGE;
			break;
		case 24:			/* operation .gt. */
			operation = ISGT;
			break;
		case 25:			/* operation true */
			operation = ISTRUE;
			break;
		case 26:			/* operation false */
			operation = ISFALSE;
			break;
		case 27:			/* operation .ne. */
			operation = ISNE;
			break;
		case 30:			/* operation .lt. */
			operation2 = ISLT;
			break;
		case 31:			/* operation .le. */
			operation2 = ISLE;
			break;
		case 32:			/* operation .eq. */
			operation2 = ISEQ;
			break;
		case 33:			/* operation .ge. */
			operation2 = ISGE;
			break;
		case 34:			/* operation .gt. */
			operation2 = ISGT;
			break;
		case 35:			/* operation true */
			operation2 = ISTRUE;
			break;
		case 36:			/* operation false */
			operation2 = ISFALSE;
			break;
		case 37:			/* operation .ne. */
			operation2 = ISNE;
			break;
		case 40:           /* and */
			doandor = AND;
			which = 3;
#ifdef D0FLAVOR
			setreq_(&which,&doandor);
#else
			setreq(&which,&doreqtf);
#endif
			break;
		case 41:           /* or */
			doandor = OR;
			which = 3;
#ifdef D0FLAVOR
			setreq_(&which,&doandor);
#else
			setreq(&which,&doreqtf);
#endif
			break;
		case 42:             /* make implemented requirement positive */
			doreqtf = POSITIVE;
			which = 2;
#ifdef D0FLAVOR
			setreq_(&which,&dostop);
#else
			setreq(&which,&doreqtf);
#endif
			break;
		case 43:             /* make implemented requirement negative */
			doreqtf = NEGATIVE;
#ifdef D0FLAVOR
			setreq_(&which,&dostop);
#else
			setreq(&which,&doreqtf);
#endif
			break;
		case 44:           /* stop on requirement met */
			dostop = STOP;
			which = 1;
#ifdef D0FLAVOR
			setreq_(&which,&dostop);
#else
			setreq(&which,&dostop);
#endif
			break;
		case 45:           /* don't stop on requirement met (output?) */
			dostop = NOSTOP;
			which = 1;
#ifdef D0FLAVOR
			setreq_(&which,&dostop);
#else
			setreq(&which,&dostop);
#endif
			break;
		case 60:          /* turn errors on */
			DOERRORS = True;
			break;
		case 61:          /* turn errors off */
			DOERRORS = False;
			break;
		case 62:          /* histogram */
			DOHIST = True;
			break;
		case 63:          /* points */
			DOHIST = False;
			break;
		case 64:          /* linear scale */
			DOLINEAR = True;
			break;
		case 65:          /* log scale */
			DOLINEAR = False;
			break;
		case 80:          /* set overlay */
			fitoverlay = True;
			break;
		case 81:          /* set nooverlay */
			fitoverlay = False;
			break;
		default:
			break;
		}
}

void dxhisto(w,tag,reason) /* process for histograms note:  to make things 
                              easy all histogram id's will be the position 
                              in the requirements list + 1000 */
Widget		w;
int		*tag;
unsigned long  *reason;
{
	void timeautoupdate();
	int select = *tag, pound, idhist, i, j, degr;
	int doffset,dtype,dop,dop2,status, equal, ival, dummy, ilen, slen;
	double dfbin;
	XmString tmp;
	char dbank[5], dval[10], dval2[10], dspec[10];
	char *choice, string[100], hstring[20];
	Boolean listit;
	char hfile[100];
	
	switch (select) {
		case 0:        /* create histo here */
			listit = XmListGetSelectedPos(deflist,&pos_list,&pos_count);
			if ( !listit ) {
				XtUnmanageChild(xhmake);
				warning("Please, only ONE at a time!");
				return;
			}
			/*
			  only one at a time please - should never happen due to above 
			*/
			if ( pos_count > 1 ) {
				XtUnmanageChild(xhmake);
				warning("Please, only ONE at a time!");
				return;
			}
			if ( pos_count == 0) {
				XtUnmanageChild(xhmake);
				warning("Nothing selected");
				return;
			}
			/*
			  get list of items - should contain only one item
			*/
			XtVaGetValues(deflist, XmNitems, &itemlist, NULL);
			/*
			  put into character string
			*/
			XmStringGetLtoR(itemlist[*pos_list-1],charset,&choice);
			/*
			  see if it has a # sign on the end
			*/
			strcpy(string,choice);
			if ( strcx(string,"#") != -1) {
				warning("Histogram already requested for this definition");
				XtUnmanageChild(xhmake);
				return;
			}
			/*
			  put it into the text widget so we know what we are doing
			*/
			XmTextSetString(xhwhat,string);
			strcpy(whatstring,string);
			/*
			  the id goes here:
			*/
			hid = idnext;  idnext++;
			sprintf(hstring,"%d",hid);
			XmTextSetString(xhid,hstring);
			/*
			  put the definition name into the title text widget as well
			*/
			strcpy(string,choice);
			equal = strcx(string,"=");
			if ( equal != -1 ) string[equal] = '\0';
			XmTextSetString(xhtitle,string);
			break;
		case 1:        /* delete histo here */
			listit = XmListGetSelectedPos(deflist,&pos_list,&pos_count);
			if ( !listit ) {
				warning("Please, only ONE at a time!");
				return;
			}
			/*
			  only one at a time please - should never happen due to above
			*/
			if ( pos_count > 1 ) {
				warning("Please, only ONE at a time!");
				return;
			}
			if ( pos_count == 0) {
				warning("Nothing selected");
				return;
			}
			/*
			  get list of items - should contain only one item
			*/
			XtVaGetValues(deflist, XmNitems, &itemlist, NULL);
			/*
			  put into character string
			*/
			XmStringGetLtoR(itemlist[*pos_list-1],charset,&choice);
			/*
			  see if it has a # sign on the end
			*/
			strcpy(string,choice);
			pound = strcx(string,"#");
			if ( pound == -1) {
				warning("Histogram not defined - cannot delete");
				return;
			}
			/*
			  ok, delete the # sign and put it back
			*/
			choice[pound] = '\0';
			tmp = XmStringCreateSimple(choice);
			XmListReplaceItems(deflist,&itemlist[*pos_list-1],1,&tmp);
			XmStringFree(tmp);
			/*
			  and delete the thing from hbook and tell D0X not to fill it
			*/
#ifdef D0FLAVOR
			hdelet_(&hid);
			chcanh_(&hid);
#else
			hdelet(&hid);
			chcanh(&hid);
#endif
			break;
		case 2:        /* update here view histo here */
			/*
			  ntuples are serviced by case 14 - this is for histograms
			*/
			if (viewtype == NTUPLE) {
				dummy = 14;
			        dxhisto(wdumq,&dummy,uldumq);
				return;
			}
			listit = XmListGetSelectedPos(deflist,&pos_list,&pos_count);
			if ( !listit ) {
				warning("Please, only ONE at a time!");
				XtUnmanageChild(hdraw);
				return;
			}
			/*
			  only one at a time please - should never happen due to above
			*/
			if ( pos_count > 1 ) {
				warning("Please, only ONE at a time!");
				XtUnmanageChild(hdraw);
				return;
			}
			if ( pos_count == 0) {
				warning("Nothing selected");
				return;
			}
			/*
			  get list of items - should contain only one item
			*/
			XtVaGetValues(deflist, XmNitems, &itemlist, NULL);
			/*
			  put into character string
			*/
			XmStringGetLtoR(itemlist[*pos_list-1],charset,&choice);
			/*
			  see if it has a # sign on the end
			*/
			strcpy(string,choice);
			pound = strcx(string,"#");
			if ( pound == -1) {
				warning("Histogram not defined - cannot view");
				XtUnmanageChild(hdraw);
				return;
			}
			/*
			  grab the id number
			*/
			for (i=pound+1, j=0; i<strlen(string); i++, j++)
				hstring[j] = string[i];
			hstring[j] = '\0';
			/*
			  get id and make sure histogram exists (should...)
			*/
			sscanf(hstring,"%d",&hid);
#ifdef D0FLAVOR
			dummy = hexist_(&hid);
#else
			dummy = hexist(&hid);
#endif
			if (dummy == 0) {
				warning("Histogram not defined ");
			    return;
			}
			/*
			  get all the stuff corresponding to this hid
			*/
#ifdef D0FLAVOR
			chgive_
#else
			chgive
#endif
			(&hid,title,&lentitle,&nbins,&fbin,&lbin,&minb,&maxb,
					&hsumm,&mean,&sigma,hval,herr,&nentries);
			/*
			  cancel fit and superimpose
			*/
			dofit = False;
			super2 = False;
			XtManageChild(hdraw);
			dummy = 0;
			d0xhdraw(wdumq,&dummy,uldumq);
			break;
		case 3:        /* accept newly created histo */
			/*
			  TITLE 
			*/
			choice = XmTextGetString(xhtitle);
			if (strlen(choice) == 0) {
				warning("Please furnish a histogram TITLE");
				return;
			}
			strcpy(title,choice);
			lentitle = strlen(title);
			/*
			  ID 
			*/
			choice = XmTextGetString(xhid);
			if (strlen(choice) == 0) {
				warning("Please furnish a histogram ID");
				return;
			}
			hid = atoi(choice);
			strcpy(hstring,choice);
			/*
			  NBINS
			*/
			choice = XmTextGetString(xhnbin);
			if (strlen(choice) == 0) {
				warning("Please furnish number of BINS in histogram ");
				return;
			}
			nbins = atoi(choice);
			/*
			  LOWER EDGE FIRST BIN
			*/
			choice = XmTextGetString(xhbinl);
			if (strlen(choice) == 0) {
				warning("Please furnish LOWER EDGE OF 1ST BIN in histogram ");
				return;
			}
			sscanf(choice,"%f",&fbin);
			/*
			  UPPER EDGE LAST BIN
			*/
			choice = XmTextGetString(xhbinh);
			if (strlen(choice) == 0) {
				warning("Please furnish UPPER EDGE OF LAST BIN in histogram ");
				return;
			}
			sscanf(choice,"%f",&lbin);
			/*
			  ok, mark it with the # sign and put it back
			*/
			strcpy(string,whatstring);
			strcat(string,"#");
			strcat(string,hstring);
			tmp = XmStringCreateSimple(string);
			XmListReplaceItems(deflist,&itemlist[*pos_list-1],1,&tmp);
			XmStringFree(tmp);
			/*
			  fire away
			*/
#ifdef D0FLAVOR
			chbook1_
#else
			chbook1
#endif
			(&hid,title,&lentitle,&nbins,&fbin,&lbin,&store);
			/*
			  decode it
			*/
			if ( !decodeitem(whatstring,whatstring,1,
					dbank,dspec,&doffset,&dtype,&dop,dval) ) {
						warning("INTERNAL error, inconsistency, tell DREW");
						return;
				}
			/*
			  and tell D0X driver to go ahead and fill it
			*/
			slen = strlen(dspec);
#ifdef D0FLAVOR
			chseth_
#else
			chseth
#endif
				(&hid,dbank,dspec,&slen,&doffset,&dtype);
			break;
		case 4:       /* modify existing histogram definition (deletes
		                 old histogram in the process)  */
			listit = XmListGetSelectedPos(deflist,&pos_list,&pos_count);
			if ( !listit ) {
				warning("Please, only ONE at a time!");
				XtUnmanageChild(hdraw);
				return;
			}
			/*
			  only one at a time please - should never happen due to above
			*/
			if ( pos_count > 1 ) {
				warning("Please, only ONE at a time!");
				XtUnmanageChild(hdraw);
				return;
			}
			if ( pos_count == 0) {
				warning("Nothing selected");
				return;
			}
			/*
			  get list of items - should contain only one item
			*/
			XtVaGetValues(deflist, XmNitems, &itemlist, NULL);
			/*
			  put into character string
			*/
			XmStringGetLtoR(itemlist[*pos_list-1],charset,&choice);
			/*
			  see if it has a # sign on the end
			*/
			strcpy(string,choice);
			pound = strcx(string,"#");
			if ( pound == -1) {
				warning("Histogram not defined - cannot view");
				XtUnmanageChild(hdraw);
				return;
			}
			/*
			  grab the id number
			*/
			for (i=pound+1, j=0; i<strlen(string); i++, j++)
				hstring[j] = string[i];
			hstring[j] = '\0';
			/*
			  get id and make sure histogram exists (should...)
			*/
			sscanf(hstring,"%d",&hid);
#ifdef D0FLAVOR
			dummy = hexist_(&hid);
#else
			dummy = hexist(&hid);
#endif
			if (dummy == 0) {
				warning("Histogram not defined ");
			    return;
			}
			/*
			  get all the stuff corresponding to this hid
			*/
#ifdef D0FLAVOR
			chgive_
#else
			chgive
#endif
			(&hid,title,&lentitle,&nbins,&fbin,&lbin,&minb,&maxb,
					&hsumm,&mean,&sigma,hval,herr,&nentries);
			/*
			  set the right fields
			*/
			XmTextSetString(xhtitle,title);

			sprintf(string,"%d",hid);
			XmTextSetString(xhid,string);

			sprintf(string,"%d",nbins);
			XmTextSetString(xhnbin,string);

			sprintf(string,"%g",fbin);
			XmTextSetString(xhbinl,string);

			sprintf(string,"%g",lbin);
			XmTextSetString(xhbinh,string);

			/*
			  and delete old histo
			*/
#ifdef D0FLAVOR
			hdelet_(&hid);
#else
			hdelet(&hid);
#endif
			break;
		case 5:       /* save all hbook4 stuff to disk */
			xgetchar("Name of HBOOK4 output file to create:"," ",
				hfile,&status);
			ilen = strlen(hfile);
#ifdef D0FLAVOR
			chout_(hfile,&ilen);
#else
			chout(hfile,&ilen);
#endif
		    break;
		case 6:      /* clear histogram */
			listit = XmListGetSelectedPos(deflist,&pos_list,&pos_count);
			if ( !listit ) {
				warning("Please, only ONE at a time!");
				XtUnmanageChild(hdraw);
				return;
			}
			/*
			  only one at a time please - should never happen due to above
			*/
			if ( pos_count > 1 ) {
				warning("Please, only ONE at a time!");
				XtUnmanageChild(hdraw);
				return;
			}
			if ( pos_count == 0) {
				warning("Nothing selected");
				return;
			}
			/*
			  get list of items - should contain only one item
			*/
			XtVaGetValues(deflist, XmNitems, &itemlist, NULL);
			/*
			  put into character string
			*/
			XmStringGetLtoR(itemlist[*pos_list-1],charset,&choice);
			/*
			  see if it has a # sign on the end
			*/
			strcpy(string,choice);
			pound = strcx(string,"#");
			if ( pound == -1) {
				warning("Histogram not defined - cannot view");
				XtUnmanageChild(hdraw);
				return;
			}
			/*
			  grab the id number
			*/
			for (i=pound+1, j=0; i<strlen(string); i++, j++)
				hstring[j] = string[i];
			hstring[j] = '\0';
			/*
			  get id and make sure histogram exists (should...)
			*/
			sscanf(hstring,"%d",&hid);
#ifdef D0FLAVOR
			dummy = hexist_(&hid);
#else
			dummy = hexist(&hid);
#endif
			if (dummy == 0) {
				warning("Histogram not defined ");
			    return;
			}
			/*
			  clear histogram
			*/
#ifdef D0FLAVOR
			chreset_(&hid);
#else
			chreset(&hid);
#endif
			/*
			  and no functions please
			*/
			dofit = False;
			super2 = False;
			break;		
		case 7:			/* fit to gaussian using hbook fitting */
			/*
			  lots of checking before we do this:
			*/
			if (nbins<1) return;
			if (lentitle<1) return;
			if (hsumm<1.0) return;
			/*
			  ok, what do we want to fit this histo to?
			*/
			fittype = GAUSS;
			dummy = 10;
#ifdef D0FLAVOR
			hfitga_
#else
			hfitga
#endif
			(&hid,&xpar[0],&xpar[1],&xpar[2],&chi2,&dummy,sigpar);
			/*
			  overlay?
			*/
			if ( fitoverlay ) dofit = True;
			dummy = 0;
			d0xhdraw(wdumq,&dummy,uldumq);
			break;
		case 8:			/* fit to exponential using hbook fitting */
			/*
			  lots of checking before we do this:
			*/
			if (nbins<1) return;
			if (lentitle<1) return;
			if (hsumm<1.0) return;
			fittype = EXPO;
			dummy = 10;
#ifdef D0FLAVOR
			hfitex_
#else
			hfitex
#endif
			(&hid,&xpar[0],&xpar[1],&chi2,&dummy,sigpar);
			/*
			  overlay?
			*/
			if ( fitoverlay ) dofit = True;
			dummy = 0;
			d0xhdraw(wdumq,&dummy,uldumq);
			break;
		case 9:			/* fit to polynomial using hbook fitting */
			/*
			  lots of checking before we do this:
			*/
			if (nbins<1) return;
			if (lentitle<1) return;
			if (hsumm<1.0) return;
			fittype = POLY;
			dummy = 10;
			xgetchar("How many parameters? (degree+1)","2",hstring,&status);
			sscanf(hstring,"%d",&degr); npar = degr;
#ifdef D0FLAVOR
			hfitpo_
#else
			hfitpo
#endif
			(&hid,&degr,xpar,&chi2,&dummy,sigpar);
			/*
			  overlay?
			*/
			if ( fitoverlay ) dofit = True;
			dummy = 0;
			d0xhdraw(wdumq,&dummy,uldumq);
			break;
		case 10:        /* create tuple here */
			listit = XmListGetSelectedPos(deflist,&pos_list,&pos_count);
			if ( !listit ) {
				XtUnmanageChild(xnmake);
				warning("Please, only ONE at a time!");
				return;
			}
			/*
			  only one at a time please - should never happen due to above 
			*/
			if ( pos_count > 1 ) {
				XtUnmanageChild(xnmake);
				warning("Please, only ONE at a time!");
				return;
			}
			if ( pos_count == 0) {
				XtUnmanageChild(xnmake);
				warning("Nothing selected");
				return;
			}
			/*
			  get list of items - should contain only one item
			*/
			XtVaGetValues(deflist, XmNitems, &itemlist, NULL);
			/*
			  put into character string
			*/
			XmStringGetLtoR(itemlist[*pos_list-1],charset,&choice);
			/*
			  see if it has a # sign on the end
			*/
			strcpy(string,choice);
			if ( strcx(string,"#") != -1) {
				XtUnmanageChild(xnmake);
				warning("Tuple already requested for this definition");
				return;
			}
			strcpy(whatstring,string);
			/*
			  the id goes here:
			*/
			hid = nidnext;  nidnext++;
			sprintf(hstring,"%d",hid);
			XmTextSetString(xnid,hstring);
			break;
		case 11:        /* delete tuple here */
			listit = XmListGetSelectedPos(deflist,&pos_list,&pos_count);
			if ( !listit ) {
				warning("Please, only ONE at a time!");
				return;
			}
			/*
			  only one at a time please - should never happen due to above
			*/
			if ( pos_count > 1 ) {
				warning("Please, only ONE at a time!");
				return;
			}
			if ( pos_count == 0) {
				warning("Nothing selected");
				return;
			}
			/*
			  get list of items - should contain only one item
			*/
			XtVaGetValues(deflist, XmNitems, &itemlist, NULL);
			/*
			  put into character string
			*/
			XmStringGetLtoR(itemlist[*pos_list-1],charset,&choice);
			/*
			  see if it has a # sign on the end
			*/
			strcpy(string,choice);
			pound = strcx(string,"#");
			if ( pound == -1) {
				warning("Histogram not defined - cannot delete");
				return;
			}
			/*
			  ok, delete the # sign and put it back
			*/
			choice[pound] = '\0';
			tmp = XmStringCreateSimple(choice);
			XmListReplaceItems(deflist,&itemlist[*pos_list-1],1,&tmp);
			XmStringFree(tmp);
			/*
			  and delete the thing from hbook and tell D0X not to fill it
			*/
#ifdef D0FLAVOR
			hdelet_(&hid);
			chcann_(&hid);
#else
			hdelet(&hid);
			chcann(&hid);
#endif
			break;
		case 12:      /* clear tuple */
			listit = XmListGetSelectedPos(deflist,&pos_list,&pos_count);
			if ( !listit ) {
				warning("Please, only ONE at a time!");
				XtUnmanageChild(hdraw);
				return;
			}
			/*
			  only one at a time please - should never happen due to above
			*/
			if ( pos_count > 1 ) {
				warning("Please, only ONE at a time!");
				XtUnmanageChild(hdraw);
				return;
			}
			if ( pos_count == 0) {
				warning("Nothing selected");
				return;
			}
			/*
			  get list of items - should contain only one item
			*/
			XtVaGetValues(deflist, XmNitems, &itemlist, NULL);
			/*
			  put into character string
			*/
			XmStringGetLtoR(itemlist[*pos_list-1],charset,&choice);
			/*
			  see if it has a # sign on the end
			*/
			strcpy(string,choice);
			pound = strcx(string,"#");
			if ( pound == -1) {
				warning("Histogram not defined - cannot view");
				XtUnmanageChild(hdraw);
				return;
			}
			/*
			  grab the id number
			*/
			for (i=pound+1, j=0; i<strlen(string); i++, j++)
				hstring[j] = string[i];
			hstring[j] = '\0';
			/*
			  get id and make sure histogram exists (should...)
			*/
			sscanf(hstring,"%d",&hid);
#ifdef D0FLAVOR
			dummy = hexist_(&hid);
#else
			dummy = hexist(&hid);
#endif
			if (dummy == 0) {
				warning("Histogram not defined ");
			    return;
			}
			/*
			  clear histogram
			*/
#ifdef D0FLAVOR
			chreset_(&hid);
#else
			chreset(&hid);
#endif
			break;
		case 13:          /* init and fill hist from tuple */
			/*
			  histograms are serviced by case 2
			*/
			if (viewtype == HISTOGRAM) {
			  warning("NTUPLE/view only - try UPDATE instead");
			  return;
			}
			listit = XmListGetSelectedPos(deflist,&pos_list,&pos_count);
			if ( !listit ) {
				warning("Please, only ONE at a time!");
				XtUnmanageChild(hdraw);
				return;
			}
			/*
			  only one at a time please - should never happen due to above
			*/
			if ( pos_count > 1 ) {
				warning("Please, only ONE at a time!");
				XtUnmanageChild(hdraw);
				return;
			}
			if ( pos_count == 0) {
				warning("Nothing selected");
				return;
			}
			/*
			  get list of items - should contain only one item
			*/
			XtVaGetValues(deflist, XmNitems, &itemlist, NULL);
			/*
			  put into character string
			*/
			XmStringGetLtoR(itemlist[*pos_list-1],charset,&choice);
			/*
			  see if it has a # sign on the end
			*/
			strcpy(string,choice);
			pound = strcx(string,"#");
			if ( pound == -1) {
				warning("Histogram not defined - cannot view");
				XtUnmanageChild(hdraw);
				return;
			}
			/*
			  grab the id number
			*/
			for (i=pound+1, j=0; i<strlen(string); i++, j++)
				hstring[j] = string[i];
			hstring[j] = '\0';
			/*
			  get id and make sure histogram exists (should...)
			*/
			sscanf(hstring,"%d",&hidn);
#ifdef D0FLAVOR
			dummy = hexist_(&hidn);
#else
			dummy = hexist(&hidn);
#endif
			if (dummy == 0) {
				warning("Histogram not defined ");
			    return;
			}
			/*
			  get title
			*/
			equal = strcx(string,"=");
			string[equal] = '\0';
			/*
			  get the values from the text widgets
			*/
			choice = XmTextGetString(low1text);
			if (strlen(string) < 1) {
				warning("Must supply initial low edge bin 1");
				return;
			}
			sscanf(choice,"%f",&fbin);
			choice = XmTextGetString(hightext);
			if (strlen(choice) < 1) {
				warning("Must supply initial high edge last bin 1");
				return;
			}
			sscanf(choice,"%f",&lbin);
			choice = XmTextGetString(numtext);
			if (strlen(choice) < 1) {
				warning("Must supply initial number of bins");
				return;
			}
			sscanf(choice,"%d",&nbins);
			/*
			  set up the scales
			*/
			/*XtVaSetValues(low1scale, XmNvalue, (int) fbin, NULL);
			XtVaSetValues(highscale, XmNvalue, (int) lbin, NULL);*/
			XtVaSetValues(numscale, XmNvalue, nbins, NULL);
			/*
			  ok, book a new histo, get new data, and manage
			*/
		case 15:                 /* a cheat - called from 14 below */
			/*
			  cancel fit and superimpose
			*/
			dofit = False;
			super2 = False;
			/*
			  new histo to create
			*/
			hid = 100000;
			strcpy(title,string);
			lentitle = strlen(title);
			store = 0;
			/*
			  delete old one, book new one 
			*/
#ifdef D0FLAVOR
			dummy = hexist_(&hid);
			if (dummy == -1) hdelet_(&hid);
			chfilln_
#else
			dummy = hexist(&hid);
			if (dummy == -1) hdelet(&hid);
			chfilln
#endif
			(&hid,&hidn,title,&lentitle,&nbins,&fbin,&lbin,&store);
			/*
			  get all the stuff corresponding to this hid
			*/
#ifdef D0FLAVOR
			chgive_
#else
			chgive
#endif
			(&hid,title,&lentitle,&nbins,&fbin,&lbin,&minb,&maxb,
					&hsumm,&mean,&sigma,hval,herr,&nentries);
			dummy = 0;
			d0xhdraw(wdumq,&dummy,uldumq);
			break;
		case 14:      /* dynamic rebooking and refilling */
			strcpy(string,title);
			/*
			  get the values from the scales
			*/
			/*XmScaleGetValue(low1scale,&ival);
			fbin = ival;
			XmScaleGetValue(highscale,&ival);
			lbin = ival;*/
			XmScaleGetValue(numscale,&ival);
			nbins = ival;
			/*
			  put them into the proper text widgets
			*/
			/*
			  get the values from the text widgets
			*/
			/*sprintf(string,"%f",fbin);
			XmTextSetString(low1text,string);
			sprintf(string,"%f",lbin);
			XmTextSetString(hightext,string);*/
			sprintf(string,"%d",nbins);
			XmTextSetString(numtext,string);
			/*
			  and fill the histo
			*/
			dummy = 15;
			dxhisto(wdumq,&dummy,uldumq);
			break;
			/*
			  clear histogram
			*/
		case 16:   /* clear all */
			dummy = 0;
#ifdef D0FLAVOR
			chreset_(&dummy);
#else
			chreset(&dummy);
#endif
			break;
		case 17:        /* superimpose new histo here */
			listit = XmListGetSelectedPos(deflist,&pos_list,&pos_count);
			if ( !listit ) {
				warning("Please, only ONE at a time!");
				XtUnmanageChild(hdraw);
				return;
			}
			/*
			  only one at a time please - should never happen due to above
			*/
			if ( pos_count > 1 ) {
				warning("Please, only ONE at a time!");
				XtUnmanageChild(hdraw);
				return;
			}
			if ( pos_count == 0) {
				warning("Nothing selected");
				return;
			}
			/*
			  get list of items - should contain only one item
			*/
			XtVaGetValues(deflist, XmNitems, &itemlist, NULL);
			/*
			  put into character string
			*/
			XmStringGetLtoR(itemlist[*pos_list-1],charset,&choice);
			/*
			  see if it has a # sign on the end
			*/
			strcpy(string,choice);
			pound = strcx(string,"#");
			if ( pound == -1) {
				warning("Histogram not defined - cannot view");
				XtUnmanageChild(hdraw);
				return;
			}
			/*
			  grab the id number
			*/
			for (i=pound+1, j=0; i<strlen(string); i++, j++)
				hstring[j] = string[i];
			hstring[j] = '\0';
			/*
			  get id and make sure histogram exists (should...)
			*/
			sscanf(hstring,"%d",&hid2);
#ifdef D0FLAVOR
			dummy = hexist_(&hid2);
#else
			dummy = hexist(&hid2);
#endif
			if (dummy == 0) {
				warning("Histogram not defined ");
			    return;
			}
			/*
			  get all the stuff corresponding to this hid
			*/
#ifdef D0FLAVOR
			chgive_
#else
			chgive
#endif
			(&hid2,title2,&lentitle2,&nbins2,&fbin2,&lbin2,&minb2,&maxb2,
					&hsumm2,&mean2,&sigma2,hval2,herr2,&nentries2);
			/*
			  must have same attributes to be superimposed
			*/
			if ( (nbins2 == nbins) && (fbin2 == fbin) &&
				(lbin2 = lbin) ) {
				super2 = True;
				XtManageChild(hdraw);
			}
			else {
				warning("Histogram has different binning");
			}				
			break;
		case 20:          /* set id to begin with 1 - mark for filling
		                     histogram regardless of cuts */
			/*
			  get the id, make sure first digit is a 1
			*/
		    choice = XmTextGetString(xhid);
		    strcpy(string,choice);
		    squeeze(string,' ');
		    string[0] = '1';
			XmTextSetString(xhid,string);
			break;
		case 21:          /* set id to begin with 2 - mark for filling
		                     histogram after cuts */
			/*
			  get the id, make sure first digit is a 2
			*/
		    choice = XmTextGetString(xhid);
		    strcpy(string,choice);
		    squeeze(string,' ');
		    string[0] = '2';
			XmTextSetString(xhid,string);
			break;
		case 22:            /* process this ntuple */
			/*
			  get the ntuple id
			*/
			choice = XmTextGetString(xnid);
			if (strlen(choice) == 0) {
				warning("Please furnish an ntuple ID");
				return;
			}
			hid = atoi(choice);
			/*
			  ok, mark the selection with the # sign and put it back
			*/
			strcpy(string,whatstring);
			sprintf(hstring,"%d",hid);
			strcat(whatstring,"#");
			strcat(whatstring,hstring);
			tmp = XmStringCreateSimple(whatstring);
			XmListReplaceItems(deflist,&itemlist[*pos_list-1],1,&tmp);
			XmStringFree(tmp);
			/*
			  get the title
			*/
			equal = strcx(string,"=");
			string[equal] = '\0';
			lentitle = strlen(string);
			/*
			  and book it
			*/
#ifdef D0FLAVOR
			chbookn_
#else
			chbookn
#endif
			(&hid,string,&lentitle);
			/*
			  decode it
			*/
			if ( !decodeitem(whatstring,whatstring,1,
					dbank,dspec,&doffset,&dtype,&dop,dval) ) {
						warning("INTERNAL error, inconsistency, tell DREW");
						return;
				}
			/*
			  and tell D0X driver to go ahead and fill it
			*/
			slen = strlen(dspec);
#ifdef D0FLAVOR
			chsetn_
#else
			chsetn
#endif
				(&hid,dbank,dspec,&slen,&doffset,&dtype);
			break;
		case 30:          /* set id to begin with 1 - mark for filling
		                     ntuple regardless of cuts */
			/*
			  get the id, make sure first digit is a 1
			*/
		    choice = XmTextGetString(xnid);
		    strcpy(string,choice);
		    squeeze(string,' ');
		    string[0] = '1';
			XmTextSetString(xnid,string);
			break;
		case 31:          /* set id to begin with 2 - mark for filling
		                     ntuple after cuts */
			/*
			  get the id, make sure first digit is a 2
			*/
		    choice = XmTextGetString(xnid);
		    strcpy(string,choice);
		    squeeze(string,' ');
		    string[0] = '2';
			XmTextSetString(xnid,string);
			break;
		case 50:           /* we're viewing a histogram */
			viewtype = HISTOGRAM;
			break;
		case 51:           /* we're viewing an ntuple/histogram */
			viewtype = NTUPLE;
			break;
		case 60:           /* set timer for auto updating */
			timer_enabled = True;
			XmScaleGetValue(updtimer,&itime);
			if (itime < 1) {
				warning("Time interval must be positive definite");
				return;
			}
			itime = 1000*itime;   /* milliseconds */
			i = 62;
			dxhisto(wdumq,&i,uldumq);
			break;
		case 61:           /* cancel timer for auto updating */
			if (timer_enabled) {
				XtRemoveTimeOut(update_timer);
				timer_enabled = False;
		    }
		    else {
		    	warning ("Update Timer has not been enabled");
		    }
			break;
		case 62:     /* udpate and set timer again */
			i = 2;
			dxhisto(wdumq,&i,uldumq);
			update_timer = XtAppAddTimeOut(appcontext,itime,timeautoupdate,
                                                       (void*)(unsigned long)itime_tag);
			break;
		default:
			printf("Illegal TAG D0XHIST - Tell FNALD0::DREW\n");
			break;
	}
}

void 
timeautoupdate(tag)
int tag;
{
	int dummy = 62;
	dxhisto(wdumq,&dummy,uldumq);
}

void d0xhdraw(w,tag,reason)  /* this is the thing that draws the histogram
                           it's a 'expose' */
Widget		w;
int		*tag;
unsigned long *reason;
{
	char string[100];
	Position width,height;
	float dm, tdm, xmin, ynorm, val, xminh, z1,z2,z3;
	int xm, ym, di, ict, i, j, k, xdm, ye1, ye2, xmh, ym1, idm;
	int ytop,power,numsub, ip,dy;
	double frac, dbl;
	XPoint PointList[1000];
/*
  get fonts now
*/
	if ( qfont == 0 ) {
		qfont = XLoadQueryFont(XtDisplay(toplevel_widget),
				"-*-fixed-medium-r-*-*-*-120-*-*-*-*-*-*");
		qfontlist = XmFontListCreate(qfont,"charset");
	}
/*
  setup for graphics
*/  
	if (gc == 0) {
		hdisplay = XtDisplay(hdraw);
		hwindow = XtWindow(hdraw);
		qgcmask = GCForeground | GCBackground;
		gcv.foreground = BlackPixelOfScreen(XtScreen(hdraw));
		gcv.background = WhitePixelOfScreen(XtScreen(hdraw));
		gc = XCreateGC(hdisplay,hwindow,qgcmask,&gcv);
	}
/*
  get height and width of draw widget
*/
	XtVaGetValues(hdraw, XmNwidth, &width, XmNheight, &height, NULL); 
/*
  start rectangle at xx1,yy1, xx2,yy2
*/
	xx1 = 50; yy1 = 80;
	xx2 = width - 30;
	yy2 = height - 30;
/*
  2-d not supported (yet?)
*/
	if (nbins < 0) {   /* 2-d histo */
		XDrawString(hdisplay,hwindow,gc,100,height/2,
		"2-D Histograms NOT SUPPORTED (YET)",34);
		return;
	}
	XClearWindow(hdisplay,hwindow);
/*
  draw the title and statistics
*/
	sprintf(string,"id=%d",hid);
	XDrawString(hdisplay,hwindow,gc,20,15,string,strlen(string));
	if (lentitle) 
			XDrawString(hdisplay,hwindow,gc,100,15,title,lentitle);
	sprintf(string," Entries: %.0f",hsumm);
	XDrawString(hdisplay,hwindow,gc,width-100,15,string,strlen(string));
	sprintf(string," Mean: %.4g",mean);
	XDrawString(hdisplay,hwindow,gc,width-100,27,string,strlen(string));
	sprintf(string," Sigma: %.4g",sigma);
	XDrawString(hdisplay,hwindow,gc,width-100,39,string,strlen(string));
	sprintf(string," Min: %g",minb);
	XDrawString(hdisplay,hwindow,gc,width-100,51,string,strlen(string));
	sprintf(string," Max: %g",maxb);
	XDrawString(hdisplay,hwindow,gc,width-100,63,string,strlen(string));
/*
  if it's empty, return
*/
	if ( hsumm < 1.0 ) return;
/*
  set solid lines and draw rectangle around border
*/
	XSetLineAttributes(hdisplay,gc,0,LineSolid,CapButt,JoinMiter);
	XDrawRectangle(hdisplay,hwindow,gc,xx1,yy1,xx2-xx1,yy2-yy1);
/*
  normalize to highest bin
*/
	if ( DOLINEAR ) {
		if (maxb < 10 ) {
			ynorm = .8*yy2/10.;
			ytop = 10;
		}
		else {
			ynorm = .8*yy2/maxb;
			ytop = maxb;
		}
	}
	else {
		if (maxb < 10 ) {
			ynorm = .8*yy2;
			ytop = 1;
		}
		else {
			dbl = maxb;
			ynorm = .8*yy2/log10(dbl);
			scntfc(dbl,&frac,&power);
			ytop = power;
		}
	}
	XSetFillStyle(hdisplay,gc,FillSolid);   
	ym1 = yy2;
	xdm = (xx2-xx1)/nbins;        /* integer bin width in pixels */
	dm = (lbin-fbin)/nbins;    /* dm is float bin width */
	xm = xx1;
	xmin = fbin;
	di = nbins/10;
/*
  each bin should generate 2 sets of points: left and right edge of bin
*/
	ip = 0;
	PointList[ip].x = xm; PointList[ip].y = ym1; ip++;
	for (i=0, ict=di; i<nbins; i++, ict++) {
		if (DOLINEAR) ym = ynorm*hval[i];
		else {
			if (hval[i] > 0.0) ym = ynorm*log10( (double) hval[i]);
			else ym = 5;
		}
		xmh = xm + .5*xdm;
		PointList[ip].x = xm; PointList[ip].y = ym1-ym; ip++;
		PointList[ip].x = xm+xdm; PointList[ip].y = ym1-ym; ip++;
		if (ym > 0) {
			if (!DOHIST) XDrawRectangle(hdisplay,hwindow,gc,
					xmh-3,ym1-ym-3,6,6);
			if (DOERRORS) {
				if (DOLINEAR) {
					ye1 = ym + herr[i]*ynorm;
					ye2 = ym - herr[i]*ynorm;
				}
				else {
					ye1 = ynorm*log10( (double) hval[i]+herr[i]);
					ye2 = ynorm*log10( (double) hval[i]-herr[i]);
					if (ye1 > ym1) ye1 = ym1;
					if (ye2 < 0) ye2 = 0;
				}
				XDrawLine(hdisplay,hwindow,gc,
				xmh,yy2-ye1,xmh,yy2-ye2);
			}
		}
		/*
		  put up 10 tick marks
		*/
		if (ict == di) {
			ict = 0;
			XDrawLine(hdisplay,hwindow,gc,xm,yy2+5,xm,yy2);
			sprintf(string,"%.1f",xmin);
			XDrawString(hdisplay,hwindow,gc,xm,yy2+15,string,strlen(string));
		}			
		xm += xdm;
		xmin += dm;
	}
/*
  put one more point to get down to the x axis
*/
	PointList[ip].x = xm; PointList[ip].y = ym1; ip++;
/*
  put one last point to get back to the origin
*/
	PointList[ip].x = xx1; PointList[ip].y = ym1; ip++;
/*
  draw the histogram using XPoints
*/
	if (DOHIST) 
		XDrawLines(hdisplay,hwindow,gc,PointList,ip,CoordModeOrigin);
/*
  put ticks on y axis
*/
	if (DOLINEAR) {
		if ( ytop > 10) {
			scntfc((double) ytop,&frac,&i);
			dbl = 10.;
			val = 1.2*frac*pow(dbl, (double) i)/10.;
			dy = val;
		}
		else {
			dy = 1;
		}
		ym1 = yy2;
		val = 0;
		for (i=0; i<10; i++) {
			ym = ym1 - ynorm*val;
			XDrawLine(hdisplay,hwindow,gc,xx1-5,ym,xx1,ym);
			j = val;
			sprintf(string,"%d",j);
			XDrawString(hdisplay,hwindow,gc,5,ym,string,strlen(string));
			val += dy;
		}
	}
	else {
		ym1 = yy2 - 5;
		for (i=1; i<=ytop; i++) {
			val = i;
			ym = ym1 - ynorm*val;  
			XDrawLine(hdisplay,hwindow,gc,xx1-5,ym,xx1,ym);
			sprintf(string,"10**%d",i);
			XDrawString(hdisplay,hwindow,gc,5,ym,string,strlen(string));
		}			
	}
/*
  superimpose another? do just like the first, use first binning and no
  errors
*/
	if (super2) {
/*
  		set dashed lines 
*/
		XSetLineAttributes(hdisplay,gc,0,LineOnOffDash,CapButt,JoinMiter);
		ip = 0;
		PointList[ip].x = xx1; PointList[ip].y = ym1; ip++;
		ym1 = yy2;
		xm = xx1;
		xmin = fbin;
		for (i=0; i<nbins; i++, ict++) {
			if (DOLINEAR) ym = ynorm*hval2[i];
			else {
				if (hval2[i] > 0.0) ym = ynorm*log10( (double) hval2[i]);
				else ym = 5;
			}
			xmh = xm + .5*xdm;
			PointList[ip].x = xm; PointList[ip].y = ym1-ym; ip++;
			PointList[ip].x = xm+xdm; PointList[ip].y = ym1-ym; ip++;
			xm += xdm;
			xmin += dm;
		}
		PointList[ip].x = xm; PointList[ip].y = ym1; ip++;
/*
  	draw the histogram using XPoints
*/
		XDrawLines(hdisplay,hwindow,gc,PointList,ip,CoordModeOrigin);
	}
/*
    ok, draw the function
*/
	if ( dofit ) {
		/*
		  printout fit results
		*/
		if (fittype == GAUSS) {
			sprintf(string," Fit C*exp(-.5[(x-AV)/S]**2) - chi**2 = %g",chi2);
			XDrawString(hdisplay,hwindow,gc,20,27,string,strlen(string));
  			sprintf(string,"  C = %g +- %g",xpar[0],sigpar[0]);
			XDrawString(hdisplay,hwindow,gc,20,39,string,strlen(string));
  			sprintf(string," AV = %g +- %g",xpar[1],sigpar[1]);
			XDrawString(hdisplay,hwindow,gc,20,51,string,strlen(string));
  			sprintf(string," S = %g +- %g",xpar[2],sigpar[2]);
			XDrawString(hdisplay,hwindow,gc,20,63,string,strlen(string));
		}
		else if (fittype == EXPO) {
			sprintf(string," Fit exp(A+Bx) - chi**2 = %g",chi2);
			XDrawString(hdisplay,hwindow,gc,20,27,string,strlen(string));
			sprintf(string,"  A = %g +- %g",xpar[0],sigpar[0]);
			XDrawString(hdisplay,hwindow,gc,20,39,string,strlen(string));
			sprintf(string,"  B = %g +- %g",xpar[1],sigpar[1]);
			XDrawString(hdisplay,hwindow,gc,20,51,string,strlen(string));
		}
		else if (fittype == POLY) {
			sprintf(string," Poly Fit - chi**2 = %g Parameters are:",chi2);
			XDrawString(hdisplay,hwindow,gc,20,27,string,strlen(string));
			for (i=0,j=39; i<npar; i++) {
			  sprintf(string,
			  	" parameter %2d result: %g +- %g",i,xpar[i],sigpar[i]);
			  j += 12;
			  XDrawString(hdisplay,hwindow,gc,20,j,string,strlen(string));
			}
		}
		ym1 = yy2;
		xdm = (xx2-xx1)/nbins;        /* integer bin width in pixels */
		dm = (lbin-fbin)/nbins;    /* dm is float bin width */
		numsub = 10;
		if (numsub > xdm) numsub = xdm;
		tdm = dm/numsub;
		idm = xdm/numsub;
		xm = xx1;
		xmin = fbin;
		di = nbins/10;
/*
  		loop over bins, put up 10 points in each bin
*/
		for (ip=0,i=0; i<nbins; i++, ict++) {
		  for (j=0; j<numsub; j++) {
		  	val = j;
			xminh = xmin + tdm*val;
/*
  			calculate value using fit
*/
			if (fittype == GAUSS) {
				if ( xpar[2] < 0.0000001 ) return;
				z2 = xpar[2]*xpar[2];
				z1 = xpar[1]-xminh;
				z3 = -.5*z1*z1/z2;
				dbl = xpar[0]*exp(z3);
			}
			else if (fittype == EXPO) {
				z1 = xpar[0] + xminh*xpar[1];
				dbl = exp(z1);
			}
			else if (fittype == POLY) {
				dbl = 0.;
				for (k=0; k<npar; k++) 
					dbl = dbl + xpar[k]*pow( (double) xminh, (double) k);
			}
			val = dbl;
			if (DOLINEAR) ym = ynorm*val;
			else {
				if (val > 0.0) ym = ynorm*log10( (double) val);
				else ym = 5;
			}
			xmh = xm + idm*j;
/*			if (ym > 0) 
			    XDrawPoint(hdisplay,hwindow,gc,xmh,ym1-ym);*/
			PointList[ip].x = xmh; PointList[ip].y = ym1-ym; ip++;
		  }
		  xm += xdm;
		  xmin += dm;
		}
		XDrawLines(hdisplay,hwindow,gc,PointList,ip,CoordModeOrigin);
	}
}


int
strcx(str,ch)  /* returns the position of ch in str */
char *str, *ch;
{
	int i;
	
	for (i=0; i<strlen(str); i++) 
		if (str[i] == *ch) return(i);
	return(-1);
}


/*
  here are some utility functions
*/
Boolean sunique(list,string)
Widget list;
char *string;
{
	XmString *strlist;
	char *tstring, str[40];
	int number, i, equal;
	/*
	  number of items in list
	*/
	XtVaGetValues(list, XmNitemCount, &number, NULL);
	if ( number < 1 ) return(True);
	/*
	  get list of items
	*/
	XtVaGetValues(list, XmNitems, &strlist, NULL);
	/*
	  loop over them, strip off the first part before the = sign,
	  and compare
	*/
	for (i=0; i<number; i++) {
		XmStringGetLtoR(strlist[i], charset, &tstring);
		strcpy(str,tstring);
		equal = strcx(str,"=");
		str[equal] = '\0';
		if ( strcmp(str,string) == 0 ) return(False);
	}
	
	return(True);
	
}

void
nomoref(w,tag,reason)
Widget		w;
XtPointer tag;
XmFileSelectionBoxCallbackStruct *reason;
{
		XtUnmanageChild(reqfile);	
}

void
openreq(w,tag,reason)
Widget		w;
XtPointer tag;
XmFileSelectionBoxCallbackStruct *reason;
{
	int single,equal,dollar,bracket;
	XmString tmp;
	char string[100], *filename;
/*
    get file name
*/
	XmStringGetLtoR(reason->value, XmSTRING_DEFAULT_CHARSET, &filename);
	if ( !*filename ) {
		warning("No filename selected");
		XtUnmanageChild(reqfile);
		return;
	}
/*
    ok, unmanage the file selection widget and open the file
*/
	XtUnmanageChild(reqfile);
	fqpo = fopen(filename,"r");
/*
	  look for "!DONE" to end
*/
	while (single = fgets(string,80,fqpo) != NULL) {
		/*
		  filter out comments
		*/
		if ( strcx(string,"!") == -1) {
			/*
			  not a comment - is this a requirement which is implemented?
			*/
			equal = strcx(string,"=");
			bracket = strcx(string,"{");
			dollar = strcx(string,"$");
			if ( dollar != -1 ) {
				/*
				  operations
				*/
				/*
				  is it POSITIVE or NEGATIVE
				*/
				if ( strcmp(string,"$POSITIVE") == 0) {
					doreqtf = POSITIVE;
					XtVaSetValues(rpositive, XmNset, True, NULL);
					XtVaSetValues(rnegative, XmNset, False, NULL);
				}
				else if ( strcmp(string,"$NEGATIVE") == 0) {
					doreqtf = NEGATIVE;
					XtVaSetValues(rpositive, XmNset, False, NULL);
					XtVaSetValues(rnegative, XmNset, True, NULL);
				}
				else if ( strcmp(string,"$AND") == 0) {
					doandor = AND;
					XtVaSetValues(wrand, XmNset, False, NULL);
					XtVaSetValues(wror, XmNset, True, NULL);
				}
				else if ( strcmp(string,"$OR") == 0) {
					doreqtf = OR;
					XtVaSetValues(wrand, XmNset, False, NULL);
					XtVaSetValues(wror, XmNset, True, NULL);
				}
				else if ( strcmp(string,"$STOP") == 0) {
					dostop = STOP;
					XtVaSetValues(rstop, XmNset, False, NULL);
					XtVaSetValues(rcont, XmNset, True, NULL);
				}
				else if ( strcmp(string,"$CONTINUE") == 0) {
					dostop = NOSTOP;
					XtVaSetValues(rstop, XmNset, False, NULL);
					XtVaSetValues(rcont, XmNset, True, NULL);
				}
			}
			else if ( bracket != -1 ) {
				/*
				  #banks requirements list
				*/
		 		tmp = XmStringCreateSimple(string);
		 		if ( !XmListItemExists(reqlists,tmp) ) {
		 			nreqs++;
		 			XmListAddItemUnselected(reqlists,tmp,nreqs);
		 		}
		 		XmStringFree(tmp);
			}
			else {
				if ( string[equal+1] == '=') {
					/*
					  definitions list
					*/
					tmp = XmStringCreateSimple(string);
					if ( !XmListItemExists(deflist,tmp) ) {
						ndefs++;
						XmListAddItemUnselected(deflist,tmp,ndefs);
					}
					XmStringFree(tmp);
				}
				else {
					/*
					  req1 list
					*/
					tmp = XmStringCreateSimple(string);
					if ( !XmListItemExists(req1list,tmp) ) {
						n1reqs++;
						XmListAddItemUnselected(req1list,tmp,n1reqs);
					}
				}
			}
		}
	}
	fclose(fqpo);
}

Boolean decodeitem(rstring,dstring,
	mode,dbank,dspecial,doffset,dtype,dop,dval)
char *dstring,*rstring,*dbank,*dval,*dspecial;
int *doffset,*dtype,*dop,mode;
{
	char temp[100];
	int equal,equal2,paren1,paren2,type1,type2,bracket1,bracket2;
	int lbrack,rbrack,lenr;
	int i,j,k,i1;
	/*
	  dstring should be of the form 
	  NAME==BANK[PARAM](OFFSET)|TYPE|
	  rstring should be of the form
	  NAME>20
	  for mode=0, decode both, for mode=1, decode only dstring
	*/
	if ( (equal = strcx(dstring,"=")) == -1) return(False);
	if ( (paren1 = strcx(dstring,"(")) == -1) return(False);
	if ( (paren2 = strcx(dstring,")")) == -1) return(False);
	if ( (type1 = strcx(dstring,"|")) == -1) return(False);
	lbrack = strcx(dstring,"[");
	if (lbrack == -1) i1 = paren1;
	else i1 = lbrack;
	/*
	  dig out bank name
	*/
	i = equal + 2;     /* i points to bank name */
	for (j=0; i<i1; i++, j++) 
		dbank[j] = dstring[i];
	dbank[j] = '\0';
	i++;                /* i points to beginning of offset or param*/
	/*
	  if there's a special parameter, dig it out now
	*/
	if ( lbrack > -1) {
		for (j=0; i<strcx(dstring,"]"); i++, j++) {
			dspecial[j] = dstring[i];	
		}
		dspecial[j] = '\0';
		i++;
		i++;
	}
	else dspecial[0] = '\0';
	/*
	  dig out offset
	*/
	for (j=0; i<paren2; i++, j++)
		temp[j] = dstring[i];
	temp[j] = '\0';
	sscanf(temp,"%d",&k);
	*doffset = k;
	i++;                /* i points to first | */
	i++;                /* i points to beginning of type */
	/*
	  dig out data type
	*/
	for (j=0; dstring[i]!='|'; i++, j++)
		temp[j] = dstring[i];
	temp[j] = '\0';
	if ( strcmp(temp,"REAL") == 0 ) k = REAL;
	else if (strcmp(temp,"INTEGER") == 0) k = INTEGER;
	else if (strcmp(temp,"HEX") == 0) k = HEX;
	else if (strcmp(temp,"BOOLEAN") == 0) k = BOOLEAN;
	else if (strcmp(temp,"CHAR4") == 0) k = CHAR4;
	*dtype = k;
	/*
	  if mode is 1, then we're done
	*/
	if (mode == 1) return(True);
	/*
	  now dig out the operation and value.  rstring is of the
	  form ETELE<21.00
	*/
	lenr = strlen(rstring);
	for (i=0; i<lenr; i++) {
		k = isalpha(rstring[i]);
		if ( k == 0 ) break;
	}			
	/*
	  i should now point to the first non-alpha character.
	  all ops are 2 characters
	*/
	temp[0] = rstring[i]; i++;
	temp[1] = rstring[i]; i++;
	temp[2] = '\0';
	if ( strcmp(temp,"< ") == 0) k = ISLT;
	else if ( strcmp(temp,"<=") == 0) k = ISLE;
	else if ( strcmp(temp,"> ") == 0) k = ISGT;
	else if ( strcmp(temp,">=") == 0) k = ISGE;
	else if ( strcmp(temp,"= ") == 0) k = ISEQ;
	else if ( strcmp(temp,"!=") == 0) k = ISNE;
	else if ( strcmp(temp,"T ") == 0) k = ISTRUE;
	else if ( strcmp(temp,"F ") == 0) k = ISFALSE;
	*dop = k;
	/*
	  dig out value
	*/
	for (j=0; i<lenr; i++, j++)
		dval[j] = rstring[i];
	dval[j] = '\0';
	/*
	  make sure there are no trailing control characters
	*/
	for (j=0; j<strlen(dval); j++) 
		if ( dval[j] == NL ) dval[j] = '\0';
	return(True);		
}


Boolean getdefstring(string,defstring) /* get the definition which has the same
                                    name as what's in string and return it */
char *string, *defstring;
{
	int nitemd, i, j, k, rlen, equal;
	XmString *itemdeflist;
	char reqstring[100], rdef[100], ddef[100], *singledef;
	Boolean foundit;
	/*
	  dig out definitions first
	*/
	XtVaGetValues(deflist, XmNitems, &itemdeflist, 
		XmNitemCount, &nitemd, NULL);
	if ( nitemd != ndefs ) {
		return(False);
	}
	/*
	  get the length of the definition name (ugly)
	*/
	for (k=0, rlen=0; k<strlen(string); k++) {
		if ( isalpha(string[k]) ) {
			rdef[rlen] = string[k];
			rlen++;
		}
	}
	rdef[rlen] = '\0';
	/*
	  loop over definitions, find the right one
	*/
	foundit = False;
	for (j=0; j<ndefs; j++) {
		XmStringGetLtoR(itemdeflist[j],charset,&singledef);
		strcpy(defstring,singledef);
		equal = strcx(defstring,"=");
		strncpy(ddef,defstring,equal);
		ddef[equal] = '\0';
		/*
		  now we have the definition name - see if it compares
		  with the requirement definition name
		*/
		dummy = strcmp(rdef,ddef);
		if (dummy == 0) {
			foundit = True;
			break;
		}
	}
	return(foundit);
}

Boolean getdefbank(defstring,bank)
char *bank, *defstring;
{
	int i, j, equal, paren1, lbrack, i1;
	
	if ( (equal = strcx(defstring,"=")) == -1) return(False);
	if ( (paren1 = strcx(defstring,"(")) == -1) return(False);
	lbrack = strcx(defstring,"[");
	if (lbrack == -1) i1 = paren1;
	else i1 = lbrack;
	/*
	  dig out bank name
	*/
	i = equal + 2;     /* i points to bank name */
	for (j=0; i<i1; i++, j++) 
		bank[j] = defstring[i];
	bank[j] = 0;

	return(True);
}


Boolean getrdef(rstring,string,num)
/* get the "num"th (starting at 1) requirement 
 on definition from this entire string */
char rstring[],string[];
int num;
{
	int i, ic, last, j, k, ampers, l1, l2;
	/*
	  how many &s are there?
	*/
	for (i=0, ampers=0; i<strlen(rstring); i++) 
		if ( rstring[i] == '&') ampers++;
	/*
	  if there are none, then we just dig out what's between the { and }
	*/
	if ( ampers == 0 ) {
		l1 = strcx(rstring,"{") + 1;
		l2 = strcx(rstring,"}");
		for (i=l1,j=0; i<l2; i++,j++)
			string[j] = rstring[i];
		string[j] = '\0';
		return(True);
	}
	/*
	  at least one of them - loop
	*/
	for (ic=0,i=1,last=1; i<strlen(rstring); i++) {
		if ( rstring[i] == '&') {
			ic++;
			if (ic == num) {
				/*
				  ok, decode the string, from last to i-1
				*/
				for (j=last,k=0; j<i; k++,j++) 
					string[k] = rstring[j];
				string[i-last-1] = '\0';
				return(True);
			}
			else {
				last = i+1;
			}
		}
		else if ( rstring[i] == '}' ) {
			for (j=last,k=0; j<i; k++,j++) 
				string[k] = rstring[j];
			string[i-last-1] = '\0';
			return(True);
		}
	}
	/*
	  if we got here, there's been a BIG mistake
	*/
	return(False);
}

Boolean getopval2(string,op,dval)
/* strings of form {...}>3 - returns operation (>) and value (3) */
char *string,*dval;
int *op;
{
	int i,j,k,bracket1, bracket2, len;
	char temp[10];
	
	if ( (bracket1 = strcx(string,"{")) == -1) return(False);
	if ( (bracket2 = strcx(string,"}")) == -1) return(False);
	if ( (len = strlen(string)) < 1) return(False);

	/*
	  start at bracket2+1, op is 2 characters
	*/
	temp[0] = string[bracket2+1];
	temp[1] = string[bracket2+2];
	temp[2] = '\0';
	if ( strcmp(temp,"< ") == 0) k = ISLT;
	else if ( strcmp(temp,"<=") == 0) k = ISLE;
	else if ( strcmp(temp,"> ") == 0) k = ISGT;
	else if ( strcmp(temp,">=") == 0) k = ISGE;
	else if ( strcmp(temp,"= ") == 0) k = ISEQ;
	else if ( strcmp(temp,"!=") == 0) k = ISNE;
	else if ( strcmp(temp,"T ") == 0) k = ISTRUE;
	else if ( strcmp(temp,"F ") == 0) k = ISFALSE;
	*op = k;
	/*
	  and from here get the value
	*/
	for (i=bracket2+3, j=0; i<len; i++,j++) {
		dval[j] = string[i];
	}
	dval[j] = '\0';
	for (j=0; j<strlen(dval); j++) 
		if ( dval[j] == NL ) dval[j] = '\0';
	return(True);		
}			
void scntfc(value,frac,power)  /* returns the power.  and fraction ala scientific.
                             e.g. scntfc(924.1, frac, power) returns .9241 as
                             the fraction and 3 as the power */
double value,*frac;
int *power;
{
	double ds,q;
	int i;
	
	ds = log10(value);
	i = ds;
	q = 10.0*pow(10.,(double) i);
	i++;
	*frac = value/q;
	*power = i;
}
