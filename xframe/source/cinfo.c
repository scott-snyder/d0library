/* 
        cinfo.c
         Created           : 29-DEC-1992 by Drew Baden
*/
 
#include <stdio.h>                   /* I/O definitions                       */
 
#include "/d0lib/scratch/xframe/source/d0x_c.h"

cinfo(w,tag,reason)               /* help - pops up own widget */
Widget		w;
int		*tag;
unsigned long	*reason;
{

	Widget temp;
	int select = *tag;
	int n;
	Arg wargs[10];
	
	switch (select) {
		case 0:              /* physics help */
			n = 0;
			XtSetArg(wargs[n], XmNdialogTitle,
			  XmStringCreateSimple("Physics Help")); n++;
			XtSetValues(physhelp_bull, wargs, n);
			temp = curtext;
			curtext = physhelp;
			cerrmsg(
"This window displays information  about the  physics  objects which are\n\
present  in  the ZEBRA  event.  Physics  objects  are electrons (PELC),\n\
gammas (PPHO),  muons (PMUO), 'neutrinos' (PNUT)  and jets (JETS).  The\n\
objects  are  all  fetched using  the GZ* routines,  so if  you want to\n\
change the ZEBRA PATH, do so in the previous main window (ZEBRA PANEL).\n\
Each of the columns will  display information  which can  be changed by\n\
hand provided that you enter the change via a carriage return.\n\n\
Each of the physics objects has a toggle button which  is used in along\n\
with one of the buttons in the bottom left hand set:\n\n\
  o ADD 4-Vectors      select the physiscs objects via the toggle button\n\
                       and push this and a new object is created as a 4-\n\
                       vector  sum of the selected objects.  Here is how\n\
                       we define the quantities:\n\n\
                       E = sum(E), px/py/pz = sum(px/py/pz) over particles\n\n\
                       Et = E*sin(theta)  cos(theta) = pz/p\n\n\
                       Mt = sqrt(SEt**2 - pt**2) where SEt is Sum(Et), e.g.\n\n\
                            SEt = Et1+Et2.\n\n\
                       Note that there is a subtety here:  Mt needs the sum\n\
                       over Et for each particle, but the final Et is given\n\
                       by E*sin(theta).  BE AWARE!!!\n\n\
  o Delete             deletes selected objects\n\n\
  o Calculate Angles   only relevant for 2  selected  objects, calculates\n\
                       the space angle (alpha)  and difference in phi and\n\
                       theta between the two objects and displays results\n\
                       towards the top of the display.\n\n\
  o Apply Scale Factor in the slide bar under 'Scale by:', you set a scale\n\
                       factor value (default  =  1.00).  Using  the toggle\n\
                       button for each physics object,  select  the object\n\
                       and  press  'Apply Scale Factor'  to apply  the new\n\
                       factor. This is intended to let Boaz vary the scale\n\
                       to see if he can find tops!\n\n\
  o W Mass Constrain   Select two  physics objects, one of which  HAS to be\n\
                       the PNUT bank, and push this button and the two will\n\
                       be constrained to  make a  W mass  particle decaying\n\
                       into these two.   Note that  BOTH solutions  will be\n\
                       presented, using FIND_WLNU courtesy of Serban.\n\n\
Control for objects buttons:\n\n\
  o PELC/PPHO    Brings up a display of PELC/PPHO stuff (for instance\n\
                 longitudinal profile) and additional quantities.\n\n\
  o JETS         Brings up a display which allows you to:\n\n\
  		- Jet algorithm      To change which jet algorithm to use (cone\n\
                             cut or nearest neighbor)\n\n\
        - Jet ET Scale corrections\n\
                             Apply jet energy scale corrections using \n\
                             JET_ET_MCCORR until Blazey says otherwise\n\n\
        - Remove PELC/PPHO Candidates:\n\
                             This will use the JNEP bank values instead\n\
                             of the JETS values.\n\n\
  o PNUT         To change which PNUT bank to display\n\n\
  o PMUO         Nothing yet\n\n\
Additional buttons:\n\
  o Et Threshold    A slider used to set an ET threshold (bottom middle).\n\n\
  o Refresh         After any changes (see above or below) hit this to\n\
                    implement - button is near upper left corner.\n\n\
The following additiona buttons are located lower right corner:\n\n\
  o Event Display      calls up a 'quick-and-dirty' event display\n\
  o Refresh            remakes the list of objects from scratch\n\
  o Next Event         reads in the next event\n\
  o Help               this\n\n\
In addition, you can control which variables to include in the list by\n\
choosing the item with the toggle buttons on the top.");
			curtext = temp;
			break;
		case 1:          /* help on display */
			n = 0;
			XtSetArg(wargs[n], XmNdialogTitle,
			  XmStringCreateSimple("Physics Help")); n++;
			XtSetValues(physhelp_bull, wargs, n);
			temp = curtext;
			curtext = physhelp;
			cerrmsg(
"This is a quick-and-dirty display, meant to augment the real D0 display\n\
program.  The views are self-explanatory with the following additions:\n\n\
   o Refresh           - refreshes the display.  Turns out that due to the\n\
                         way that X/Windows works, you can also refresh by\n\
                         clicking on some other window and then back here.\n\n\
   o 'lego display'    - jets are  shown  with circles  which indicate the\n\
                         RMS width of the jet in  eta/phi space, otherwise\n\
                         physics objects' locations are indicated by their\n\
                         id  (correlate  with PHYSICS list window).  Also,\n\
                         the vertex is displayed with  a *  using the Zvtx\n\
                         and the face of the calorimeter at 85cm, z=0.\n\n\
   o 'rotate'          - this is a full EULER rotation - hold down the \n\n\
                         buttons for continuous rotation.\n");
			curtext = temp;
			break;
		default:
			break;
		}
		
}
