/*
        cinfo.c
         Created           : 29-DEC-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "xframe/source/d0x_c.h"

void cinfo(w,tag,reason)               /* help - pops up own widget */
Widget        w;
int        *tag;
unsigned long    *reason;
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
present  in  the ZEBRA  event.  Physics  objects  are electrons  (PELC),\n\
photons (PPHO),  muons (PMUO), taus (PTAU), jets (JETS), and 'neutrinos'\n\
(PNUT).  The objects  are  all fetched using the GZ* routines, so if you\n\
want to  change  the  ZEBRA  PATH,  do  so  in  the previous main window\n\
(ZEBRA PANEL). Each of the columns will display information which can be\n\
changed by hand provided that you enter the change via a carriage return.\n\n\
Each of the physics objects has a toggle button which  is used in along\n\
with one of the  buttons in the bottom left hand (under the label 'From\n\
Selections') set:\n\n\
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
  o Apply Scale Factor in the slide bar ('Energy Scale'), you  set a scale\n\
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
Under the label 'More on Objects...' are control buttons for objects:\n\n\
  o ELE          Brings  up  a  display  of  PELC stuff (for instance\n\
                 longitudinal profile) and additional quantities. You\n\
                 must  select  a  pmuo candidate from the Main Object\n\
                 Table before hitting this button.\n\n\
  o PHOTON       Same as above, only for PPHO.\n\n\
  o MUON         Displays PMUO stuff.  You must select a pmuo candidate\n\
                 from the Main Object Table before hitting this button.\n\n\
  o TAU          Displays PTAU stuff.  You must select a ptau candidate\n\
                 from the Main Object Table before hitting this button.\n\n\
  o JETS         Brings up a display which controls:\n\n\
        - Jet ET Corrections\n\
                             Apply jet energy scale corrections using\n\
                             QCD_JET_CORRECTION for both data and MC.\n\
                    ***NOTE*** Missing ET is corrected appropriately,\n\
                    with the following 2 caveats:\n\
                       1. jets which have an ET below the threshold\n\
                          (read  further  on threshold cuts)\n\
                       2. jets which have a JNEP bank, with JNEP enabled\n\
                          (see Remove PELC/PPHO below)\n\n\
          ***************************************************************\n\
          * Note:  If you have turned on CAFIX, then you will NOT be    *\n\
          *        allowed to turn this option on!                      *\n\
          ***************************************************************\n\n\
        - Corrections to Apply\n\
                             For togglin which corrections to apply here.\n\n\
        - Jet algorithm      To change which jet algorithm to use (cone\n\
                             cut or nearest neighbor)\n\n\
        - Remove PELC/PPHO Candidates:\n\
                             This will use the JNEP bank values instead\n\
                             of the JETS values.\n\
        - If you have 'selected' a jet in the object list, additional\n\
          JETS bank information is displayed in the text window.\n\n\
  o MET          To change which PNUT bank to display + more PNUT stuff.\n\
  o Enable/Disable  Toggles objects for inclusion in physics table.\n\n\
  o Tracks       Brings up another window where you can display detailed\n\
                 information for the CD, VTX, and FDC track banks.\n\n\
Additional buttons:\n\
  o Refresh         After any changes (see above or below) hit this to\n\
                    implement - button is near upper left corner.\n\n\
  o Help            Prints this\n\n\
  o Et Threshold    A slider used to set an ET threshold (bottom middle).\n\
                    Note that this threshold is applied to the LEGO display\n\
                    for both 'data' and MC' (see below)\n\n\
  o Energy Scale    A slider used to change the energy scale (see above\n\
                    on 'Apply Scale Factor').\n\n\
The following additional buttons are located under the label 'Physics\n\
Controls'\n\n\
  o Event Display   calls up a 'quick-and-dirty' event display\n\
  o Next Event      reads in the next event\n\
  o Print List      prompts you for a file name, prints the list of objects\n\
  o ISAJ/Q          puts up a window displaying the ISAJ and ISAQ info. \n\
                    Help is available in this window.\n\n\
In addition, you can control which variables to include in the list via the\n\
'Select Columns'  button.  This  puts  up  yet  another window - toggle the\n\
column of choice and hit REFRESH.");
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
   o 'lego display'    - There are two 'types' of 'LEGO' displays:\n\n\
                         PHYSICS:\n\
                          jets are  shown  with circles  which indicate the\n\
                          RMS width of the jet in  eta/phi space, otherwise\n\
                          physics objects' locations are indicated by their\n\
                          id  (correlate  with PHYSICS list window).  Also,\n\
                          the vertex is displayed with  a *  using the Zvtx\n\
                          and the face of the calorimeter at 85cm, z=0.\n\n\
                         CATD:\n\
                          towers  from  the  CATD  bank  are  unpacked  and\n\
                          converted  to  ET using the primary vertex.  they\n\
                          are  then  plotted  using  the  integer   eta/phi\n\
                          coordinates  1<phi<64  and -37<eta<37  (excluding\n\
                          0)  on  the eta/phi  display using the 36-decimal\n\
                          scale  (0-9,A-Z  corresponding  to 0-36 GeV). all\n\
                          ET  values  are  rounded  to the nearest integer.\n\
                          Note  that  you can  raise the  threshold via the\n\
                          threshold slider  in  the  PHYSICS  window.\n\n\
   o 'rotate'          - this is a full EULER rotation - hold down the \n\n\
                         buttons for continuous rotation.\n");
            curtext = temp;
            break;
        case 2:           /* help on MC list */
            n = 0;
            XtSetArg(wargs[n], XmNdialogTitle,
              XmStringCreateSimple("MC Help")); n++;
            XtSetValues(physhelp_bull, wargs, n);
            temp = curtext;
            curtext = physhelp;
            cerrmsg(
"What you see here is a list of the contents of the ISAJ and ISAQ banks. The\n\
column labelled 'JET' contains the  jet id for the ISAJ banks, and contains\n\
the parent id for the  ISAQ banks. The toggle button for each object causes\n\
the LEGO display to overlay this object.  The toggle buttons on  the bottom\n\
labelled  'ISAJ',  'ISAQ'  and  'ISAL'  select  which  ISAJET banks will be\n\
included  in  the  list.  If you change the settings, hist the 'REFRESH' to\n\
cause this list to be rebuilt.  The 'PRINT'  button will dump the list into\n\
a file. You will be prompted for the file name.");
            curtext = temp;
            break;
        case 3:          /* help on tracks */
            n = 0;
            XtSetArg(wargs[n], XmNdialogTitle,
              XmStringCreateSimple("Help on Tracks")); n++;
            XtSetValues(physhelp_bull, wargs, n);
            temp = curtext;
            curtext = physhelp;
            cerrmsg(
"For  each  detector track, there is a header bank (DTRH,VTRH,FTRH for\n\
the  CD,VTX,FDC  respectively).  These  banks  are  the  header for the\n\
track banks for the respective detectors (DTRK,VTRK,FTRK respectively).\n\
Push  the  detector  of  choice (CD is the default) and you get what is\n\
basically a formatted  dump  for  each header bank and all tracks which\n\
are  contained.    Note  that  you  can  also  see  these  banks in the\n\
NAVIGATOR.\n\n\
Here is the hierarchy of CD tracking - VTX and FD are similar:\n\n\
Author  : Qizhong Li-Demarteau\n\
Date    : 21-MAY-1989\n\
                 ZEBRA structure for CDC track banks \n\
                 ===================================\n\
\n\
                     +--------------+\n\
                     |      HEAD    |\n\
                     |  header bank |\n\
                     +--------------+\n\
                             |\n\
                    +------------------+\n\
                    |      RECO        |\n\
                    | event descriptor |\n\
                    |       -10        |\n\
                    +------------------+\n\
                             |\n\
                       +-----------+\n\
                       |   PROC    |\n\
                       |    -2     |\n\
                       +-----------+\n\
                             |\n\
                             |\n\
                             |\n\
                          +------+\n\
                          | ZTRH |\n\
                          |  -1  |\n\
                          +------+\n\
                             |\n\
                             |\n\
         +--------------------------------------+\n\
         |            |           |             |\n\
         |            |           |             |\n\
      +------\\     +------+    +------+     +------+\n\
      | ZTRK  \\    | VTRH |    | DTRH |     | FTRH |\n\
      |  -1   /    |  -2  |    |  -3  |     |  -4  |\n\
      +------/     +------+    +------+     +------+\n\
         \\                         |\n\
         \\                        |\n\
     +-----------------------------------------------------------+\n\
     |           |           |           |           |           |\n\
     |           |           |           |           |           |\n\
     |           |           |           |           |           |\n\
  +------\\    +------+    +------+    +------+    +------+    +------+\n\
  | DTRK  \\   | DTSG |    | DTSG |    | DTSG |    | DTSG |    | DITR |\n\
  |  -1   /   |  -2  |    |  -3  |    |  -4  |    |  -5  |    |  -6  |\n\
  +------/    +------+    +------+    +------+    +------+    +------+\n\
     |\n\
     |\n\
     |\n\
  +------+\n\
  | DTTH |\n\
  |  -1  |\n\
  +------+\n\
\n\
    Banks      Description\n\
  _________  _________________________________________________________\n\
\n\
    HEAD       event header\n\
    RECO       header for reconstructed events\n\
    PROC       processed event descriptor\n\
    HSTR       processing history\n\
    ZTRH       Central Tracking Header\n\
    ZTRK       Central tracking tracks\n\
    VTRH       Vertex chamber Track Header\n\
    DTRH       central Drift chamber Track Header\n\
    FTRH       Forward drift chamber Track Header\n\
    DTRK       central Drift chamber Track bank\n\
    DTSG       central Drift chamber Track Segments ( num = layer+2 )\n\
    DTTH       central Drift chamber Track to Hits bank\n\
    DITR       Isajet Tracks found inside the CDC");
            curtext = temp;
        	break;
        default:
            break;
        }

}
