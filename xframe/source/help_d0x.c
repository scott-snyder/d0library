/*
        help_d0x.c
         Created           : 26-AUG-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "xframe/source/d0x_c.h"

/*---------------------------------------------------------------------
  main help routine
----------------------------------------------------------------------*/
void help_d0x(w,tag,reason)
Widget w;
int		*tag;
unsigned long	*reason;
{
	int select = *tag;
	Widget temp;

	switch (select) {
/*
  		                                                */
		case 0:            /* main help on the menu bar */
/*		                                                */
			XmTextSetString(qhelp,
"The program  is intended to be a 'mini-frame' to allow fast access to data\n\
files and  STP files without having to relink a D0USER or CALOR_OFF or etc.\n\
program. The  main  window  menu  items  contains controls for commands and\n\
functions to be able to do the following:\n\n\
 Files             Allows  opening  and  closing  of Zebra FZ or STP files,\n\
  				   as  well as providing access to the 'OUTPUT' submenu for\n\
  				   writing output (FZ) files.\n\n\
 Tools             Access  to  D0X specialties,  D0 specialties,  and ZEBRA\n\
                   specialties (see below)\n\n\
 Control           Controls reading in records, fast searching, and  access\n\
                   to the D0XUSER control menu (see HELP on D0XUser)\n\n\
     Search        Quick  search  for  RUN  number  EVENT  number, L1 or L2\n\
                   or BANK name.\n\n\
     D0XUSER       Can enable, disable calling  D0XUSER per event,  and can\n\
                   get  statistics of  D0XUSER result  (T/F per call) - for\n\
                   more info see Help on D0XUser.\n\n\
 Tools:\n\n\
     D0X           Access to D0X specialties: NAVIGATOR (X/Motif version of\n\
                   DBANK), RAW DATA BROWSER (decodes RAW  cable  bank  data\n\
                   by crates  for  easy viewing/debugging), PHYSICS  window\n\
                   (physics  object  manipulation  and  display)  and QUERY\n\
                   EDITOR (for setting up cuts, requirements for stripping,\n\
                   histograms, etc.).\n\n\
     D0            Access to  EZBANK,  SET_CAPH,  and  most  of  the PRxxxx\n\
                   routines where 'xxxx' is a bank name (e.g. PRPELC).\n\n\
     ZEBRA         Access to some standard  ZEBRA  debugging tools: DZSURV,\n\
                   DZVERI, bank form checking, and path setting.");
  		    break;
/*		                                               */
/*		                                               */
		case 1:            /* help on "files" */
/*		                                               */
			XmTextSetString(qhelp,
"There are 2 'types' of files you may open:\n\n\
  FZ       - these are usually data files - records are read in via the\n\
             'CONTROL' panel or the 'NEXT EVENT' button on the far right.\n\
  STP      - these are D0 parameters - when an STP  file is opened, the\n\
             record is read in and the file is closed - you do NOT have\n\
             to read in records via 'CONTROL' as for FZ files.\n\
  The OPEN button causes a file selection window to appear.  To specify the\n\
  filename via  wildcards,  enter  the  name  directly  in the window under\n\
  'Selection' (try it,  you'll see) and hit OK.  Otherwise, it's the normal\n\
  X/Motif file box that should be familiar.\n\n\
  You can change the ZEBRA common (ZEBCOM/ZEBSTP/GEANT/ZEBWRK), the mode\n\
  (exchange/native) and the type (FZ/STP) via pushbuttons in the file menu.");
			break;
/*		                                               */
		case 2:            /* help on "control" */
/*		                                               */
/*		                                               */
			XmTextSetString(qhelp,
"There are 2 boxes of buttons here to allow you to 'control' the reading\
 of records from the\n\
FZ/RZ file:\n\n\
  o CONTROL box - once a data file is read into memory, to go to a particular\n\
                  record you can push one of the following buttons:\n\n\
    - Read Record (1)    reads in the next record - information is typed to\n\
                         the  screen  as to  which  kind of record (event, \n\
                         begin_run, etc.) is present.\n\n\
    - Read All           reads in all records sequentially - hit HALT button\n\
                         to stop.\n\
    - Skip Records       skip 'n' records,'n'  is  entered in the text window\n\
                         below.\n\
    - **HALT**           stops SCAN, SKIP, or 'searching' asynchronously");
			break;
/*		                                               */
/*		                                               */
		case 3:            /* on output events */
/*		                                               */
			XmTextSetString(qhelp,
" The OUTPUT window will let you write events out in either EXCHANGE or\n\
 NATIVE mode in one of the following ways:\n\n\
o Output the event which is currently in memory.\n\n\
o Set a  flag to  output  ALL  events as  they  are read in. This is useful\n\
 if you want to convert a file from NATIVE to EXCHANGE or vice versa.  Note\n\
 that this button only sets a flag - you  must  hit the  CANCEL  button  to\n\
 cancel  the  set flag.  Anyway, if you select this, then go to the CONTROL\n\
 window and push the READ ALL button. This will cause events which are read\n\
 in sequentially to be written out (and remember you hit the HALT button to\n\
 put a halt to sequential reads).\n\n\
o Set a flag to output  ALL events from a list of  run/record numbers (FROM\n\
 LIST  IN...). Again,  this  only sets a  flag - use  the  CANCEL button to\n\
 cancel.  You will be prompted  for the  LIST file name.   The  list should\n\
 be in the form of RUN EVENT per line.   RUN  is  IQ(LHEAD+6)  and EVENT is\n\
 IQ(LHEAD+9).\n\n\
Note that in this window you can also specify a list of banks to be \n\
either dropped or kept.\n");
			break;
/*		                                               */
/*		                                               */
		case 4:            /* help on D0X */
/*		                                               */
			XmTextSetString(qhelp,
"This menu provides access to D0X speciaties described below.  Note that\n\
all of the 'specialties' are quite involved, and have more detailed help\n\
available from within whatever windows pop up:\n\n\
 o Navigate\n\n\
      This button cause a 'ZEBRA  NAVIGATOR' window  to appear. This window\n\
      allows you  to  navigate around  in the Zebra  array,  displaying the\n\
      contents of banks and the  corresponding .ZEB discription file. (More\n\
      HELP is  in the  'NAVIGATOR'  window.)  Note  that if this  window is\n\
      blank, the default bank is HEAD (or STP for STP-files).\n\n\
 o Raw Data Browser\n\n\
      Enter a  raw data bank name in the  text window (e.g. MUD1, etc.) and\n\
      hit this button and you will be  popped into a browser which will let\n\
      you navigate  around  inside the bank -  the program  ``knows'' about\n\
      crates, etc.\n\n\
 o Physics\n\n\
      This button causes a  'PHYSICS'  window to appear.  This window lists\n\
      physics objects  (electron, photons, muons,  and jets) present in the\n\
      zebra  array and  allows you to  'fool  around' with  them (calculate\n\
      invariant   masses,  etc.).  It is  meant to  be an   'experiment' in\n\
      analysis! There is also a 'quick-and-dirty' event display available.\n\n\
 o Query Editor\n\n\
      This button causes a quite complicated 'QUERY EDITOR' to appear.  In\n\
      the Query Editor window you can specify bank  contents to  histogram\n\
      as well as  semi-complicated requirements on events for browsing and\n\
      possibly  event stripping.  See  the internal  HELP available in the\n\
      Query Editor window.\n\n\
 o Tracks\n\n\
      This pops  up a window which gives a table of tracks in the CD, VTX,\n\
      or FD.  It's a useful debugging tool.");
			break;
/*		                                               */
/*		                                               */
		case 5:            /* help on "navigate" */
/*
    swap widgets - help on the navigator goes into the zeblst window
*/
			XmTextSetString(qhelp,
"The three main text windows show\n\n\
 o 'Bank.ZEB Window: this window will show the .ZEB file for the selected\n\
   bank. This file is found  under the  name D0$ZEBLST:xxxx.ZEB for  bank\n\
   xxxx.\n\n\
   1. Note that you can change the logical D0$ZEBLST in the small text\n\
   window   directly above  this  one by  changing  the  'logical' and\n\
   hitting a <CR>.\n\n\
   2. By   clicking  the   button  under  '.ZEB    display:',  you can\n\
   enable/disable displaying the .ZEB information.  You should do this\n\
   when you want  to quickly  navigate to a bank in  the chain, as the\n\
   .ZEB display feature takes time to fetch the appropriate .ZEB file.\n\
   Once you have  navigated to the  desired bank,  clicking the toggle\n\
   button again  will cause  the  appropriate file to  be fetched into\n\
   this window.\n\n\
 o 'Bank Data Window': this  window will   show the actual data in the\n\
   selected bank.The following sets of controls allows you to navigate\n\
   around and view the data:\n\n\
   1. Directly above are 5 toggle  buttons (diamonds), which allow you\n\
      to change the  format. Note  that 'Auto' will  use the format as\n\
      understood via MZFORM.\n\n\
   2. When the bank is found, the address and chain length is reported\n\
      in the status  text field  directly under  the RETURN button. If\n\
      the bank is part of  a chain (length>1), the  bank  displayed is\n\
      the first  bank in the  chain. If  you  want to  go to the  n'th\n\
      bank  in the  chain,  type  'n' in  the  small text window under\n\
      'Select Chain #:' followed by <CR>.\n\n\
   3. Bank:  You can  go  directly to a  named  bank by  typing in the\n\
      bank's name in the text window next to 'Bank:'\n\n\
   4. Print: Allows  printing  of  information  in  window below (bank\n\
      contents).  You will be prompted for  a file name, you can leave\n\
      it open if you wish (to collect info  from several banks...).\n\n\
   5. Navigate: This set of buttons  allows you to navigate around the\n\
      chain.  The value  in the  text  window is  the  offset from the\n\
      bank-pointer. The  arrow buttons changes  this value (or you can\n\
      change  it by  hand). A  <CR> when  this  little text  widget is\n\
      selected causes the bank which  is at LQ(bank_pointer+offset) to\n\
      be fetched (along  with the corresponding  .ZEB file if the bank\n\
      name changes).\n\n\
 o 'Event Tree Window':  this  window  contains  a 'map' of the store.\n\
   Clicking ONCE on the bank name causes the  .ZEB  file and  data for\n\
   that bank to be displayed in the 'Bank Data' and 'Bank.Zeb' windows.\n\
   Note that each bank button has is labelled by it's name, the offset\n\
   in  the  parent bank (e.g. RECO,-10 means the RECO bank, structural\n\
   link -10  in the HEAD bank) and if it is a chain, the length of the\n\
   chain (e.g. CAPH,-4(5) means the CAPH bank, -4 structural link from\n\
   the PROC bank, and  chain  length 5).  The  banks  which  have  the\n\
   thicker borders are bank chains.\n\n\
 Note on TREE:  In the CONTROLs  window you  will see a  toggle  button\n\
                labeled  'Partial Tree'.  If  you  click this, then the\n\
                tree will be rebuilt with only the  HEAD and next level\n\
                banks shown.   Then  by DOUBLE-CLICKing  on a  bank the\n\
                bank will be shown.  This allows you to 'see' only that\n\
                part of the tree which is of interest.  Also, all banks\n\
                in the chain are displayed in this case.\n\n\
 Helpful hints:\n\n\
 - as  said above, the .ZEB display often takes a long time, especially\n\
   on FNALD0 where you will have to go over the network to get the .ZEB\n\
   file.  if you disable this feature via the toggle button, upper left\n\
   corner, you can get to the bank of choice  much faster. anytime  you\n\
   re-enable this feature, the program will  try to get  the  .ZEB file\n\
   which corresponds to whatever is in the window below.\n\n\
 - push the Hex<==>Dec button to bring up a  little hexidecimal/decimal\n\
   converter window.");
			break;
/*		                                               */
/*		                                               */
		case 6:            /* help on "raw data browser" */
/*
    swap widgets - help on the raw goes into the raw_text window
*/
			XmTextSetString(qhelp,
"There are 4 text  windows in this display, plus  a Hex/Decimal decoder.  If\n\
the bank is a true raw  data bank, then the data  should be organized as to\n\
the documentation in  ADC_DATA_FORMAT and  D0_DATA_FORMAT (e.g. MUD1, CAD1,\n\
etc. banks).  The four  windows allow you to  look inside  these banks on a\n\
`crate-by-crate' basis.  Data is presented in HEX  format. The four windows\n\
are:\n\n\
1. Header      this displays the header information per `crate'\n\n\
2. Trailer     ditto for trailer info\n\n\
3. Crate       this window  contains a list of crates and number of data\n\
               words associated with each crate.  to move from one crate\n\
               to the next, simply select (with the mouse)  the crate of\n\
               choice and hit <CR>.\n\n\
4. Data        this window contains the raw data.  if you wish to change\n\
               the  number of  columns per  row, type  the number in the\n\
               small text window, upper right hand corner, and hit <CR>.\n\n\
PRINT button:     When pushed will cause you to be prompted for a file name\n\
				  which will  contain the contents of the  HEADER, TRAILER,\n\
				  and DATA of the current crate.\n\n\
Next Crate:       When pushed will cycle to the next crate in the list (and\n\
                  will wrap around to the first after the last).\n\n\
Decoder:          Used for easy conversions between HEX and DECIMAL.  Enter\n\
				  the number you  wish to  decode and  hit the  appropriate\n\
				  button\n");
			break;
/*		                                               */
/*		                                               */
  		case 7:     /* help on search */
/*		                                               */
			XmTextSetString(qhelp,
"You can can  read  in records and stop on the event which satisfies one of\n\
the following criteria - note that you will be prompted for the appropriate\n\
variable:\n\n\
    - RUN NUMBER    has the correct run number IQ(LHEAD+6)\n\
    - EVENT NUMBER  has the correct ('output') event number IQ(LHEAD+9)\n\
    - BANK          has a particular bank in the record\n\n\
    - L1 TRIGGER    from the appropriate TSUM bank\n\n\
    - L2 TRIGGER    from the appropriate TSUM bank\n");
			break;
/*		                                               */
/*		                                               */
		case 8:            /* help on D0 */
/*		                                               */
			XmTextSetString(qhelp,
"This collection of buttons allows you to take a look at banks in the Zebra\n\
store  in  both  formatted  and   unformatted ways.  You can use EZBANK and\n\
SET_CAPH but DBANK and DADDR, which are based heavily on SMG, are no longer\n\
available. You can call many of the PRxxxx routines (where 'xxxx' specifies\n\
a  bank name).  The resulting bank dump can be written directly to file via\n\
the 'PRxxxx to output' button.");
			break;
/*		                                               */
/*		                                               */
		case 9:           /* help ZEBRA */
			XmTextSetString(qhelp,
"This menu provides access to several ZEBRA debugging tools:\n\n\
 o Show Bank Chain\n\n\
      Dumps the entire bank chain  for  the record  present in memory  to\n\
      the screen.\n\n\
 o Verify Store\n\n\
      verifies the Zebra structure (via DZVERI)\n\n\
 o Show/Set Path\n\n\
      If the text window is  empty (or has '?') then  the current PATH will\n\
      be displayed in the text window. Otherwise, if you type a  legitimate\n\
      4 letter PATH  name into the TEXT  window and hit  the  Show/Set PATH\n\
      button (or <CR>) the path is altered accordingly.\n\n\
 o Check FORM\n\n\
      Checks the MZFORM for the bank by calling DZFORM.");
			break;
/*		                                               */
/*		                                               */
  		case 10:     /* help on d0xuser */
/*		                                               */
			XmTextSetString(qhelp,
"D0XUSER is a set of 4 entries (LOGICAL FUNCTIONS) that are called by the D0X\n\
framework. There are 4 entries available:\n\n\
 o Event     This entry is called for every event which is read in.\n\
 o Talk      This entry is called for interactive uses - set parameters,\n\
             reinitialize arrays, etc.\n\
 o Init      This entry is called during the initialization phase of D0X.\n\
 o Finish    This entry is called  before  D0X exists  (which is via the\n\
             Exit pushbutton in the 'File' menu).\n\n\
The code that D0X  uses is in the  D0X source area,  D0XUSER.FOR. This code\n\
will do NOTHING.  However, you can write your own  version of this function\n\
and link it in by hand using the  symbol LINK_XFRAME with a single argument\n\
corresponding to  the files you want  to link. For  example you have a file\n\
called MINE.FOR which has the function D0XUSER in it (event entry), as well\n\
as  other  routines used  by  your  version of  D0XUSER.  You  compile this\n\
function and link  it in by  'LINK_D0XUSER MINE',  producing the executable\n\
D0XUSER.  If  you don't  supply  one or  more of  the 4  entries,  then the\n\
default entry will be called (which will do nothing).\n\n\
Note that each entry has an integer  argument. D0X will call the entry with\n\
the argument set to  zero. However, you can use  this argument for your own\n\
purposes when (or if) you call each entry explicitly from the D0XUSER menu,\n\
described here:\n\n\
In the 'Control' menu, the button labelled 'D0XUSER' brings up a menu which\n\
allows you to\n\n\
     o call D0XUSER entries    you will be prompted for the integer argument\n\
     o EN/DISable D0XUSER      control wither D0XUSER is called during\n\
                               readins - default is ENabled\n\
     o calculate statistics    returns the rate of true/false returns from\n\
                               the EVENT entry.");
			break;
/*		                                               */
/*		                                               */
  		case 11:     /* help on tree */
/*		                                               */
			XmTextSetString(qhelp,
"The Option menu for Full/Partial does the following:\n\n\
      Full:     The entire Zebra tree is shown but only the first bank\n\
                in every bank chain (with a thick outline)\n\n\
      Partial:  HEAD bank plus next level is displayed only.  Double-click\n\
                on any bank and the next level below will be displayed");
            break;
		default:
			printf("\n\n\n");
			printf(" **************************************\n");
		    printf(" ***Illegal tag presented 'help_d0x'***\n");
			printf(" **************************************\n");
			printf("\n\n\n");
		    break;
	}
}
