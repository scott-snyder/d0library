/*
        help_d0x.c
         Created           : 26-AUG-1992 by Drew Baden
*/

#include <stdio.h>                   /* I/O definitions                       */

#include "/d0lib/scratch/xframe/source/d0x_c.h"

/*---------------------------------------------------------------------
  main help routine
----------------------------------------------------------------------*/
help_d0x(w,tag,reason)
int w;
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
		    cerrmsg(
"The program  is intended to be a 'mini-frame' to allow fast access to data\n\
files and  STP files without having to relink a D0USER or CALOR_OFF or etc.\n\
program. The following windows contain controls for commands and functions.\n\n\
 File Window       Allows  opening  and  closing  of  Zebra, STP, or OUTPUT\n\
  				   files  (OUTPUT  files  are used to capture  output  from\n\
  				   things like PRPELC, etc.).\n\n\
 Control Window    Controls reading in records, skipping and searching\n\n\
 Utilities Window  Allows  access to Zebra debugging utilities (e.g. DZSURV,\n\
                   etc.),  various  standard  D0  debugging  utilities (e.g.\n\
                   DBANK, etc.), and some new D0 debugging  facilities (e.g.\n\
                   'Navigator', 'Raw Data Browser', and 'Physics' windows).\n\n\
 Output Window     Allows you to write ether the event which is currently in\n\
                   memory  to a file (in either exchange or native mode) OR \n\
                   you  can  have  every event read in written out in either\n\
                   exchange or native mode.  This last feature allows you to\n\
                   convert files easily (for transport, etc.)  In  addition,\n\
                   you can select a subset of the banks to save.\n\n\
In addition, there are two buttons in the menu bar:\n\n\
 Exit            - Exit the program\n\n\
 Windows         - Any  of  the  four  above windows can be 'dismissed'.  To\n\
                   get these windows back, use this pull-down menu.\n");
  		    break;
/*		                                               */
/*		                                               */
		case 1:            /* help on "files" */
/*		                                               */
		    cerrmsg(
"There are 3 'types' of files you may open:\n\n\
  FZ/RZ    - these are usually data files - records are read in via the\n\
             'CONTROL' panel (RZ files not yet implemented).\n\
  STP      - these are D0 parameters - when an STP  file is opened, the\n\
             record is read in and the file is closed - you do NOT have\n\
             to read in records via 'CONTROL' as for FZ/RZ files.\n\
  OUTPUT   - to capture output (from bank DUMP, etc.) - output  will be\n\
             directed to the output file AND to the terminal\n\n\
  To open ZEBRA (data) files, you can just type the  file name into the\n\
  rectangular  text  area  and  hit <CR> (or push the OPEN button, same\n\
  thing).  In addition, if you push the 'File Browser'  button, you get\n\
  the usual X/Windows  file browser  widget (this is convenient  if you\n\
  want to search around for a file without leaving the program).\n\n\
  Rules you have to follow:\n\
  o specify type of file via the 3 vertical buttons on the left\n\
  o specify mode (Exchange/Native) - note that D0OPEN will do this \
automatically \n\
  o input filename and hit OPEN (or <CR>) to open\n");
			break;
/*		                                               */
		case 2:            /* help on "control" */
/*		                                               */
/*		                                               */
		    cerrmsg(
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
    - **HALT**           stops SCAN, SKIP, or 'searching' asynchronously\n\n\
  o SEARCH_BY   - you can read in records and stop on the event which\n\
                  satisfies one of the following criteria - note that\n\
                  you MUST use the text window  in the  box  to enter\n\
                  either the run or event number or bank name:\n\n\
    - RUN NUMBER    has the correct run number IQ(LHEAD+6)\n\
    - EVENT NUMBER  has the correct ('output') event number IQ(LHEAD+9)\n\
    - BANK          has a particular bank in the record\n\n\
    - L1 TRIGGER    from the appropriate TSUM bank\n\n\
    - L2 TRIGGER    from the appropriate TSUM bank\n");
			break;
/*		                                               */
/*		                                               */
		case 3:            /* on output events */
/*		                                               */
			cerrmsg(
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
		case 4:            /* help on utilities */
/*		                                               */
		    cerrmsg(
"This collection of buttons allows  you to take a look at banks in the Zebra\n\
store in  both  formatted and   unformatted ways.  There are 3  collections\n\
(boxes) of  buttons which group  buttons in some kind  of logical way. Each\n\
button is  described below. Note  that the familiar  debugging tools DBANK,\n\
etc. rely  on SMG or on  the SMG  emulator in  UNIX - no  changes have been\n\
made to these tools, and there is no guarantee that they will work on UNIX.\n\
However, there  are a few new  debugging tools which  are based on X/Motif.\n\
Some of these  tools require  you to enter a  bank name (or   address) in a\n\
text window.\n\n\
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
 o Formatted Dump\n\n\
      For bank 'xxxx', the subroutine PRxxxx is  called. If you enter xxxx\n\
      (or ?) in the window, a  list of  available routines will  be shown.\n\
      If your favorite bank is not in the list, send some nice e-mail to\n\
      DREW.\n\n\
 o Check FORM\n\n\
      Checks the MZFORM for the bank by calling DZFORM.\n\n\
 o Dump at Address\n\n\
      Type an address into the text  window, and an ascii dump will appear.\n\n\
      Hopefully you provide a legal address.\n\n\
 o Show Bank Chain\n\n\
      Dumps the entire bank chain  for  the record  present in memory  to\n\
      the screen.\n\n\
 o Show/Set Path\n\n\
      If the text window is  empty (or has '?') then  the current PATH will\n\
      be displayed in the text window. Otherwise, if you type a  legitimate\n\
      4 letter PATH  name into the TEXT  window and hit  the  Show/Set PATH\n\
      button (or <CR>) the path is altered accordingly.\n\n\
  o Verify Store\n\n\
      verifies the Zebra structure (via DZVERI)\n\n\
  o Set Store\n\n\
      allows you to toggle 'store'\n");
			break;
/*		                                               */
/*		                                               */
		case 5:            /* help on "navigate" */
/*
    swap widgets - help on the navigator goes into the zeblst window
*/
    	    temp = curtext;
    	    curtext = xdbank_text_top;
		    cerrmsg(
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
   which corresponds to whatever is in the window below.\n\n");
		    curtext = temp;
			break;
/*		                                               */
/*		                                               */
		case 6:            /* help on "raw data browser" */
/*
    swap widgets - help on the raw goes into the raw_text window
*/
    	    temp = curtext;
    	    curtext = raw_text;
		    cerrmsg(
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
		    curtext = temp;
			break;
		default:
			printf("\n\n\n");
			printf(" **************************************\n");
		    printf(" ***Illegal tag presented 'save tag'***\n");
			printf(" **************************************\n");
			printf("\n\n\n");
		    break;
	}
}
