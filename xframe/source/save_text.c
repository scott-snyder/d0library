/* 
        save_text.c
         Created           : 26-AUG-1992 by Drew Baden
*/
 
#include <stdio.h>                   /* I/O definitions                       */
 
#include <Mrm/MrmAppl.h>             /* Motif Toolkit and MRM */

#ifdef D0FLAVOR
#include <Xm/Text.h>
#endif

Widget  file_text;              /*   0 */
Widget  skip_text;              /*   1 */
Widget  path_text;              /*   2 */
Widget  dbank_text;             /*   3 */
Widget  daddr_text;             /*   4 */
Widget  prbank_text;            /*   5 */
Widget  search_text;            /*   6 */
Widget  bank_text;              /*   7 */
Widget  xdbank_text_top;        /*   8 */
Widget  xdbank_text_bottom;     /*   9 */
Widget  xdbank_text_map;        /*  10 */
Widget  xdbank_text_zeblst;     /*  11 */
Widget  xdbank_text_chain;      /*  12 */
Widget  xdbank_text_navigate;   /*  13 */
Widget  xdbank_text_length  ;   /*  14 */
Widget  xdbank_text_address ;   /*  15 */
Widget  xdbank_main;            /*  16 */
Widget  bulletin_main;          /*  17 */
Widget  main_text;              /*  18 */
Widget  navigate_text;          /*  19 */
Widget  fz_label;               /*  20 */
Widget  stp_label;              /*  21 */
Widget  output_label;           /*  22 */
Widget  xdbank_bank_2;          /*  23 */
Widget  decode_text;            /*  24 */
Widget  raw_text;               /*  25 */
Widget  raw_header;			    /*  26 */
Widget  raw_trailer;            /*  27 */
Widget  crate_label;            /*  28 */
Widget  goto_crate;             /*  29 */
Widget  goto_offset;            /*  30 */
Widget  raw_status;             /*  31 */
Widget  numrow_text;			/*  32 */
Widget  crates_text;		    /*  33 */
Widget  tree_scroll;            /*  34 */
Widget  data_scroll;            /*  35 */
Widget  physics_bull;           /*  36 */
Widget  physics_scroll;         /*  37 */
Widget  anglelab;         	    /*  38 */
Widget  drawitxy;         	   	/*  39 */
Widget  drawityz;         	   	/*  40 */
Widget  drawitxz;         	   	/*  41 */
Widget  vertlab;         	   	/*  42 */
Widget  drawitlego; 			/*  43 */
Widget  phi0lab;                /*  44 */
Widget  physhelp;               /*  45 */
Widget  theta0lab;              /*  46 */
Widget  psi0lab;              	/*  47 */
Widget  physhelp_bull; 			/*  48 */
Widget  outtext;                /*  49 */
Widget  listbull;               /*  50 */
Widget  emlist;					/*  51 */
Widget  drawemlong;				/*  52 */
Widget  scalefac;			   	/*  53 */
Widget  thresh;  			    /*  54 */
Widget  zebscroll;              /*  55 */
Widget  pmuolist;               /*  56 */
Widget  curtop;  /* current top level - could be toplevel or ... */
Widget  curtext; /* current text widget */
XtAppContext appcontext;
/*---------------------------------------------------------------------
  saves the tags for widgets upon creation 
----------------------------------------------------------------------*/
save_text(w,tag,reason)   
Widget		w;
int		*tag;
unsigned long	*reason;
{
	int select = *tag;
	
	switch (select) {
		case 0:    file_text = w;					break;
		case 1:    skip_text = w;					break;
		case 2:    path_text = w;					break;
		case 3:    dbank_text = w;					break;
		case 4:    daddr_text = w;					break;
		case 5:    prbank_text = w; 
				   XmTextSetString(prbank_text,"?");
				   									break;
		case 6:    search_text = w;					break;
		case 7:    bank_text = w;					break;
		case 8:    xdbank_text_top = w;				break;
		case 9:    xdbank_text_bottom = w;			break;
		case 10:   xdbank_text_map = w;				break;
		case 11:   xdbank_text_zeblst = w;
/* for unix, I change d0$zeblst to $d0zeblst/ */
#ifdef D0FLAVOR
			       XmTextSetString(w,"$d0zeblst/");
#endif
												   break;
		case 12:   xdbank_text_chain = w;			break;
		case 13:   xdbank_text_navigate = w;		break;
		case 14:   xdbank_text_length = w;			break;
		case 15:   xdbank_text_address = w;			break;
		case 16:   xdbank_main = w;					break;
		case 17:   bulletin_main = w;				
			appcontext = XtWidgetToApplicationContext(bulletin_main);
			break;
		case 18:   main_text = w;					break;
		case 19:   navigate_text = w;				break;
		case 20:   fz_label = w;					break;
		case 21:   stp_label = w;					break;
		case 22:   output_label = w;				break;
		case 23:   xdbank_bank_2 = w;				break;
		case 24:   decode_text = w;					break;
		case 25:   raw_text = w;					break;
		case 26:   raw_header = w;					break;
		case 27:   raw_trailer = w;					break;
		case 28:   crate_label = w;					break;
		case 29:   goto_crate = w;					break;
		case 30:   goto_offset = w;					break;
		case 31:   raw_status = w;					break;
		case 32:   numrow_text = w;					break;
		case 33:   crates_text = w;					break;
		case 34:   tree_scroll = w;					break;
		case 35:   data_scroll = w;					break;
		case 36:   physics_bull = w;				break;
		case 37:   physics_scroll = w;				break;
		case 38:   anglelab = w;					break;
		case 39:   drawitxy = w;					break;
		case 40:   drawityz = w;					break;
		case 41:   drawitxz = w;					break;
		case 42:   vertlab = w;						break;
		case 43:   drawitlego = w;					break;
		case 44:   phi0lab = w;                     break;
		case 45:   physhelp = w;                    break;
		case 46:   theta0lab = w;                   break;
		case 47:   psi0lab = w;                     break;
		case 48:   physhelp_bull = w;               break;
		case 49:   outtext = w;                     break;
		case 50:   listbull = w;					break;
		case 51:   emlist = w;						break;
		case 52:   drawemlong = w;					break;
		case 53:   scalefac = w;					break;
		case 54:   thresh = w;  			   		break;
		case 55:   zebscroll = w;					break;
		case 56:   pmuolist = w;					break;
		default:
		    printf(" ***Illegal tag presented 'save_text'***\n");
			break;
		}
}

