/* 
        d0x_c.h
         Created           : 26-AUG-1992 by Drew Baden
*/
 
#include <Mrm/MrmAppl.h>             /* Motif Toolkit and MRM */
 
#ifdef D0FLAVOR
#include <Xm/Text.h>
#endif

/*  place widget id's for text widgets here, get'em in save_text 
    note:  extern doesn NOT allocate space - this is done in 
    save_text.c - if you add anything here, add it there as well */

extern Widget  file_text;              /*   0 */
extern Widget  skip_text;              /*   1 */
extern Widget  path_text;              /*   2 */
extern Widget  dbank_text;             /*   3 */
extern Widget  daddr_text;             /*   4 */
extern Widget  prbank_text;            /*   5 */
extern Widget  search_text;            /*   6 */
extern Widget  bank_text;              /*   7 */
extern Widget  xdbank_text_top;        /*   8 */
extern Widget  xdbank_text_bottom;     /*   9 */
extern Widget  xdbank_text_map;        /*  10 */
extern Widget  xdbank_text_zeblst;     /*  11 */
extern Widget  xdbank_text_chain;      /*  12 */
extern Widget  xdbank_text_navigate;   /*  13 */
extern Widget  xdbank_text_length  ;   /*  14 */
extern Widget  xdbank_text_address ;   /*  15 */
extern Widget  xdbank_main;            /*  16 */
extern Widget  bulletin_main;          /*  17 */
extern Widget  main_text;              /*  18 */
extern Widget  navigate_text;          /*  19 */
extern Widget  fz_label;               /*  20 */
extern Widget  stp_label;              /*  21 */
extern Widget  output_label;           /*  22 */
extern Widget  xdbank_bank_2;          /*  23 */
extern Widget  decode_text;            /*  24 */
extern Widget  raw_text;               /*  25 */
extern Widget  raw_header;			   /*  26 */
extern Widget  raw_trailer;            /*  27 */
extern Widget  crate_label;            /*  28 */
extern Widget  goto_crate;             /*  29 */
extern Widget  goto_offset;            /*  30 */
extern Widget  raw_status;             /*  31 */
extern Widget  numrow_text;			   /*  32 */
extern Widget  crates_text;			   /*  33 */
extern Widget  tree_scroll;            /*  34 */
extern Widget  data_scroll;            /*  35 */
extern Widget  physics_bull;           /*  36 */
extern Widget  physics_scroll;         /*  37 */
extern Widget  anglelab;         	   /*  38 */
extern Widget  drawitxy;         	   /*  39 */
extern Widget  drawityz;         	   /*  40 */
extern Widget  drawitxz;         	   /*  41 */
extern Widget  vertlab;         	   /*  42 */
extern Widget  drawitlego; 			   /*  43 */
extern Widget  phi0lab;                /*  44 */
extern Widget  physhelp;               /*  45 */
extern Widget  theta0lab;              /*  46 */
extern Widget  psi0lab;                /*  47 */
extern Widget  physhelp_bull; 		   /*  48 */
extern Widget  outtext;                /*  49 */
extern Widget  listbull;               /*  50 */
extern Widget  emlist; 				   /*  51 */
extern Widget  drawemlong;			   /*  52 */
extern Widget  scalefac;			   /*  53 */
extern Widget  thresh;  			   /*  54 */
extern Widget  zebscroll;              /*  55 */
extern Widget  pmuolist;               /*  56 */
extern Widget  toplevel_widget;
extern Widget  curtop;  /* current top level - could be toplevel or ... */
extern Widget  curtext; /* current text window for errors, etc.  */
extern XtAppContext appcontext;
typedef union {
	char name[3];
	int iname;
} EQUIV;
