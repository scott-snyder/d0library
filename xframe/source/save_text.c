/* 
        save_text.c
         Created           : 26-AUG-1992 by Drew Baden
*/
 
#include <stdio.h>                   /* I/O definitions                       */
 
#include <Mrm/MrmAppl.h>             /* Motif Toolkit and MRM */
#include <Xm/Text.h>

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
Widget  raw_header;             /*  26 */
Widget  raw_trailer;            /*  27 */
Widget  crate_label;            /*  28 */
Widget  goto_crate;             /*  29 */
Widget  goto_offset;            /*  30 */
Widget  raw_status;             /*  31 */
Widget  numrow_text;            /*  32 */
Widget  crates_text;            /*  33 */
Widget  tree_scroll;            /*  34 */
Widget  data_scroll;            /*  35 */
Widget  physics_bull;           /*  36 */
Widget  physics_scroll;         /*  37 */
Widget  anglelab;               /*  38 */
Widget  drawitxy;               /*  39 */
Widget  drawityz;               /*  40 */
Widget  drawitxz;               /*  41 */
Widget  vertlab;                /*  42 */
Widget  drawitlego;             /*  43 */
Widget  phi0lab;                /*  44 */
Widget  physhelp;               /*  45 */
Widget  theta0lab;              /*  46 */
Widget  psi0lab;                /*  47 */
Widget  physhelp_bull;          /*  48 */
Widget  outtext;                /*  49 */
Widget  listbull;               /*  50 */
Widget  emlist;                 /*  51 */
Widget  drawemlong;             /*  52 */
Widget  scalefac;               /*  53 */
Widget  thresh;                 /*  54 */
Widget  zebscroll;              /*  55 */
Widget  pmuolist;               /*  56 */
Widget  defined;                /*  57 */
Widget  thevalue;               /*  58 */
Widget  boffset;                /*  59 */
Widget  req1list;               /*  60 */
Widget  reqtext;                /*  61 */
Widget  reqlists;               /*  62 */
Widget  reqvalue;               /*  63 */
Widget  reqzeb;                 /*  64 */
Widget  rpositive;              /*  65 */
Widget  rnegative;              /*  66 */
Widget  qhelp;                  /*  67 */
Widget  wrand;                  /*  68 */
Widget  wror;                   /*  69 */
Widget  histon;                 /*  70 */
Widget  histoff;                /*  71 */
Widget  xhwhat;                 /*  72 */
Widget  xhtitle;                /*  73 */
Widget  xhid;                   /*  74 */
Widget  xhnbin;                 /*  75 */
Widget  xhbinl;                 /*  76 */
Widget  xhbinh;                 /*  77 */
Widget  xhstore;                /*  78 */
Widget  xhmake;                 /*  79 */
Widget  hdraw;                  /*  80 */
Widget  deflist;                /*  81 */
Widget  rstop;                  /*  82 */
Widget  rcont;                  /*  83 */
Widget  low1text;               /*  84 */
Widget  low1scale;              /*  85 */
Widget  hightext;               /*  86 */
Widget  highscale;              /*  87 */
Widget  numtext;                /*  88 */
Widget  numscale;               /*  89 */
Widget  ocount;                 /*  90 */
Widget  oopenb;                 /*  91 */
Widget  oclosb;                 /*  92 */
Widget  xnid;                   /*  93 */
Widget  xnmake;                 /*  94 */
Widget  pnuttext;               /*  95 */
Widget  physmc;                 /*  96 */
Widget  physmctext;             /*  97 */
Widget  isajtoggle;             /*  98 */
Widget  isaqtoggle;             /*  99 */
Widget  fzdestination;          /* 100 */
Widget  fz_mode;                /* 101 */
Widget  fztype;                 /* 102 */
Widget  zpath;                  /* 103 */
Widget  stattext;               /* 104 */
Widget  mcanglelab;             /* 105 */
Widget  microcasc;              /* 106 */
Widget  cafixcasc;              /* 107 */
Widget  cleanemtext;            /* 108 */
Widget  navaddresslab;          /* 109 */
Widget  navchainlab;            /* 110 */
Widget  d0dadrun;               /* 111 */
Widget  d0dadevent;             /* 112 */
Widget  d0dadcattext;           /* 113 */
Widget  nav_main;               /* 114 */
Widget  d0dad_main;             /* 115 */
Widget  mcphys_main;            /* 116 */
Widget  raw_main;               /* 117 */
Widget  jettext;                /* 118 */
Widget  ptautext;               /* 119 */
Widget  updtimer;               /* 120 */
Widget  trktext;                /* 121 */
Widget  trackvariabs;           /* 122 */
Widget  diskortape;             /* 123 */
Widget  andortext;              /* 124 */
Widget  andorresult;            /* 125 */
Widget  track_bull;             /* 126 */
Widget  caeh_text;              /* 127 */
Widget  caeh_bull;              /* 128 */
Widget  etamin_text;            /* 129 */
Widget  etamax_text;            /* 130 */
Widget  phimin_text;            /* 131 */
Widget  phimax_text;            /* 132 */
Widget  laymin_text;            /* 133 */
Widget  laymax_text;            /* 134 */
Widget  caeh_thresh;            /* 135 */
Widget  cachcaeh_text;          /* 136 */
Widget  evout_window;           /* 137 */
Widget  etmin_text;             /* 138 */
Widget  etmax_text;             /* 139 */
Widget  encahits;               /* 140 */
Widget  discahits;              /* 141 */
Widget  enudst;                 /* 142 */
Widget  disudst;                /* 143 */
Widget  encleanem;	            /* 144 */
Widget  discleanem;             /* 145 */
Widget  encleanmu;              /* 146 */
Widget  discleanmu;             /* 147 */
Widget  encafix;                /* 148 */
Widget  discafix;               /* 149 */
Widget  curtop;  /* current top level - could be toplevel or ... */
Widget  curtext; /* current text widget */
XtAppContext appcontext;
/*---------------------------------------------------------------------
  saves the tags for widgets upon creation 
----------------------------------------------------------------------*/
void
save_text(w,tag,reason)   
Widget        w;
int        *tag;
unsigned long    *reason;
{
    int select = *tag;
    
    switch (select) {
        case 0:    file_text = w;                    break;
        case 1:    skip_text = w;                    break;
        case 2:    path_text = w;                    break;
        case 3:    dbank_text = w;                   break;
        case 4:    daddr_text = w;                   break;
        case 5:    prbank_text = w; 
                   XmTextSetString(prbank_text,"?");
                                                     break;
        case 6:    search_text = w;                  break;
        case 7:    bank_text = w;                    break;
        case 8:    xdbank_text_top = w;              break;
        case 9:    xdbank_text_bottom = w;           break;
        case 10:   xdbank_text_map = w;              break;
        case 11:   xdbank_text_zeblst = w;           break;
        case 12:   xdbank_text_chain = w;            break;
        case 13:   xdbank_text_navigate = w;         break;
        case 14:   xdbank_text_length = w;           break;
        case 15:   xdbank_text_address = w;          break;
        case 16:   xdbank_main = w;                  break;
        case 17:   bulletin_main = w;                
            appcontext = XtWidgetToApplicationContext(bulletin_main);
            break;
        case 18:   main_text = w;                    break;
        case 19:   navigate_text = w;                break;
        case 20:   fz_label = w;                     break;
        case 21:   stp_label = w;                    break;
        case 22:   output_label = w;                 break;
        case 23:   xdbank_bank_2 = w;                break;
        case 24:   decode_text = w;                  break;
        case 25:   raw_text = w;                     break;
        case 26:   raw_header = w;                   break;
        case 27:   raw_trailer = w;                  break;
        case 28:   crate_label = w;                  break;
        case 29:   goto_crate = w;                   break;
        case 30:   goto_offset = w;                  break;
        case 31:   raw_status = w;                   break;
        case 32:   numrow_text = w;                  break;
        case 33:   crates_text = w;                  break;
        case 34:   tree_scroll = w;                  break;
        case 35:   data_scroll = w;                  break;
        case 36:   physics_bull = w;                 break;
        case 37:   physics_scroll = w;               break;
        case 38:   anglelab = w;                     break;
        case 39:   drawitxy = w;                     break;
        case 40:   drawityz = w;                     break;
        case 41:   drawitxz = w;                     break;
        case 42:   vertlab = w;                      break;
        case 43:   drawitlego = w;                   break;
        case 44:   phi0lab = w;                      break;
        case 45:   physhelp = w;                     break;
        case 46:   theta0lab = w;                    break;
        case 47:   psi0lab = w;                      break;
        case 48:   physhelp_bull = w;                break;
        case 49:   outtext = w;                      break;
        case 50:   listbull = w;                     break;
        case 51:   emlist = w;                       break;
        case 52:   drawemlong = w;                   break;
        case 53:   scalefac = w;                     break;
        case 54:   thresh = w;                       break;
        case 55:   zebscroll = w;                    break;
        case 56:   pmuolist = w;                     break;
        case 57:   defined = w;                      break;
        case 58:   thevalue = w;                     break;
        case 59:   boffset = w;                      break;
        case 60:   req1list = w;                     break;
        case 61:   reqtext = w;                      break;
        case 62:   reqlists = w;                     break;
        case 63:   reqvalue = w;                     break;
        case 64:   reqzeb = w;                       break;
        case 65:   rpositive = w;                    break;
        case 66:   rnegative = w;                    break;
        case 67:   qhelp = w;                        break;
        case 68:   wrand = w;                        break;
        case 69:   wror = w;                         break;
        case 70:   histon = w;                       break;
        case 71:   histoff = w;                      break;
        case 72:   xhwhat = w;                       break;
        case 73:   xhtitle = w;                      break;
        case 74:   xhid = w;                         break;
        case 75:   xhnbin = w;                       break;
        case 76:   xhbinl = w;                       break;
        case 77:   xhbinh = w;                       break;
        case 78:   xhstore = w;                      break;
        case 79:   xhmake = w;                       break;
        case 80:   hdraw = w;                        break;
        case 81:   deflist = w;                      break;
        case 82:   rstop = w;                        break;
        case 83:   rcont = w;                        break;
        case 84:   low1text = w;                     break;
        case 85:   low1scale = w;                    break;
        case 86:   hightext= w;                      break;
        case 87:   highscale = w;                    break;
        case 88:   numtext = w;                      break;
        case 89:   numscale = w;                     break;
        case 90:   ocount = w;                       break;
        case 91:   oopenb = w;                       break;
        case 92:   oclosb = w;                       break;
        case 93:   xnid = w;                         break;
        case 94:   xnmake = w;                       break;
        case 95:   pnuttext = w;                     break;
        case 96:   physmc = w;                       break;
        case 97:   physmctext = w;                   break;
        case 98:   isajtoggle = w;                   break;
        case 99:   isaqtoggle = w;                   break;
        case 100:  fzdestination = w;                break;
        case 101:  fz_mode = w;                      break;
        case 102:  fztype = w;                       break;
        case 103:  zpath = w;                        break;
        case 104:  stattext = w;                     break;
        case 105:  mcanglelab = w;                   break;
        case 106:  microcasc = w;                    break;
        case 107:  cafixcasc = w;                    break;
        case 108:  cleanemtext = w;                  break;
        case 109:  navaddresslab = w;                break;
        case 110:  navchainlab = w;                  break;
        case 111:  d0dadrun = w;                     break;
        case 112:  d0dadevent = w;                   break;
        case 113:  d0dadcattext = w;                 break;
        case 114:  nav_main = w;                     break;
        case 115:  d0dad_main = w;                   break;
        case 116:  mcphys_main = w;                  break;
        case 117:  raw_main = w;                     break;
        case 118:  jettext = w;                      break;
        case 119:  ptautext = w;                     break;
        case 120:  updtimer = w;                     break;
        case 121:  trktext = w;                      break;
        case 122:  trackvariabs = w;                 break;
        case 123:  diskortape = w;                   break;
        case 124:  andortext = w;                    break;
        case 125:  andorresult = w;                  break;
        case 126:  track_bull = w;                   break;
        case 127:  caeh_text = w;                    break;
        case 128:  caeh_bull = w;                    break;
        case 129:  etamin_text = w;                  break;
        case 130:  etamax_text = w;                  break;
        case 131:  phimin_text = w;                  break;
        case 132:  phimax_text = w;                  break;
        case 133:  laymin_text = w;                  break;
        case 134:  laymax_text = w;                  break;
        case 135:  caeh_thresh = w;                  break;
        case 136:  cachcaeh_text = w;                break;
        case 137:  evout_window = w;                 break;
        case 138:  etmin_text = w;                   break;
        case 139:  etmax_text = w;                   break;
        case 140:  encahits = w;                     break;
        case 141:  discahits = w;                    break;
        case 142:  enudst = w;                       break;
        case 143:  disudst = w;                      break;
        case 144:  encleanem = w;	                 break;
        case 145:  discleanem = w;                   break;
        case 146:  encleanmu = w;                    break;
        case 147:  discleanmu = w;                   break;
        case 148:  encafix = w;                      break;
        case 149:  discafix = w;                     break;
        default:
            printf(" ***Illegal tag presented 'save_text'***\n");
            break;
        }
}

