/* 
        d0x_c.h
         Created           : 26-AUG-1992 by Drew Baden
*/
#include <stdlib.h>
#include <math.h>
#include <Xm/Xm.h>
#include <Xm/List.h>
#include <Xm/Text.h>
#include <Xm/ScrollBar.h>
#include <Xm/FileSB.h>
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <Xm/Scale.h>
#include <Xm/MessageB.h>
#include <Xm/SelectioB.h>
#include <Mrm/MrmAppl.h>             /* Motif Toolkit and MRM */
#include <X11/Intrinsic.h>
/*
      here are all of my function declarations
*/
void SetDefaultCursor(Widget);
void SetWatchCursor(Widget);
void SetLabel(Widget,char*);
void cupcase(char*);
void cfd0util(int*,int*);
void xgetchar(char*,char*,char*,int*);
void squeeze(char*,char);
void cerrmsg(char*);
int strcx(char*,char*);
void warning(char*);
void MakeT4();
void padcat(char*,int,char*);
void cfcontrol(int*,int*,int*);
void MakeRCMC();
void FillRcMC();
void ResetPN();
void DispRZ();
void NewRow(int,char*,int);
void DrawCircle(Widget,GC,int,int,int);
void MakeRCPhysics();
void FillRcPhysics(int);
void cp_to_n(int,int);
void cp_from_n(int,int);
void ReportVertices();
void MakeRCLables();
void MakeRCMCLables();
void NewMCRow();
void change_phi(int,int*);
void change_theta(int,int*);
void cerrmsg(char*);
/* the next set are for d0x.c */
void myquit(Widget,int*,unsigned long*);
void help_d0x(Widget,int*,unsigned long*);
void d0_init(Widget,int*,unsigned long*);
void save_text(Widget,int*,unsigned long*);
void fileopen(Widget,int*,unsigned long*);
void control(Widget,int*,unsigned long*);
void czebra(Widget,int*,unsigned long*);
void cd0util(Widget,int*,unsigned long*);
void csetmode(Widget,int*,unsigned long*);
void csearch(Widget,int*,unsigned long*);
void cinterrupt(Widget,int*,unsigned long*);
void cset_store(Widget,int*,unsigned long*);
void cselect(Widget,int*,unsigned long*);
void cformat(Widget,int*,unsigned long*);
void czeblst(Widget,int*,unsigned long*);
void cnavigate(Widget,int*,unsigned long*);
void cagain(Widget,int*,unsigned long*);
void csetcurtop(Widget,int*,unsigned long*);
void xdecode(Widget,int*,unsigned long*);
void crawdata(Widget,int*,unsigned long*);
void evout(Widget,int*,unsigned long*);
void cprintit(Widget,int*,unsigned long*);
void undotree(Widget,int*,unsigned long*);
void csetout(Widget,int*,unsigned long*);
void cphys(Widget,int*,unsigned long*);
void cseljet(Widget,int*,unsigned long*);
void selectc(Widget,int*,XmToggleButtonCallbackStruct*);
void cinfo(Widget,int*,unsigned long*);
void selecor(Widget,int*,XmToggleButtonCallbackStruct*);
void phirot(Widget,int*,XmArrowButtonCallbackStruct*);
void thetarot(Widget,int*,XmArrowButtonCallbackStruct*);
void psirot(Widget,int*,XmArrowButtonCallbackStruct*);
void retree(Widget,int*,unsigned long*);
void d0xquery(Widget,int*,XmListCallbackStruct*);
void setquery(Widget,int*,unsigned long*);
void dxhisto(Widget,int*,unsigned long*);
void d0tracks(Widget,int*,unsigned long*);
void d0caeh(Widget,int*,unsigned long*);
void cfiletype(Widget,int*,unsigned long*);
void DXmInitialize();
void d0xhdraw(Widget,int*,unsigned long*);
void cbranch(Widget,int*,int,int*,int*,int*);
void evout(Widget,int*,unsigned long*);
void myquit(Widget,int*,unsigned long*);

#ifdef D0FLAVOR
void picaehn_(int*,int*,int*,int*,int*,int*,int*,int*,int*,float*,float*);
void picaeh_(int*,int*,char*,char*,int*,int*,int*,int*,int*,int*,float*,float*);
void fd0util_(int*,int*,char*);
void foenable_(int*);
void fsetoff_(int*);
void fbankok_(int*,int*,int*,int*);
int nzbank_(int*,int*);
void fsetlin_(int*);
void fzeblst_(char*,int*);
void xfopen_(char*,int*,char*,int*);
void ffiletype_(int*,int*);
void xfclose_();
void fformat_(int*);
void finterrupt_(int*);
void fclose4_();
void fncol_(int*,int*,int*);
void ftcol_(int*,char*,int*);
void frcval_(int*,int*,int*,float*);
void fopen4_(char*,int*,int*);
void frcrow_(int*,int*,float*);
int d0xuser_(int*);
int d0xuser_init_(int*);
int d0xuser_finish_(int*);
int d0xuser_talk_(int*);
void d0xd0dad_(int*,int*);
void setd0dad_(int*,char*,int*);
void fcontrol_(int*,int*,int*);
void taustuff_(int*,float*);
void jetstuff_(int*,int*,float*);
void fgobjmc_(int*,int*,int*,int*,int*,float*,float*,float*,float*,
            float*,float*,float*,float*,float*,float*,int*,float*,float*);
void muostuff_(int*,float*);
void pnutstuff_(int*,float*);
void find_wlnu_(float*,float*,float*,float*,float*,int*);
void det_eta_(float*,float*,float*);
void fgcatdt_(int*);
void fgcatdts_(int*,float*,int*,int*);
void fgemlist_(int*,float*,char*,int*,float*);
void fgcleanem_(int*,int*);
void fgobj_(float*,int*,int*,int*,int*,int*,int*,int*,int*,
    int*,char*,char*,char*,char*,char*,char*,int*,float*,float*,
    float*,float*,float*,float*,float*,float*,float*,float*,
    float*,float*,float*,int*,float*,float*,int*,int*);
void iscafix_(int*);
void frawdata_(int*,int*,char*,int*);
void fsearch_(int*,char*,int*);
void seljet_(Widget,int*,unsigned long*);
void fmode_(int*);
void fsetout_(Widget,int*,unsigned long*);
void fset_store_(int*);
void treestate_(int*,int*);
void zebstate_(int*,int*);
void fblist_(int*,char*,int*);
void datastate_(int*,int*);
void numcdtks_(int*,int*);
void cdstuff_(int*,int*,int*,int*,int*,int*,int*,int*,
    int*,int*,int*,int*,int*,int*,int*,int*,
    int*,int*,int*,int*,int*,int*,int*,int*,
    int*,int*,int*,int*,int*,int*,int*,float*,
    float*,float*,float*,float*,float*,float*,
    float*,float*,float*,float*,float*,float*,
    float*,float*);
void numvtxtks_(int*,int*);
void vtxstuff_(int*,int*,int*,int*,int*,int*,int*,int*,int*,int*,
    int*,int*,int*,int*,int*,int*,int*,int*,int*,float*,float*,
    float*,float*,float*,float*,float*,float*,float*,float*,float*,
    float*,float*,float*,float*,float*);
void numfdtks_(int*,int*);
void fdcstuff_(int*,int*,int*,int*,int*,int*,int*,int*,int*,int*,float*,
    float*,int*,int*,int*,int*,int*,int*,int*,int*,float*,float*,float*,
    float*,float*,float*,float*,float*,float*,float*,float*,int*,
    float*,float*,float*,float*,float*,int*);
void setreq_(int*,int*);
void fzebra_(int*,char*);
void mapinit_();
void freqzeb_(Widget,char*);
void loadreq_(int*,int*,int*,int*,char*,char*,int*,int*,int*,
    int*,char*,int*);
void loadtreq_(int*,char*,int*,int*);
void hdelet_(int*);
void chcanh_(int*);
int hexist_(int*);
void chgive_(int*,char*,int*,int*,float*,float*,float*,float*,
    float*,float*,float*,float*,float*,int*);
void chbook1_(int*,char*,int*,int*,float*,float*,float*);
void chseth_(int*,char*,char*,int*,int*,int*);
void chout_(char*,int*);
void chreset_(int*);
void hfitga_(int*,float*,float*,float*,float*,int*,float*);
void hfitex_(int*,float*,float*,float*,int*,float*);
void hfitpo_(int*,int*,float*,float*,int*,float*);
void chcann_(int*);
void chbookn_(int*,char*,int*);
void chsetn_(int*,char*,char*,int*,int*,int*);
void chfilln_(int*,int*,char*,int*,int*,float*,float*,float*);
void fevout_(int*,int*,int*,char*);
void cnout_(int*);
void setlist_(int*,char*,int*);
void cllist_(int*,char*);
int d0xuser_finish_(int*);
void exit(int);

#else
void picaehn(int*,int*,int*,int*,int*,int*,int*,int*,int*,float*,float*);
void picaeh(int*,int*,char*,char*,int*,int*,int*,int*,int*,int*,float*,float*);
void fd0util(int*,int*,char*);
void foenable(int*);
void fsetoff(int*);
void fbankok(int*,int*,int*,int*);
int nzbank(int*,int*);
void fsetlin(int*);
void fzeblst(char*,int*);
void xfopen(char*,int*,char*,int*);
void ffiletype(int*,int*);
void xfclose();
void fformat(int*);
void finterrupt(int*);
void fclose4();
void fncol(int*,int*,int*);
void ftcol(int*,char*,int*);
void frcval(int*,int*,int*,float*);
void fopen4(char*,int*,int*);
void frcrow(int*,int*,float*);
int d0xuser(int*);
int d0xuser_init(int*);
int d0xuser_finish(int*);
int d0xuser_talk(int*);
void d0xd0dad(int*,int*);
void setd0dad(int*,char*,int*);
void fcontrol(int*,int*,int*);
void taustuff(int*,float*);
void jetstuff(int*,int*,float*);
void fgobjmc(int*,int*,int*,int*,int*,float*,float*,float*,float*,
            float*,float*,float*,float*,float*,float*,int*,float*,float*);
void muostuff(int*,float*);
void pnutstuff(int*,float*);
void find_wlnu(float*,float*,float*,float*,float*,int*);
void det_eta(float*,float*,float*);
void fgcatdt(int*);
void fgcatdts(int*,float*,int*,int*);
void fgemlist(int*,float*,char*,int*,float*);
void fgcleanem(int*,int*);
void fgobj(float*,int*,int*,int*,int*,int*,int*,int*,int*,
    int*,char*,char*,char*,char*,char*,char*,int*,float*,float*,
    float*,float*,float*,float*,float*,float*,float*,float*,
    float*,float*,float*,int*,float*,float*,int*,int*);
void iscafix(int*);
void frawdata(int*,int*,char*,int*);
void fsearch(int*,char*,int*);
void seljet(Widget,int*,unsigned long*);
void fmode(int*);
void fsetout(Widget,int*,unsigned long*);
void fset_store(int*);
void treestate(int*,int*);
void zebstate(int*,int*);
void fblist(int*,char*,int*);
void datastate(int*,int*);
void numcdtks(int*,int*);
void cdstuff(int*,int*,int*,int*,int*,int*,int*,int*,
    int*,int*,int*,int*,int*,int*,int*,int*,
    int*,int*,int*,int*,int*,int*,int*,int*,
    int*,int*,int*,int*,int*,int*,int*,float*,
    float*,float*,float*,float*,float*,float*,
    float*,float*,float*,float*,float*,float*,
    float*,float*);
void numvtxtks(int*,int*);
void vtxstuff(int*,int*,int*,int*,int*,int*,int*,int*,int*,int*,
    int*,int*,int*,int*,int*,int*,int*,int*,int*,float*,float*,
    float*,float*,float*,float*,float*,float*,float*,float*,float*,
    float*,float*,float*,float*,float*);
void numfdtks(int*,int*);
void fdcstuff(int*,int*,int*,int*,int*,int*,int*,int*,int*,int*,float*,
    float*,int*,int*,int*,int*,int*,int*,int*,int*,float*,float*,float*,
    float*,float*,float*,float*,float*,float*,float*,float*,int*,
    float*,float*,float*,float*,float*,int*);
void setreq(int*,int*);
void fzebra(int*,char*);
void mapinit();
void freqzeb(Widget,char*);
void loadreq(int*,int*,int*,int*,char*,char*,int*,int*,int*,
    int*,char*,int*);
void loadtreq(int*,char*,int*,int*);
void hdelet(int*);
void chcanh(int*);
int hexist(int*);
void chgive(int*,char*,int*,int*,float*,float*,float*,float*,
    float*,float*,float*,float*,float*,int*);
void chbook1(int*,char*,int*,int*,float*,float*,float*);
void chseth(int*,char*,char*,int*,int*,int*);
void chout(char*,int*);
void chreset(int*);
void hfitga(int*,float*,float*,float*,float*,int*,float*);
void hfitex(int*,float*,float*,float*,int*,float*);
void hfitpo(int*,int*,float*,float*,int*,float*);
void chcann(int*);
void chbookn(int*,char*,int*);
void chsetn(int*,char*,char*,int*,int*,int*);
void chfilln(int*,int*,char*,int*,int*,float*,float*,float*);
void fevout(int*,int*,int*,char*);
void cnout(int*);
void setlist(int*,char*,int*);
void cllist(int*,char*);
int d0xuser_finish(int*);

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
extern Widget  raw_header;             /*  26 */
extern Widget  raw_trailer;            /*  27 */
extern Widget  crate_label;            /*  28 */
extern Widget  goto_crate;             /*  29 */
extern Widget  goto_offset;            /*  30 */
extern Widget  raw_status;             /*  31 */
extern Widget  numrow_text;            /*  32 */
extern Widget  crates_text;            /*  33 */
extern Widget  tree_scroll;            /*  34 */
extern Widget  data_scroll;            /*  35 */
extern Widget  physics_bull;           /*  36 */
extern Widget  physics_scroll;         /*  37 */
extern Widget  anglelab;               /*  38 */
extern Widget  drawitxy;               /*  39 */
extern Widget  drawityz;               /*  40 */
extern Widget  drawitxz;               /*  41 */
extern Widget  vertlab;                /*  42 */
extern Widget  drawitlego;             /*  43 */
extern Widget  phi0lab;                /*  44 */
extern Widget  physhelp;               /*  45 */
extern Widget  theta0lab;              /*  46 */
extern Widget  psi0lab;                /*  47 */
extern Widget  physhelp_bull;          /*  48 */
extern Widget  outtext;                /*  49 */
extern Widget  listbull;               /*  50 */
extern Widget  emlist;                 /*  51 */
extern Widget  drawemlong;             /*  52 */
extern Widget  scalefac;               /*  53 */
extern Widget  thresh;                 /*  54 */
extern Widget  zebscroll;              /*  55 */
extern Widget  pmuolist;               /*  56 */
extern Widget  defined;                /*  57 */
extern Widget  thevalue;               /*  58 */
extern Widget  boffset;                /*  59 */
extern Widget  req1list;               /*  60 */
extern Widget  reqtext;                /*  61 */
extern Widget  reqlists;               /*  62 */
extern Widget  reqvalue;               /*  63 */
extern Widget  reqzeb;                 /*  64 */
extern Widget  rpositive;              /*  65 */
extern Widget  rnegative;              /*  66 */
extern Widget  qhelp;                  /*  67 */
extern Widget  wrand;                  /*  68 */
extern Widget  wror;                   /*  69 */
extern Widget  histon;                 /*  70 */
extern Widget  histoff;                /*  71 */
extern Widget  xhwhat;                 /*  72 */
extern Widget  xhtitle;                /*  73 */
extern Widget  xhid;                   /*  74 */
extern Widget  xhnbin;                 /*  75 */
extern Widget  xhbinl;                 /*  76 */
extern Widget  xhbinh;                 /*  77 */
extern Widget  xhstore;                /*  78 */
extern Widget  xhmake;                 /*  79 */
extern Widget  hdraw;                  /*  80 */
extern Widget  deflist;                /*  81 */
extern Widget  rstop;                  /*  82 */
extern Widget  rcont;                  /*  83 */
extern Widget  low1text;               /*  84 */
extern Widget  low1scale;              /*  85 */
extern Widget  hightext;               /*  86 */
extern Widget  highscale;              /*  87 */
extern Widget  numtext;                /*  88 */
extern Widget  numscale;               /*  89 */
extern Widget  ocount;                 /*  90 */
extern Widget  oopenb;                 /*  91 */
extern Widget  oclosb;                 /*  92 */
extern Widget  xnid;                   /*  93 */
extern Widget  xnmake;                 /*  94 */
extern Widget  pnuttext;               /*  95 */
extern Widget  physmc;                 /*  96 */
extern Widget  physmctext;             /*  97 */
extern Widget  isajtoggle;             /*  98 */
extern Widget  isaqtoggle;             /*  99 */
extern Widget  fzdestination;          /* 100 */
extern Widget  fz_mode;                /* 101 */
extern Widget  fztype;                 /* 102 */
extern Widget  zpath;                  /* 103 */
extern Widget  stattext;               /* 104 */
extern Widget  mcanglelab;             /* 105 */
extern Widget  microcasc;              /* 106 */
extern Widget  cafixcasc;              /* 107 */
extern Widget  cleanemtext;            /* 108 */
extern Widget  navaddresslab;          /* 109 */
extern Widget  navchainlab;            /* 110 */
extern Widget  d0dadrun;               /* 111 */
extern Widget  d0dadevent;             /* 112 */
extern Widget  d0dadcattext;           /* 113 */
extern Widget  nav_main;               /* 114 */
extern Widget  d0dad_main;             /* 115 */
extern Widget  mcphys_main;            /* 116 */
extern Widget  raw_main;               /* 117 */
extern Widget  jettext;                /* 118 */
extern Widget  ptautext;               /* 119 */
extern Widget  updtimer;               /* 120 */
extern Widget  trktext;                /* 121 */
extern Widget  trackvariabs;           /* 122 */
extern Widget  diskortape;             /* 123 */
extern Widget  andortext;              /* 124 */
extern Widget  andorresult;            /* 125 */
extern Widget  track_bull;             /* 126 */
extern Widget  caeh_text;              /* 127 */
extern Widget  caeh_bull;              /* 128 */
extern Widget  etamin_text;            /* 129 */
extern Widget  etamax_text;            /* 130 */
extern Widget  phimin_text;            /* 131 */
extern Widget  phimax_text;            /* 132 */
extern Widget  laymin_text;            /* 133 */
extern Widget  laymax_text;            /* 134 */
extern Widget  caeh_thresh;            /* 135 */
extern Widget  cachcaeh_text;          /* 136 */
extern Widget  evout_window;           /* 137 */
extern Widget  etmin_text;             /* 138 */
extern Widget  etmax_text;             /* 139 */
extern Widget  encahits;               /* 140 */
extern Widget  discahits;              /* 141 */
extern Widget  enudst;                 /* 142 */
extern Widget  disudst;                /* 143 */
extern Widget  encleanem;	           /* 144 */
extern Widget  discleanem;             /* 145 */
extern Widget  encleanmu;              /* 146 */
extern Widget  discleanmu;             /* 147 */
extern Widget  encafix;                /* 148 */
extern Widget  discafix;               /* 149 */
extern Widget  toplevel_widget;
extern Widget  curtop;  /* current top level - could be toplevel or ... */
extern Widget  curtext; /* current text window for errors, etc.  */
extern XtAppContext appcontext;
typedef union {
    char name[3];
    int iname;
} EQUIV;
