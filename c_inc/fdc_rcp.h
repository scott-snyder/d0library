/* 
        USR$ROOT2:[KLOPFENSTEIN.LEVEL2.TEST]FDC_RCP.H;
         Created           : 28-JAN-1994 by Chris Klopfenstein
*/
 
#define PULMAX_SW	     20
#define PULMAX_DL      30
#define PULWEI_SW        0.60
#define PULWEI_DL        1.30
#define PULWEI_SW_BIG    1.00
#define PULWEI_DL_BIG    1.30
#define PULWEI_CUT_SW    210.0
#define PULWEI_CUT_DL   9999.0
#define PULTH1_SW    1.00
#define PULTH2_SW    2.20
#define PULTH3_SW    16.00
#define PULTH1_DL    1.00
#define PULTH2_DL    2.40
#define PULTH3_DL    18.00
#define MAX_BIN_PHI    512		/* last valib bin for phi chamber */
#define MAX_BIN_TSW    512		/* theta sense wires */
#define MAX_BIN_DL     512		/* delay lines */
#define MAXCNT         255
#define CLOSE_PEAK       1      /* TRUE to use CLOSE_PEAK method in FDPULS */
#define WIDE_THETA       8      /* Point at which a theta SW hit is too wide */
#define RESET_THETA      5      /* Width to reset theta SW hit to */
#define WIDE_DL          9      /* DL hit too wide */
#define RESET_DL         5      /* Reset DL hit */
#define WIDE_PHI         7      /* Phi hit too wide */
#define RESET_PHI        4      /* Reset Phi hit */
#define HITS_PER_WIRE   20		/* max hits/wire */
#define MAXWIRT			 7		/* max theta wire number */
#define MIN_AVE_DIFF_PHI  3		/* flatness cut */
#define MIN_AVE_DIFF_THETA  1		/* flatness cut */
#define TSCALE         64      /* Scale factor for time in FCHT/CDH3 bank */
#define TMAX           262143  /* maximum value for scaled time */
#define ARMAX          16383   /* maximum value for scaled pulse */
#define WIDMAX         15      /* maximum value for pulse width */
