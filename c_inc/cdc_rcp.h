/* 
        USR$ROOT2:[KLOPFENSTEIN.LEVEL2.TEST]CDC_RCP.H;
         Created           : 19-SEP-1993 by Chris Klopfenstein

   header file containing RCP parameters for L2CDC
*/
 
#define INL2  	0	/* false */

#define VTXHIT 	0	/* false */
#define CDCHIT	1	/* true */
#define FDCHIT	0	/* false */
#define TRDHIT	0	/* false */

#define MAXCNT 	255
#define FFRQCY	106.0
#define BINTIME	9.433962	/* 1000./FFRQCY	*/

#define PULTH1_SW	5
#define PULTH1_DL	3
#define PULTH2_SW	10
#define PULTH2_DL	10
#define PULTH3_SW	10
#define PULTH3_DL	10
#define PULMAX_SW	10
#define PULMAX_DL	20
#define PULWEI_SW	1.2
#define PULWEI_DL	1.2

#define TRGOFF	-230.0
#define TSCALE	64  
#define TMAX	4096
/*#define TSCALE	512
#define TMAX	512   */ /* time in units of FADC bins */
#define ARMAX	16383
#define WIDMAX	15

