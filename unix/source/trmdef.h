/* !*** MODULE $TRMDEF *** */
/* ! */
/* !  Define symbols for the item list QIO format */
/* ! */
/* !  Item list type codes */
#define TRM$_MODIFIERS 0x00000000
#define TRM$_EDITMODE 0x00000001
#define TRM$_TIMEOUT 0x00000002
#define TRM$_TERM 0x00000003
#define TRM$_PROMPT 0x00000004
#define TRM$_INISTRNG 0x00000005
#define TRM$_PICSTRNG 0x00000006
#define TRM$_FILLCHR 0x00000007
#define TRM$_INIOFFSET 0x00000008
#define TRM$_ALTECHSTR 0x00000009
#define TRM$_ESCTRMOVR 0x0000000A
#define TRM$_LASTITM 0x0000000B    /* !  must remain the last item */
/* ! */
/* !  Editmode type codes */
/* ! */
#define TRM$K_EM_DEFAULT 0x00000000
#define TRM$K_EM_RDVERIFY 0x00000001
#define TRM$M_TM_NOECHO 0x00000040
#define TRM$M_TM_TIMED 0x00000080
#define TRM$M_TM_CVTLOW 0x00000100
#define TRM$M_TM_NOFILTR 0x00000200
#define TRM$M_TM_DSABLMBX 0x00000400
#define TRM$M_TM_PURGE 0x00000800
#define TRM$M_TM_TRMNOECHO 0x00001000
#define TRM$M_TM_REFRESH 0x00002000
#define TRM$M_TM_ESCAPE 0x00004000
#define TRM$M_TM_NOEDIT 0x00008000
#define TRM$M_TM_NORECALL 0x00010000
#define TRM$M_TM_R_JUST 0x00020000
#define TRM$M_TM_AUTO_TAB 0x00040000
#define TRM$M_CV_UPPER 0x00000001
#define TRM$M_CV_LOWER 0x00000002
#define TRM$M_CV_NUMERIC 0x00000004
#define TRM$M_CV_NUMPUNC 0x00000008
#define TRM$M_CV_PRINTABLE 0x00000010
#define TRM$M_CV_ANY 0x00000020

