      INTEGER FUNCTION LIBNXT(TEXT,ATTR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Put TEXT on next line of display. VAX-specific
C-
C-   Inputs  : TEXT:  String to be output
C-             ATTR:  Attributes to use in output (high-lite, reverse etc.)
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-   Updated    27-SEP-1991   Herbert Greenlee
C-   Modified    13-AUG-1992 sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) TEXT
      INTEGER ATTR
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER SMG$PUT_CHARS,I,SMG$RETURN_CURSOR_POS,J,K,LIBLIN
C----------------------------------------------------------------------
      I=SMG$RETURN_CURSOR_POS(MAINID,J,K)
      IF(J.EQ.PBROWS) THEN
         I=LIBLIN(' ',0)
         J=PBROWS-1
      ENDIF
      LIBNXT=SMG$PUT_CHARS(MAINID,TEXT,J+1,1,0,ATTR,%VAL(0),%VAL(0))
      RETURN
      END
