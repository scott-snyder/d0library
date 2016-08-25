      INTEGER FUNCTION LIBPUT(TEXT,LINE,COLUMN,ATTR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Output text on a line at a given column
C-                         VAX-specific (Uses SMG)
C-
C-   Inputs  : TEXT:   String to output
C-             LINE:   Line to output on.
C-             COLUMN: Column within LINE were erase starts
C-             ATTR:   Display attribute (see SMG manual)
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-   Updated    27-SEP-1991   Herbert Greenlee
C-   Modified    13-AUG-1992 sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) TEXT
      INTEGER LINE,COLUMN,ATTR
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER SMG$PUT_CHARS,I
C----------------------------------------------------------------------
C&IF IBMAIX
C&      I=LOC(ATTR)
C&ELSE
      I=LOC(ATTR)
C&ENDIF
      IF(I.GT.100) THEN
         LIBPUT=SMG$PUT_CHARS(MAINID,TEXT,LINE,COLUMN,0,ATTR,
     &                        %VAL(0),%VAL(0))
      ELSE
         LIBPUT=SMG$PUT_CHARS(MAINID,TEXT,LINE,COLUMN,0,
     &                        %VAL(0),%VAL(0),%VAL(0))
      ENDIF
      RETURN
      END
