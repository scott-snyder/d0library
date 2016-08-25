      INTEGER FUNCTION LIBBIG(TEXT,LINE,COLUMN,ATTR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Put a string at a certain line and column
C-                         in BIG letters. VAX-specific. (Uses SMG)
C-
C-   Inputs  : TEXT:   String to output
C-             LINE:   Line to put text at.
C-             COLUMN: Column to put string at.
C-             ATTR:   Attribute for display of text. (see SMG manual)
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-   Modified    13-AUG-1992 sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) TEXT
      INTEGER LINE,COLUMN,ATTR
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER SMG$PUT_CHARS_WIDE,I
C----------------------------------------------------------------------
C&IF IBMAIX
C&      I=LOC(ATTR)
C&ELSE
      I=LOC(ATTR)
C&ENDIF
      IF(I.GT.100) THEN
         LIBBIG=SMG$PUT_CHARS_WIDE(MAINID,TEXT,LINE,COLUMN,0,ATTR,
     &                             %VAL(0))
      ELSE
         LIBBIG=SMG$PUT_CHARS_WIDE(MAINID,TEXT,LINE,COLUMN,0,
     &                             %VAL(0),%VAL(0))
      ENDIF
      RETURN
      END
