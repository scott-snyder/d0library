      INTEGER FUNCTION LIBCAR(LINE,COLUMN,ATTR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Put A CARAT SYMBOL at a given line and column
C-                         VAX-specific (Uses SMG)
C-
C-   Inputs  : LINE:   Line to put symbol at.
C-             COLUMN: Column to put the symbol at.
C-             ATTR:   Display attribute (see SMG manual)
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-   Updated    30-SEP-1991   Herbert Greenlee
C-   Modified   14-AUG-1992   sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LINE,COLUMN,ATTR
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER SMG$PUT_CHARS,I
C&IF VAXVMS
      INTEGER SMG$ALLOW_ESCAPE
      CHARACTER*(6) TEXT
C----------------------------------------------------------------------
      TEXT=CHAR(27)//')0'//CHAR(14)//'`'//CHAR(15)
      I=SMG$ALLOW_ESCAPE(MAINID,1)
      I=%LOC(ATTR)
      IF(I.GT.100) THEN
        LIBCAR=SMG$PUT_CHARS(MAINID,TEXT,LINE,COLUMN,0,ATTR,,)
      ELSE
        LIBCAR=SMG$PUT_CHARS(MAINID,TEXT,LINE,COLUMN,0,,,)
      ENDIF
      I=SMG$ALLOW_ESCAPE(MAINID,0)
C&ELSE
C&      LIBCAR=SMG$PUT_CHARS(MAINID,'*',LINE,COLUMN,0,ATTR,
C&     &                     %VAL(0),%VAL(0))
C&ENDIF
      RETURN
      END
