      INTEGER FUNCTION LIBERA(LINE,COLUMN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Erase page and leave cursor at LINE and COLUMN 
C-                         VAX-specific (Uses SMG)
C-
C-   Inputs  : LINE:   Line to leave cursor on.
C-             COLUMN: Column to leave cursor on.
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-   Updated    27-SEP-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LINE,COLUMN
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER SMG$ERASE_DISPLAY,SMG$SET_CURSOR_ABS,I
C----------------------------------------------------------------------
      LIBERA=SMG$ERASE_DISPLAY(MAINID,1,1,PBROWS,PBCOLS)
      I=SMG$SET_CURSOR_ABS(MAINID,LINE,COLUMN)
      RETURN
      END
