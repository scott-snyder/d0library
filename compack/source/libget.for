      INTEGER FUNCTION LIBGET(LINE,COLUMN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get current cursor position.
C-                         VAX-specific (Uses SMG)
C-
C-   Inputs  : None 
C-   Outputs : LINE:   Line were cursor is
C-             COLUMN: Column were cursor is
C-   Controls: None
C-
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-   Updated    19-SEP-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LINE,COLUMN
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER SMG$RETURN_CURSOR_POS,ISTAT
C----------------------------------------------------------------------
      ISTAT=SMG$RETURN_CURSOR_POS(MAINID,LINE,COLUMN)
      LIBGET=ISTAT
      RETURN
      END
