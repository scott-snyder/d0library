      INTEGER FUNCTION LIBCUR(LINE,COLUMN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Place cursor at LINE and COLUMN
C-                         VAX-specific (Uses SMG)
C-
C-   Inputs  : LINE:   Line to put cursor on
C-             COLUMN: Column to put cursor on.
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-   Updated    19-SEP-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LINE,COLUMN
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER SMG$SET_CURSOR_ABS
C----------------------------------------------------------------------
      LIBCUR=SMG$SET_CURSOR_ABS(MAINID,LINE,COLUMN)
      RETURN
      END
