      FUNCTION LIBCR1(LINE,COLUMN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Place cursor at LINE and COLUMN of window
C-   with ID MINID1.
C-                         VAX-specific (Uses SMG)
C-
C-   Inputs  : LINE:   Line to put cursor on
C-             COLUMN: Column to put cursor on.
C-   Outputs : None
C-   Controls: None
C-
C-   Created 26-JUL-1990   Jan Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LIBCR1,LINE,COLUMN
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER SMG$SET_CURSOR_ABS
C----------------------------------------------------------------------
      LIBCR1 = SMG$SET_CURSOR_ABS(MINID1,LINE,COLUMN)
      RETURN
      END
