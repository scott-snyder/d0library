      SUBROUTINE GETSCR(ROWS,COLUMS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the number of rows and columns in
C-                         display to user.
C-
C-   Inputs  : None
C-   Outputs : ROWS : current number of rows in display (PBROWS)
C-             COLUMS: Current number of columns (PBCOLS)
C-   Controls: None
C-
C-   Created  29-AUG-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ROWS,COLUMS
      INCLUDE 'D0$INC:SMGCOM.INC'
C----------------------------------------------------------------------
      ROWS=PBROWS
      COLUMS=PBCOLS
  999 RETURN
      END
