      INTEGER FUNCTION LIBERL(LINE,COLUMN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Erase the text on a line from a given column
C-                         VAX-specific (Uses SMG)
C-
C-   Inputs  : LINE:   Line to erase on.
C-             COLUMN: Column within LINE were erase starts
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
      INTEGER SMG$ERASE_LINE
C----------------------------------------------------------------------
      LIBERL=SMG$ERASE_LINE(MAINID,LINE,COLUMN)
      RETURN
      END
