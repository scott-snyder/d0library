      INTEGER FUNCTION LIBERP(LINE1,LINE2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Erase the text between two lines
C-                         VAX-specific (Uses SMG)
C-
C-   Inputs  : LINE1: Line to start erase from.
C-             LINE2: Line to erase till.
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LINE1,LINE2
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER SMG$ERASE_DISPLAY,SMG$SET_CURSOR_ABS,I
C----------------------------------------------------------------------
      LIBERP=SMG$ERASE_DISPLAY(MAINID,LINE1,1,LINE2,PBCOLS)
      I=SMG$SET_CURSOR_ABS(MAINID,LINE1,1)
      RETURN
      END
