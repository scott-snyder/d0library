      SUBROUTINE ENDCLR
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Output blanks on last line of display to avoid
C-                         big letters when scrolling continues. 
C-                         VAX-specific.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-   Modified    13-AUG-1992 sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:SMGCOM.INC'
C----------------------------------------------------------------------
      INTEGER SMG$PUT_CHARS,I
      I=SMG$PUT_CHARS(MAINID,' ',PBROWS,1,0,
     &                %VAL(0),%VAL(0),%VAL(0),%VAL(0))
      RETURN
      END
