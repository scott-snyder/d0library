      SUBROUTINE PFLABL(PF1,PF2,PF3,PF4)
C-   Purpose and Methods : Put PF-key labels on bottom line of display
C-   Inputs  : PF1..PF4: Labels for the four PF-keys respectively
      INCLUDE 'D0$GRAPHICS_UTIL$SGICOMPACK:MAINPACK.INC'
      CHARACTER*(*) PF1,PF2,PF3,PF4
      PFSTR(1)=PF1
      PFSTR(2)=PF2
      PFSTR(3)=PF3
      PFSTR(4)=PF4
      RETURN
      END
