      SUBROUTINE ADDSTR(STRNG1,STRNG2,STRNG3,LEN3)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Removes trailing blanks from
C-                        character STRNG1 and concatenates
C-                        STRNG2 to it. 
C-
C-   Inputs  : STRNG1,STRNG2
C-   Outputs : STRNG3 = concatenation of STRNG1,STRNG2
C-             LEN3 = Length of STRNG3
C-   Controls: None
C-
C-   Created   5-OCT-1988   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) STRNG1,STRNG2,STRNG3
      INTEGER L,I,LEN3
C----------------------------------------------------------------------
      L = LEN(STRNG1)
      DO 10 I = L , 1,-1 
        IF(STRNG1(I:I).NE.' ')GO TO 11
   10 CONTINUE
   11 CONTINUE
      IF(I.EQ.0)I=1
      STRNG3 = STRNG1(1:I)//STRNG2
      LEN3 = I + LEN(STRNG2)
      END
