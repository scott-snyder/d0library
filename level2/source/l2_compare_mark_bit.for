      SUBROUTINE L2_COMPARE_MARK_BIT(BIT,IWORD,STRING)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-        mark a bit as on or off in a string
C-   Inputs  : BIT  [I]   which bit to look at (will be mod'd by 32)
C-             IWORD [I]  the word the bit is found in
C-   Outputs : STRING [C*128] an array of flags
C-   Controls: 
C-
C-   Created   9-APR-1992   Drew Baden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IWORD,BIT,I
      CHARACTER*128 STRING
C----------------------------------------------------------------------
C
      I = MOD(BIT,32)
      IF (BTEST(IWORD,I)) THEN
        STRING(BIT+1:BIT+1) = '+'
      ELSE
        STRING(BIT+1:BIT+1) = ' '
      ENDIF
C
      RETURN
      END
