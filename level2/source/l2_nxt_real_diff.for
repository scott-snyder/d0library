      FUNCTION L2_NXT_REAL_DIFF(I1,I2,R1,R2,THR,C)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-        increment pointers and compare two real variables in ZEBCOM; 
C-   Inputs  : I1,I2  [I] pointers
C-             THR    [R] tolerance of match
C-   Outputs : R1,R2  [R] values found
C-             C      [C*1]  '*' if different; else ' '
C-   Controls: 
C-
C-   Created   9-APR-1992   Drew Baden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER I1,I2
      REAL R1,R2,THR
      LOGICAL L2_NXT_REAL_DIFF
      CHARACTER*1 C
C
      I1 = I1 + 1
      I2 = I2 + 1
      R1 = Q(I1)
      R2 = Q(I2)
      L2_NXT_REAL_DIFF = ABS(R1-R2).GT.THR
      IF (L2_NXT_REAL_DIFF) THEN
        C = '*'
      ELSE
        C = ' '
      ENDIF
C
      RETURN
      END
