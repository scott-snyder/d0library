      FUNCTION L2_NXT_INT_DIFF(I1,I2,IR1,IR2,C)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-        increment pointers and compare two integer variables in ZEBCOM; 
C-   Inputs  : I1,I2    [I] pointers
C-   Outputs : IR1,IR2  [I] values found
C-             C        [C*1]  '*' if different; else ' '
C-   Controls: 
C-
C-   Created   9-APR-1992   Drew Baden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER I1,I2,IR1,IR2
      LOGICAL L2_NXT_INT_DIFF
      CHARACTER*1 C
C----------------------------------------------------------------------
C
      I1 = I1 + 1
      I2 = I2 + 1
      IR1 = IQ(I1)
      IR2 = IQ(I2)
      L2_NXT_INT_DIFF = IR1.NE.IR2
      IF (L2_NXT_INT_DIFF) THEN
        C = '*'
      ELSE
        C = ' '
      ENDIF
C
      RETURN
      END
