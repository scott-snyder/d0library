      SUBROUTINE ADD_JETS(JET1,JET2,JET3)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : ADD 4 VECTORS OF JETS
C-
C-   Inputs  : JET1,JET2 INPUT
C-   Outputs : JET3 OUTPUT
C-   Controls: 
C-
C-   Created  24-JUN-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    JET1(*),JET2(*),JET3(*)
      INTEGER I
C----------------------------------------------------------------------
      DO I = 1 , 4
        JET3(I) = JET1(I) + JET2(I)
      ENDDO
  999 RETURN
      END
