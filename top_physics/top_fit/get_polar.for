      SUBROUTINE GET_POLAR(VEC7,VEC3)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GIVEN 4VECTOR + ET,ETA,PHI OF PARTICLE
C-   ROUTINE RETURNS ENERGY, ETA AND PHI
C-
C-   Inputs  : VEC7
C-   Outputs : VEC3
C-   Controls:
C-
C-   Created  14-FEB-1994   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE PRECISION    VEC7(*),VEC3(*)
C----------------------------------------------------------------------
      VEC3(1) = VEC7(4)  !ENERGY
      VEC3(2) = VEC7(6)  !ETA
      VEC3(3) = VEC7(7)  !PHI
  999 RETURN
      END
