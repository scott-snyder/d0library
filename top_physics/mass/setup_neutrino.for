      SUBROUTINE SETUP_NEUTRINO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SET UP THE NEUTRINO 2 VECTORS, WITH THE 1ST 
C-   NEUTRINO LAMBDA EXP(I*PHI) AROUND THE TOTAL NEUTRINO VECTOR.
C-
C-   Inputs  : LAMBDA = Magnitude of 1st neutrino ET
C-             PHI = Azimuth angle of 1st neutrino in degrees
C-   Outputs : PNUT1,PNUT2
C-   Controls: 
C-
C-   Created  23-JAN-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:KINEQ.INC'
C----------------------------------------------------------------------
      PNUT1(1) = LAMBDA*COS(PHI*RADIAN)
      PNUT1(2) = LAMBDA*SIN(PHI*RADIAN)
C      
      PNUT2(1) = PNUT(1) - PNUT1(1)
      PNUT2(2) = PNUT(2) - PNUT1(2)
C
  999 RETURN
      END
