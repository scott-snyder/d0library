      SUBROUTINE JET_OUT_OF_CONE_CORR(P)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Correction on top of Bob Gehoe's 0,3 cone 
C-             correction in CAFIX(both MC and data). This correction is 
C-             due to the out of cone correction due to radiation(not due 
C-             to the showering in calorimeter). 
C-             This correction is calculated from MC to get the parton 
C-             energy and the amount of correction needed for the real data
C-             appeared to be about the same.
C-             Since it's out of cone correction, the missing Et SHOULD NOT BE
C-             CORRECTED as this correction is made.
C-
C-   Inputs  : 
C-          P(1:4) : Px, Py, Pz, E of the jet
C-   Outputs : 
C-          P(1:4) : Px, Py, Pz, E of the jet
C-   Controls: 
C-
C-   Created  13-JUL-1994   M. Pang
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I
      REAL P(4),FACTOR,PAR(2),E_PARTON
      DATA PAR/-5.1131,0.92032/
C----------------------------------------------------------------------
	
      E_PARTON = (P(4) - PAR(1))/PAR(2)
      FACTOR = E_PARTON / P(4)

      DO I = 1,4
        P(I) = FACTOR * P(I)
      END DO
      

  999 RETURN
      END
