      REAL FUNCTION phi_difference ( phi1, phi2 )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Determine the angle in phi between PHI1 and PHI2.
C-   The returned value will be in the range 0. --> pi.
C-
C-   Returned value  : the angle in phi between PHI1 and PHI2.
C-   Inputs  : PHI1, PHI2 [R] - the positions between which we determine the
C-                              angle
C-   Outputs : none
C-   Controls: none
C-
C-   Created  30-MAR-1992   Marc Paterno
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C ****  Global variables
C
      INCLUDE  'D0$INC:PI.DEF'
      REAL  phi1, phi2
C----------------------------------------------------------------------
C
C ****  Local variables
C
      REAL phidiff
C----------------------------------------------------------------------
      phidiff = abs(phi1 - phi2)
      IF (phidiff .LE. sngl(pi)) THEN
        phi_difference = phidiff
      ELSE
        phi_difference = sngl(twopi) - phidiff
      ENDIF

      RETURN
      END
