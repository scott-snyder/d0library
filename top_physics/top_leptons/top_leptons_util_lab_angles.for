      SUBROUTINE TOP_LEPTONS_UTIL_LAB_ANGLES(VECT,THETA,PHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : For a given 4-vector, VECT returns the
C-                         lab theta and phi angles
C-
C-   Inputs  : 
C-              VETC(4) - particle 4-vector
C-   Outputs : 
C-              THETA - lab theta angle
C-              PHI   - lab phi angle
C-   Controls: 
C-              None
C-
C-   Created  22-JUL-1993   Stephen J. Wimpenny
C-   Modified 10-Sep-1993   Error in Phi calculation for large Phi fixed
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      REAL VECT(4),THETA,PHI
C
      THETA=0.0
      IF(VECT(4).GT.0.0) THETA=ACOS( VECT(3)/VECT(4) )
C
      PHI=0.0
      IF(VECT(1)*VECT(2).NE.0.0) PHI=ATAN2(VECT(2), VECT(1))
C
C----------------------------------------------------------------------
  999 RETURN
      END
