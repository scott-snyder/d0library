      FUNCTION PETA_TO_DETA( PHYSICS_ETA, Z_VERTEX )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert Physics eta to Detector eta
C-
C-   Returned value  : Detector eta (Eta obtained by assuming z=0.0)
C-   Inputs  :  PHYSICS_ETA [R] : Physics eta 
C-           :  Z_VERTEX    [R] : Z position of vertex
C-   Outputs :
C-   Controls: Uses DET_ETA
C-
C-   Created  15-SEP-1993   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL PHYSICS_ETA, PETA_TO_DETA, Z_VERTEX
      REAL THETA, ETA
C----------------------------------------------------------------------
      THETA = 2*ATAN( EXP( -PHYSICS_ETA ) )
      CALL DET_ETA( Z_VERTEX, THETA, ETA )
      PETA_TO_DETA = ETA
      RETURN
      END
