      REAL FUNCTION DCOS_FROM_ETA_PHI(DET_ETA1,PHI1,DET_ETA2,PHI2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute the Cosine of angles between two vectors
C-
C-   Inputs  : DET_ETA1 and DET_ETA2 = detector ETA of the two objects
C-             PHI1 and PHI2         
C-   Outputs : DCOS_FROM_ETA_PHI
C-   Controls: 
C-
C-   Created  26-JUN-1992   Meenakshi Narain
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    DET_ETA1, DET_ETA2, PHI1, PHI2
      REAL    THETA1, THETA2
      REAL    UX, UY, UZ, THETA, PHI, THETA_FROM_ETA, RETA 
C----------------------------------------------------------------------
C ****  Statement functions
      UX(THETA,PHI) = SIN(THETA)*COS(PHI)
      UY(THETA,PHI) = SIN(THETA)*SIN(PHI)
      UZ(THETA) = COS(THETA)
      THETA_FROM_ETA(RETA ) = 2*ATAN(EXP(-(RETA)))
C----------------------------------------------------------------------
      THETA1 = THETA_FROM_ETA(DET_ETA1)
      THETA2 = THETA_FROM_ETA(DET_ETA2)
      DCOS_FROM_ETA_PHI  = UX(THETA1,PHI1)*UX(THETA2,PHI2) +
     &                     UY(THETA1,PHI1)*UY(THETA2,PHI2) +
     &                     UZ(THETA1)*UZ(THETA2)
  999 RETURN
      END
