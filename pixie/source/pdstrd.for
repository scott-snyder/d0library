      SUBROUTINE PDSTRD(PHI1,PHI2,PHI3,PHI4)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : set a road with phi region for R-Z view display
C-
C-   Inputs  : PHI1, PHI2: phi region [phi1,phi2] for upper part R-Z display
C-             PHI3, PHI4: phi region [phi3,phi4] for lower part R-Z display
C-   ENTRY PDGTRD(CDC,phi1,phi2,phi3,phi4): to get the road phi regions.
C-
C-   Created  10-MAY-1990   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL PHI1, PHI2, PHI3, PHI4
      REAL RDPHI1, RDPHI2, RDPHI3, RDPHI4
      REAL RPHI1, RPHI2, RPHI3, RPHI4
      LOGICAL CDCON, CDC
      DATA CDCON/.FALSE./
C----------------------------------------------------------------------
C
      CDCON = .TRUE.
      RPHI1 = PHI1
      RPHI2 = PHI2
      RPHI3 = PHI3
      RPHI4 = PHI4
      RETURN
C
      ENTRY PDGTRD(CDC,RDPHI1,RDPHI2,RDPHI3,RDPHI4)
      CDC = CDCON
      RDPHI1 = RPHI1
      RDPHI2 = RPHI2
      RDPHI3 = RPHI3
      RDPHI4 = RPHI4
      RETURN
C
      END
