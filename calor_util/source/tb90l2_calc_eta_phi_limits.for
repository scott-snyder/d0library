      SUBROUTINE TB90L2_CALC_ETA_PHI_LIMITS(eta_indx,phi_indx,etas,phis)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : given eta and phi indices, determines the indices
C-   for a 3x3 cluster about the eta,phi index  accounting for the lack of a 0
C-   index
C-
C-   Inputs  : eta,phi index of central towers
C-   Outputs : eta,phi limits of corners of 3x3 cluster
C-   Controls: none
C-
C-   Created   8-AUG-1991   James Richardson
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER eta_indx,phi_indx,etas(2),phis(2)
C----------------------------------------------------------------------
      IF ( eta_indx .EQ. -1 ) THEN
        etas(1) = -2
        etas(2) = 1
      ELSEIF ( eta_indx .EQ. 1 ) THEN
        etas(1) = -1
        etas(2) = 2
      ELSE
        etas(1) = eta_indx - 1
        etas(2) = eta_indx + 1
      ENDIF
      phis(1) = phi_indx - 1
      phis(2) = phi_indx + 1
      RETURN
      END
