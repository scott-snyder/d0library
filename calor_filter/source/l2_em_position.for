      SUBROUTINE L2_EM_POSITION(IETA,IPHI,LYR,EM3,DET_ETA,DET_PHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : return position of the hit for track matching
C-
C-   Inputs  : IETA,IPHI,LYR  physics coordinates of peak EM3 cell
C-             EM3(-2:2,-2:2) 3x3 EM3 cells around the peak
C-   Outputs : DET_ETA detector-based eta of the hit
C-             DET_PHI detector-based phi of the hit
C-   Controls:
C-
C-   Created  29-FEB-1992   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER IETA,IPHI,LYR
      REAL EM3(-2:2,-2:2)   !indices are ETA,PHI in EM3 space
      REAL FLETA,FLPHI,DET_ETA,DET_PHI
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      REAL DETA_EM3(LYEM3A:LYEM3D),DPHI_EM3(LYEM3A:LYEM3D)
C...offsets of EM3 cells from tower eta,phi
      DATA DETA_EM3/-.25,-.25,.25,.25/,DPHI_EM3/-.25,.25,-.25,.25/
C----------------------------------------------------------------------
C
C...crude position algorithm: at center of the cell
C
C...note that this doesn't correct for the coarsening of EM3 cells at IETAC=27
      FLETA = FLOAT(IETA) - .5 + DETA_EM3(LYR)
      IF (IETA.LT.0) FLETA = FLETA + 1.0  !correct for  skip at IETAC=0
      DET_ETA = .1*FLETA
      FLPHI = FLOAT(IPHI) - .5 + DPHI_EM3(LYR)
      DET_PHI = FLPHI*TWOPI/64.0
C
C...nor is there an attempt to move to the boundary if there is  significant
C     sharing; could also improve slightly by using either wtd avg from
C     trans_cuts or by using the 2x2 determination code there
C
  999 RETURN
      END
