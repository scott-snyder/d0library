      SUBROUTINE L2_EM_TRACK_ROADS(PAR_SET,SUMEM,SIG_ETA,SIG_PHI,
     &      XYZ_CLUS,DETA,DPHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Give the eta, phi road width
C-
C-   Inputs  : PAR_SET parameter set number
C-             SUMEM  E of the candidate
C-             SIG_ETA  estimated resolutin in the eta direction
C-             SIG_PHI  estimated resolutin in the phi direction
C-             XYZ_CLUS(3) position of the cluster
C-   Outputs : DETA, DPHI output road widths
C-   Controls: 
C-
C-   Created   4-SEP-1992   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PAR_SET
      REAL SUMEM,SIG_ETA,SIG_PHI,XYZ_CLUS(3),DETA,DPHI
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:L2_EM.PARAMS'
      INCLUDE 'D0$INC:L2_EM_CUTS.INC'
C----------------------------------------------------------------------
C
C...we will take the nominal roads as starting values, increasing as needed
      DETA = DETA_TR(PAR_SET)
      DETA = MAX(DETA,3.0*SIG_ETA)
      DPHI = DPHI_TR(PAR_SET)
      DPHI = MAX(DPHI,3.0*SIG_PHI)
  999 RETURN
      END
