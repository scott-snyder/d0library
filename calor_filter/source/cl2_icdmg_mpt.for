        SUBROUTINE CL2_ICDMG_MPT(ZVTX,PTX,PTY,SUMET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate contributions to scalar and vector Et from
C-   the Massless Gaps and ICD's alone.  For use in level 2; does it
C-   approximately and fast
C-
C-   Inputs  : ZVTX             Z of vertex used
C-   Outputs : PTX,PTY  contributions of ICD, MG to these components of
C-                              SUMMED vector PT
C-             SUMET    contribution to summed scalar PT
C-   Controls: None
C-
C-   Created   8-NOV-1990   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    ZVTX,PTX,PTY,SUMET        ! arguments
C----------------------------------------------------------------------
      PTX = 0
      PTY = 0
      SUMET = 0
  999 RETURN
      END
