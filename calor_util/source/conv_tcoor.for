      SUBROUTINE CONV_TCOOR(IPHI,IETA,PHI,ETA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : convert trigger tower indices to eta, phi
C-
C-   Inputs  : TPHI,TETA - tower indices
C-   Outputs : PHI,ETA   - phi, eta
C-
C-   Created   7-DEC-1992   Ulrich Heintz
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER IPHI,IETA
      REAL    PHI,ETA,TETA,TPHI
      TETA=FLOAT(IETA)
      ETA=TETA/ABS(TETA)*(ABS(TETA)/5.-0.1)
      TPHI=FLOAT(IPHI)
      PHI=(TPHI-0.5)*PI/16.      
C----------------------------------------------------------------------
  999 RETURN
      END
