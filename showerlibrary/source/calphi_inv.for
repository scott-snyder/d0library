      SUBROUTINE CALPHI_INV(PHI,IETA,IPHI,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given Pseudo rapidity of point, gives the Eta Cell
C-   number
C-
C-   Inputs  : PHI = PHI OF TRACK
C-             IETA = RAPIDITY PHYSICS INDEX OF TRACK.
C-   Outputs : IPHI = Integer Cell index
C-             IERR = non zero, PHI out of bounds
C-   Controls: 
C-
C-   Created  15-JUN-1990   Rajendran Raja
C-   Inverse of CALPHI routine
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      REAL    PHI
      INTEGER IPHI,IETA,IERR
C----------------------------------------------------------------------
      IERR = 0
      IPHI=INT(PHI*64./TWOPI)+1
      IF(IETA.GE.33.AND.MOD(IPHI,2).EQ.0)THEN
        IPHI=IPHI-1
      ENDIF
  999 RETURN
      END
