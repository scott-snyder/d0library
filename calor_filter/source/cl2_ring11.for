      SUBROUTINE CL2_RING11(IETA1,IPHI1,NRING1,ETALO,ETAHI,PHILO,PHIHI,
     &  NPHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the corners of a ring of level 1 towers
C-                         Inputs and outputs are in terms of level 1 towers
C-
C-   Inputs  : IETA1,IPHI1      [I]     Center of ring (in level 1 tower coord)
C-             NRING1           [I]     number of layers of towers to add
C-                                      around center
C-   Outputs:  ETALO,ETAHI      [I]     lower, upper bounds of ring in L1 eta
C-             PHILO(2)         [I]     same for phi.  This is an array since
C-             PHIHI(2)                 the phi limits may wrap around.  In
C-                                      this case, NPHI = 2, and the phi loop
C-                                      is actually 2 loops.
C-             NPHI             [I]     number of phi limits = 1 or 2 normally
C-                                      any other value (eg 0) is an error
C-   Outputs :
C-   Controls:
C-
C-   Created  14-JUN-1991   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IETA1,IPHI1,NRING1,ETALO,ETAHI,PHILO(2),PHIHI(2),NPHI
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:CL2_RINGS.INC'
C----------------------------------------------------------------------
C
C...convert eta into continuous coordinate; enlarge ring in this space; convert
C       back to l1 eta coordinate
      ETALO = JETA_L1(L1_JETA(IETA1)-NRING1)
      ETAHI = JETA_L1(L1_JETA(IETA1)+NRING1)
      NPHI = 1
      IF (NRING1.GE.NPHIL1/2) THEN
        PHILO(1) = 1                    ! ring covers all of phi
        PHIHI(1) = NPHIL1
      ELSE
        PHILO(1) = L1_JPHI(IPHI1-NRING1)
        PHIHI(1) = L1_JPHI(IPHI1+NRING1)
        IF (PHILO(1).GT.PHIHI(1)) THEN  ! eg lo,hi = 28,2
          PHILO(2) = PHILO(1)           ! wrap around to 1,2 in (1)
          PHIHI(2) = NPHIL1
          PHILO(1) = 1                  !               28,32 in (2) 
          NPHI = 2
        ENDIF
      ENDIF
  999 RETURN
      END
