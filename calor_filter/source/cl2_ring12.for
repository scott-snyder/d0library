      SUBROUTINE CL2_RING12(IETA1,IPHI1,NRING1,ETALO,ETAHI,PHILO,PHIHI,
     &  NPHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the corners of a ring of towers
C-      The ring center and size are defined in terms of level 1 towers.
C-      The returned ring corners are in terms of level 2 coordinates
C-
C-   Inputs  : IETA1,IPHI1      [I]     Center of ring (in level 1 tower coord)
C-             NRING1           [I]     number of layers of towers to add
C-                                      around center
C-   Outputs : ETALO,ETAHI      [I]     bounds of ring in IETAC (offline eta)
C-             PHILO(2)         [I]     same for PHIC.  This is an array since
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
      INTEGER ETALO1,ETAHI1,PHILO1(2),PHIHI1(2) ! intermediate outputs
                                                ! from level 1
      INTEGER I                         ! loop index for number of phi limits
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:CL2_RINGS.INC'
C----------------------------------------------------------------------
      CALL CL2_RING11(IETA1,IPHI1,NRING1,ETALO1,ETAHI1,PHILO1,PHIHI1,
     &  NPHI)
      ETALO = L1_ETA2LO(ETALO1)
      ETAHI = L1_ETA2HI(ETAHI1)
      DO I = 1,NPHI
        PHILO(I) = 2*PHILO1(I)-1
        PHIHI(I) = 2*PHIHI1(I)
      ENDDO
  999 RETURN
      END
