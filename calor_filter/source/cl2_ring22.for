      SUBROUTINE CL2_RING22(IETA,IPHI,NRING,ETALO,ETAHI,PHILO,PHIHI,
     &  NPHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the corners of a ring of readout towers
C-                         Inputs and outputs are in terms of offline towers
C-
C-   Inputs  : IETA,IPHI        [I]     Center of ring (in offline coord)
C-             NRING            [I]     number of layers of towers to add
C-                                      around center
C-   Outputs:  ETALO,ETAHI      [I]     lower, upper bounds of ring in ietac
C-             PHILO(2)         [I]     same for phi.  This is an array since
C-             PHIHI(2)                 the phi limits may wrap around.  In
C-                                      this case, NPHI = 2, and the phi loop
C-                                      is actually 2 loops.
C-             NPHI             [I]     number of phi limits = 1 or 2 normally
C-                                      any other value (eg 0) is an error
C-   Outputs :
C-   Controls:
C-
C-   Created 27-JAN-1992   James T. Linnemann   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IETA,IPHI,NRING,ETALO,ETAHI,PHILO(2),PHIHI(2),NPHI
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:CL2_RINGS.INC'
C----------------------------------------------------------------------
C
C...convert eta into continuous coordinate; enlarge ring in this space; convert
C       back to offline eta coordinate
      ETALO = JETA_L2(L2_JETA(IETA)-NRING)
      ETAHI = JETA_L2(L2_JETA(IETA)+NRING)
      NPHI = 1
      IF (NRING.GE.NPHIL/2) THEN
        PHILO(1) = 1                    ! ring covers all of phi
        PHIHI(1) = NPHIL
      ELSE
        PHILO(1) = L2_JPHI(IPHI-NRING)
        PHIHI(1) = L2_JPHI(IPHI+NRING)
        IF (PHILO(1).GT.PHIHI(1)) THEN  ! eg lo,hi = 62,2
          PHILO(2) = PHILO(1)           ! wrap around to 1,2 in (1)
          PHIHI(2) = NPHIL
          PHILO(1) = 1                  !               62,64 in (2) 
          NPHI = 2
        ENDIF
      ENDIF
  999 RETURN
      END
