      SUBROUTINE PHYS_COR(TGPHI,TGETA,PHI,ETA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : converts from trigger tower coordinate
C-                         to physics coordinates
C-                         take the average of tower centers in TT
C-
C-   Inputs  : trigger tower eta and phi
C-   Outputs : detector eta and phi
C-   Controls: none
C-
C-   Created  15-MAY-1992   Amber S. Boehnlein
C-   Updated   2-JUL-1992   James T. Linnemann  center in trigger tower
C    Updated  18-JUL-1994   Amber S. Boehnlein, only use odd phi at high eta
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$INC:CL2_RINGS.INC'
      INTEGER TGETA,TGPHI,IETA,IPHI,ETAHI,ETALO,PHIHI(2),PHILO(2),NPHI  
      INTEGER NTOWERS,IER
      REAL    PHI_DET,ETA_DET,DEL_PHI,DEL_ETA,PHI,ETA
C----------------------------------------------------------------------
      CALL CL2_RING12(TGETA,TGPHI,0,ETALO,ETAHI,PHILO,PHIHI,NPHI)
      PHI = 0
      ETA = 0
      NTOWERS = 0
      IF(ABS(ETALO).GE.33) THEN
        IF(MOD(PHILO(1),2).EQ.1) THEN
          PHIHI(1) = PHILO(1)
        ELSE
          PHILO(1) = PHIHI(1)
        ENDIF
      ENDIF
      DO IETA = ETALO,ETAHI
        DO IPHI = PHILO(1),PHIHI(1)  !no wraparound in a single TTower
          NTOWERS = NTOWERS + 1
          CALL CALETA(IETA,ETA_DET,DEL_ETA,IER) ! convert to physics eta
          ETA= ETA + ETA_DET
          CALL CALPHI(IPHI,IETA,PHI_DET,DEL_PHI,IER) !and phi
          PHI = PHI + PHI_DET
        ENDDO
      ENDDO
      PHI = PHI/NTOWERS
      ETA = ETA/NTOWERS
  999 RETURN
      END
