      SUBROUTINE CL2_PTCAEP2_DUMP(LUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : dump the value of PTCAEP2(1,phi,eta) to check
C-   zeroing
C-
C-   Inputs  : unit number
C-   Outputs : dump of PTCAEP2
C-   Controls: 
C-
C-   Created  10-NOV-1990   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LUN
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$CALOR_FILTER$SOURCE:PTCAEP2.DEF' ! pointer array
      INTEGER IETA,IPHI                 ! loop indices
C----------------------------------------------------------------------
      WRITE(LUN,100)
  100 FORMAT(1H1)
      WRITE(LUN,150)'PTCAEP2 DUMP'
  150 FORMAT(1X,A)
      WRITE(LUN,200)0,(IPHI,IPHI=1,NPHIL)
  200 FORMAT(I4,1X,64I2)
      DO IETA = -NETAL,NETAL
        WRITE(LUN,200)IETA,(PTR2(1,IPHI,IETA),IPHI=1,NPHIL)
      END DO
  999 RETURN
      END
