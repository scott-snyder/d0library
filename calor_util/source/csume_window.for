      SUBROUTINE CSUME_WINDOW(IPHI,IETA,N,SUME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sum energy for a window of (2N-1)x(2N-1) towers 
C-                         around the (iphi,ieta)
C-
C-   Inputs  : IPHI, IETA: Phi and eta for a cell
C-             N: size for the tower window 
C-   Outputs : SUME: total energy in this window
C-
C-   Created  16-NOV-1993   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PTCATE.INC'
      INTEGER  IPHI, IETA, N, NREP, POINT, NSIZE
      INTEGER  LCATE, GZCATE
      INTEGER  IETALO, IETAHI, IPHILO, IPHIHI, I, J, JPHI
      REAL     SUME
C----------------------------------------------------------------------
C
      NSIZE = N - 1
      SUME = 0.0
      LCATE = GZCATE()
      NREP = IQ(LCATE+2)
      IETALO = IETA - NSIZE
      IETAHI = IETA + NSIZE
      IPHILO = IPHI - NSIZE
      IPHIHI = IPHI + NSIZE
      DO 100 I = IETALO , IETAHI
        DO 200 J = IPHILO , IPHIHI 
          JPHI = J
          IF (J.GE.65) JPHI = MOD(J,64)
          IF (J.LE.0)  JPHI = 64 + J
          POINT = PTCATE(I,JPHI,2)
          IF (POINT.EQ.0) GOTO 200
          POINT = NREP*(POINT-1) + LCATE + 3
          SUME = SUME + Q(POINT+4)
  200   CONTINUE
  100 CONTINUE
C
  999 RETURN
      END
