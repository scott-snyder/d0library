      SUBROUTINE CAL_ECONE_CLU(ETA, PHI, DR, NCLU, ECLU, ETCLU)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Calculates the number of clusters and energies in a
C-                          given cone. To be used for isolation calculation. 
C-
C-   Inputs  :    ETA, PHI   Center of the cone
C-                DR         Cone size
C-
C-   Outputs :    NCLU           Number of clusters found within the cone
C-                ECLU, ECLU     energy and Et of all clusters found
C-   Controls: 
C-
C-   Created   07-JAN-1991   SHAHRIAR ABACHI
C-   Modified  23-JAN-1992   SHAHRIAR ABACHI   DPHi modified
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL  ETA, PHI, DR, ECLU, ETCLU
      INTEGER  NCLU
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER GZCACL, LCACL
      REAL ETAC, PHIC, DETA, DPHI, DRC
C
      NCLU = 0
      ECLU = 0.0
      ETCLU = 0.0
C
      LCACL = GZCACL()
C
  100 IF(LCACL .LE. 0) THEN
        GOTO 999
      ENDIF
C
      ETAC = Q(LCACL + 13)
      PHIC = Q(LCACL + 12)
C
      DETA = ABS(ETAC - ETA)
      DPHI = ABS(PHIC - PHI)
      IF(DPHI .GT. PI) DPHI = TWOPI - DPHI
      DRC = SQRT(DETA**2 + DPHI**2)
      IF(DRC .LE. DR) THEN
        NCLU = NCLU + 1
        ECLU = ECLU + Q(LCACL + 7)
        ETCLU = ETCLU + Q(LCACL + 8)
      ENDIF
      LCACL = LQ(LCACL)
      GOTO 100
C
C----------------------------------------------------------------------
C
  999 RETURN
      END
