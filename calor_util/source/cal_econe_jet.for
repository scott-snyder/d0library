      SUBROUTINE CAL_ECONE_JET(ETA, PHI, DR, NJETS, EJETS, ETJETS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Calculates the number of jets and energies in a
C-                          given cone. To be used for isolation calculation. 
C-
C-   Inputs  :    ETA, PHI   Center of the cone
C-                DR         Cone size
C-
C-   Outputs :    NJETS             Number of jets found within the cone
C-                EJETS, ETJETS     energy and Et of all jets found
C-   Controls: 
C-
C-   Created   28-DEC-1990   SHAHRIAR ABACHI
C-   Modified  23-JAN-1992   SHAHRIAR ABACHI  DPHI modified
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL  ETA, PHI, DR, EJETS, ETJETS
      INTEGER  NJETS
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER GZJETS, LJETS
      REAL ETAJ, PHIJ, DETA, DPHI, DRJ
C
      NJETS = 0
      EJETS = 0.0
      ETJETS = 0.0
C
      LJETS = GZJETS()
C
  100 IF(LJETS .LE. 0) THEN
        GOTO 999
      ENDIF
C
      ETAJ = Q(LJETS + 9)
      PHIJ = Q(LJETS + 8)
C
      DETA = ABS(ETAJ - ETA)
      DPHI = ABS(PHIJ - PHI)
      IF(DPHI .GT. PI) DPHI = TWOPI - DPHI
      DRJ = SQRT(DETA**2 + DPHI**2)
      IF(DRJ .LE. DR) THEN
        NJETS = NJETS + 1
        EJETS = EJETS + Q(LJETS + 5)
        ETJETS = ETJETS + Q(LJETS + 6)
      ENDIF
      LJETS = LQ(LJETS)
      GOTO 100
C
C----------------------------------------------------------------------
C
  999 RETURN
      END
