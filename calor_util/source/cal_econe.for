      SUBROUTINE CAL_ECONE(ETA, PHI, DR, EEM, ETOT, ET, IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Finds the calorimeter energy contained
C-                          contained in a cone with a given angle and
C-                          orientation. Could be used for isolation cuts.
C-
C-   Inputs  :   ETA   Eta of the cone axis (pseudo-rapidity)
C-               PHI   Phi of the cone axis (rad.)
C-               DR    Radious of the cone.
C-
C-   Outputs :   EEM     Energy in EM Cal.
C-               ETOT    Total energy in Cal. (EM + HAD)
C-               ET      Transverse total energy in cone.
C-   Controls:   IER     0=OK, 1=Cate bank does not exist.
C-
C-   Created  28-OCT-1990   SHAHRIAR ABACHI
C-   UPDATED  12-DEC-1990   SHAHRIAR ABACHI   ET added, bug fixed.
C-   UPDATED  01-JAN-1991   SHAHRIAR ABACHI   Call to CPTCTF added
C-   UPDATED  02-JUL-1992   SHAHRIAR ABACHI   IPHI=0 avoided
C-   UPDATED  06-AUG-1993   SHAHRIAR ABACHI   ET calculation modified
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL ETA, PHI, DR, EEM, ETOT, ET
      INTEGER IER
C
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PTCATE.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER IETA1, IPHI1,IETA2, IPHI2, I, IDUM
      INTEGER IPTTL, IPTEM, NCATE, NREP, IPH, JPH, IET
      REAL TPHI, TETA, TTHE, THETA
      INTEGER LCATE, GZCATE
      REAL E(4), TDR, SAFETY, DPH, EPS, LRG
      DATA SAFETY, EPS, LRG / 2.0, 1.E-9, 1.E8 /
C
      IER = 0
      EEM = 0.0
      ETOT = 0.0
      ET = 0.0
C
      LCATE = GZCATE()
      IF(LCATE .EQ. 0) THEN
        IER = 1
        GOTO 999
      ENDIF
C
      CALL CPTCTF
C
      IPHI1 = (PHI - DR) * NPHIL / TWOPI - SAFETY
      IDUM = IPHI1
      IF(IPHI1 .LE. 0) IPHI1 = IPHI1 + NPHIL
      IPHI2 = (PHI + DR) * NPHIL / TWOPI + SAFETY
      IF(IPHI2 .LE. 0 .OR. IDUM .LE. 0) IPHI2 = IPHI2 + NPHIL
      IETA1 = (ETA - DR) * 10. - SAFETY
      IETA2 = (ETA + DR) * 10. + SAFETY
      IF(IETA1 .LT. - NETAL) IETA1 = - NETAL
      IF(IETA2 .GT. NETAL) IETA2 = NETAL
C
      NREP = IQ(LCATE + 2)
C
      DO IET = IETA1, IETA2
        DO 10 JPH = IPHI1, IPHI2
          IPH = JPH
          IF(IPH .NE. NPHIL) IPH = MOD(IPH, NPHIL)
          IPTTL = PTCATE(IET, IPH, 2)
          IF(IPTTL .EQ. 0) GOTO 10
          IPTTL = NREP * (IPTTL - 1) + LCATE
          IPTEM = PTCATE(IET, IPH, 1)
          IF(IPTEM .NE. 0) IPTEM = NREP * (IPTEM - 1) + LCATE
          DO  I = 1,4
            E(I) = Q(IPTTL + 3 + I)
            IF(ABS(E(I)) .LT. EPS) E(I) = 0.0
            IF(ABS(E(I)) .GT. LRG) GOTO 10
          ENDDO
          CALL ETOETA(E, TPHI, TTHE, TETA)
          DPH = PHI - TPHI
          IF(ABS(DPH) .GT. PI) THEN
            IF(DPH .LT. 0.0) THEN
              TPHI = TPHI + TWOPI - 2.0 * ABS(DPH)
            ELSE
              TPHI = TPHI - TWOPI + 2.0 * ABS(DPH)
            ENDIF
          ENDIF
          TDR = SQRT( (PHI - TPHI)**2 + (ETA - TETA)**2 )
          IF(TDR .GT. DR) GOTO 10
          ETOT = ETOT + Q(IPTTL + 7)
          ET = ET + Q(IPTTL + 8)
          IF(IPTEM .GT. 0) EEM = EEM + Q(IPTEM + 7)
   10   CONTINUE
      ENDDO
C
      THETA = 2.* ATAN(EXP(-ETA))
C
C----------------------------------------------------------------------
C
  999 RETURN
      END
