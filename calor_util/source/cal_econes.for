
      SUBROUTINE CAL_ECONES(ETA, PHI, EEM, ETOT, IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Finds the calorimeter energy contained
C-                          in twenty DR**2 bins around a given direction
C-
C-   Inputs  :   ETA   Eta of the cone axis (pseudo-rapidity)
C-               PHI   Phi of the cone axis (rad.)
C-
C-   Outputs :   EEM(20)     Energy in EM Cal.
C-               ETOT(20)    Total energy in Cal. (EM + HAD)
C-   Controls:   IER     0=OK, 1=Cate bank does not exist.
C-
C-   Created   4-DEC-1991   Daria Zieminska
C-   Based on CAL_ECONE Created  28-OCT-1990   SHAHRIAR ABACHI
C-   Modified  02-JUL-1992   SHAHRIAR ABACHI    IPHI=0 avoided
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL ETA, PHI, DRSQ, DR, EEM(20), ETOT(20)
      INTEGER IER
C
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PTCATE.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER IETA1, IPHI1,IETA2, IPHI2, I, J, IDUM
      INTEGER IPTTL, IPTEM, NCATE, NREP, IPH, JPH, IET
      REAL TPHI, TETA, TTHE, THETA, DIFFCHECK
      INTEGER LCATE, GZCATE
      REAL E(4), TDR, DPH, EPS, LRG
      DATA EPS, LRG /  1.E-9, 1.E8 /
      DATA DRSQ/10./
C
      IER = 0
      CALL VZERO(EEM,20)
      CALL VZERO(ETOT,20)
C
      LCATE = GZCATE()
      IF(LCATE .EQ. 0) THEN
        IER = 1
        GOTO 999
      ENDIF
C
      CALL CPTCTF
C
      DR=SQRT(DRSQ)
      IPHI1 =INT((PHI - DR) * NPHIL / TWOPI)
      IDUM = IPHI1
      IF(IPHI1 .LE. 0) IPHI1 = IPHI1 + NPHIL
      IPHI2 =INT((PHI + DR) * NPHIL / TWOPI)
      IF(IPHI2 .LE. 0 .OR. IDUM .LE. 0) IPHI2 = IPHI2 + NPHIL
      IETA1 =INT((ETA - DR) * 10.)
      IETA2 =INT((ETA + DR) * 10.)
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
          DO  J = 1,4
            E(J) = Q(IPTTL + 3 + J)
            IF(ABS(E(J)) .LT. EPS) E(J) = 0.0
            IF(ABS(E(J)) .GT. LRG) GOTO 10
          ENDDO
          CALL ETOETA(E, TPHI, TTHE, TETA)
          DPH = PHI - TPHI
          IF(ABS(DPH) .GT. PI) DPH=TWOPI-ABS(DPH)
          TDR =  DPH**2 + (ETA - TETA)**2
C
          DO I=1,20
            IF (TDR.GT.FLOAT(I-1)*0.5.AND.
     +            TDR.LE.FLOAT(I)*0.5) THEN
              ETOT(I)=ETOT(I)+Q(IPTTL+7)
              IF (IPTEM.GT.0) EEM(I)=EEM(I)+Q(IPTEM+7)
            ENDIF
          ENDDO
C
   10   CONTINUE
      ENDDO
C
C
  999 RETURN
      END
