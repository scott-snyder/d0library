      SUBROUTINE CMXTWR(ETAC,PHIC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  : LCASH - link to cash bank
C-   Outputs : ETAC, PHIC - eta and phi of the central tower associated
C-                          with the electron
C-   Controls:
C-
C-   Created  26-OCT-1992   Meenakshi Narain
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INTEGER ETAC,PHIC
      INTEGER I, K, NCH, POINTER, PACKED_WORD, IFOUND
      INTEGER IETA, IPHI, ILYR, NTWR, TWRMX, ETA(5100), PHI(5100), IER
      REAL    ECELL, EMAX, ENERGY(5100)
C----------------------------------------------------------------------
      ETAC = 0
      PHIC = 0
      LCASH = LQ(LCACL-2)
      NCH   = IQ(LCASH+2)
      NTWR  = 0
      DO I  = 1, NCH
        POINTER = LCASH + 2*(I-1)
        PACKED_WORD = IQ(POINTER+3)
        ECELL = Q(POINTER+4)
        CALL CAEP_INDICES(PACKED_WORD,IETA,IPHI,ILYR)
        IF(ILYR.LE.MXLYEM) THEN
          IFOUND = 0
          DO K = 1, NTWR
            IF (IETA.EQ.ETA(K) .AND. IPHI.EQ.PHI(K)) THEN
              ENERGY(K) = ENERGY(K) + ECELL
              IFOUND = 1
            END IF
          END DO
          IF (IFOUND.EQ.0) THEN
            NTWR = NTWR + 1
            ETA(NTWR) = IETA
            PHI(NTWR) = IPHI
            ENERGY(NTWR) = ECELL
          END IF
        END IF
      END DO
      EMAX = 0.
      TWRMX = 0
      DO I = 1, NTWR
        IF (ENERGY(I).GT.EMAX) THEN
          TWRMX = I
          EMAX  = ENERGY(I)
        END IF
      END DO
      IF (TWRMX .EQ. 0) THEN
        CALL ERRMSG('CALORIMETER','CMXTWR',
     &          'MAX TOWER NOT FOUND FROM CASH BANK ','W')
      ELSE
        ETAC = ETA(TWRMX)
        PHIC = PHI(TWRMX)
      END IF
  999 RETURN
      END
