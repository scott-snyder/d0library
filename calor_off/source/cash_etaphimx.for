      SUBROUTINE CASH_ETAPHIMX(LCASH,ETAC,PHIC,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute the ETA and PHI of the Hottest Tower 
C-                         in the cluster using CASH banks
C-
C-   Inputs  : LCASH [I] : link to CASH bank
C-   
C-   Outputs : ETAC  [I] : Eta of Max tower
C-             PHIC  [I] : phi of Max tower
C-             IER   [I] : -1 if max tower not found
C-   Controls:
C-
C-   Created  17-DEC-1992   Meenakshi Narain
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LCASH, I, K, NCH, POINTER, PACKED_WORD, IFOUND
      INTEGER IETA, IPHI, ILYR, NTWR, TWRMX, ETA(5100), PHI(5100), IER
      INTEGER ETAC, PHIC
      REAL    ECELL, EMAX, ENERGY(5100)
C
      IER   = 0
      ETAC  = -99
      PHIC  = -99
      NTWR  = 0
C
      NCH   = IQ(LCASH+2)
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
            IF (NTWR.GT.5100) THEN
              IER = -1
              GOTO 999
            ENDIF
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
        CALL ERRMSG('MX ETAPHI','CASH_ETAPHIMX',
     &          'MAX TOWER NOT FOUND FROM CASH BANK ','W')
        IER = -1
      ELSE
        ETAC = ETA(TWRMX)
        PHIC = PHI(TWRMX)
      END IF

  999 RETURN
      END
