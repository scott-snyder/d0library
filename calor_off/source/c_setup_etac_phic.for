      SUBROUTINE C_SETUP_ETAC_PHIC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Setup eta and phi of cluster highest cell.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   9-JUL-1992   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$LINKS:IZCACH.LINK'
      INCLUDE 'D0$INC:CHMATR_NEW.INC'
      INTEGER ISIGN,  I, K, NCH, POINTER, PACKED_WORD, IFOUND
      INTEGER IETA, IPHI, ILYR, NTWR, TWRMX, ETA(5100), PHI(5100), IER
      REAL    ECELL, EMAX, ENERGY(5100)
      LOGICAL FIRST, USE_CASH_INFO
      DATA    FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('HMATRIX_RCP')
        CALL EZGET('USE_CASH_INFO',USE_CASH_INFO,IER)
        CALL EZRSET
      END IF
C
      IF (USE_CASH_INFO) THEN
C
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
          CALL ERRMSG('CALORIMETER','CSETUP_ETA_PHI',
     &          'MAX TOWER NOT FOUND FROM CASH BANK ','W')
        ELSE
          ETAC = ETA(TWRMX)
          PHIC = PHI(TWRMX)
        END IF
C
      ELSE
C
        LCACH = LQ(LCACL-IZCACH)            ! belongs to present CACL
        NCACH = IQ(LCACH+2)                 ! Number of cells
        PCATE = IQ(LCACH+NCACH+3)           ! Pointer to CATE for maximum Cell
        NRCATE = IQ(LCATE+2)
        PCATE = LCATE + NRCATE*(PCATE-1)    ! True pointer
        ETAC = IQ(PCATE+12)
        PHIC = IQ(PCATE+13)                 ! Eta and phi of maximum energy cell
        NRCAEH = IQ(LCAEH+2)                ! repetition number
      END IF
C

      SIGN_ETA = ISIGN(1, ETAC)
  999 RETURN
      END
