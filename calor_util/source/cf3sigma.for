      SUBROUTINE CF3SIGMA(SIGMA5,SIGMA3)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  : NONE
C-   Outputs : SIGMA3,SIGMA5 <r> in 3x3 and 5x5 EM3 cells around peak
C-   Controls:
C-
C-   Created  24-OCT-1992   Meenakshi Narain
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      REAL    SIGMA5,SIGMA3
      REAL EM3(-2:2,-2:2)
      REAL ESH3,ESH5
      REAL AVIETA,AVIPHI
      REAL    ECELL, EMAX, ENERGY(5100)
      INTEGER I,J
      INTEGER NCELLS, POINTER, PACKED_WORD
      INTEGER IETA, IPHI, ILYR, NTWR, TWRMX
      INTEGER  ETA(5100), PHI(5100), LYR(5100)
      INTEGER ETAMX, PHIMX, LYRMX, DETA, DPHI
      INTEGER PHIEM3(4,4),ETAEM3(4,4),XPHI,XETA
      DATA PHIEM3/0,1,0,1,-1,0,-1,0,0,1,0,1,-1,0,-1,0/
      DATA ETAEM3/0,0,1,1,0,0,1,1,-1,-1,0,0,-1,-1,0,0/
C
C----------------------------------------------------------------------
      SIGMA5 = 0.
      SIGMA3 = 0.
      NTWR = 0
      NCELLS = IQ(LCASH+2)
C
C ****  LOOP OVER ALL CELLS TO FIND MAX EM3 TOWER
C
      DO I = 1,NCELLS
        POINTER = LCASH + 2*(I-1)
        PACKED_WORD = IQ(POINTER+3)
        ECELL = Q(POINTER+4)
        CALL CAEP_INDICES(PACKED_WORD,IETA,IPHI,ILYR)
        IF(ILYR.GE.LYEM3A .AND. ILYR.LE.LYEM3D) THEN
          NTWR = NTWR + 1
          ETA(NTWR) = IETA
          PHI(NTWR) = IPHI
          LYR(NTWR) = ILYR
          ENERGY(NTWR) = ECELL
        END IF
      END DO
      IF (NTWR.EQ.0) GOTO 999
      EMAX = 0.
      TWRMX = 0
      DO I = 1, NTWR
        IF (ENERGY(I).GT.EMAX) THEN
          TWRMX = I
          EMAX  = ENERGY(I)
        END IF
      END DO
      ETAMX = ETA(TWRMX)
      PHIMX = PHI(TWRMX)
      LYRMX = LYR(TWRMX)
      IF (TWRMX.EQ.0) GOTO 999
C
C ****  AGAIN LOOP TO FILL EEM3 ARRAY
C
      ESH5 = 0.
      ESH3 = 0.
      CALL UZERO(EM3,1,25)

      DO I = 1,NTWR
        DPHI = PHI(I)-PHIMX ! TAKE CARE OF WRAP
        IF (DPHI.EQ.63) THEN
          DPHI = -1
        ELSE IF (DPHI.EQ.-63) THEN
          DPHI = 1
        ENDIF
        DETA = ETA(I)-ETAMX
        IF (ETAMX.EQ.1.AND.ETA(I).EQ.-1) THEN
          DETA = -1
        ELSEIF (ETAMX.EQ.-1.AND.ETA(I).EQ.1) THEN
          DETA = 1
        ENDIF
        IF (IABS(DETA).LE.1.AND.IABS(DPHI).LE.1) THEN
          XPHI = 2 * DPHI + PHIEM3(LYR(I)-2,LYRMX-2)
          XETA = 2 * DETA + ETAEM3(LYR(I)-2,LYRMX-2)
          IF (IABS(XETA).LE.2.AND.IABS(XPHI).LE.2) THEN
            EM3(XETA,XPHI) = ENERGY(I)
            ESH5 = ESH5 + ENERGY(I)
            IF (IABS(XETA).LE.1.AND.IABS(XPHI).LE.1) THEN
              ESH3 = ESH3 + ENERGY(I)
            ENDIF
          ENDIF
        END IF
      END DO
C
C ****  NOW COMPUTE SIGMA5/SIGMA3 (LEVEL2)
C
C...calculate <r> in 3x3 em3 around peak
      SIGMA3 = 0
      AVIETA = 0.
      AVIPHI = 0.
      IF (ESH3.EQ.0) THEN
        CALL ERRMSG('CF3SIGMA','CF3SIGMA',
     &        'ESH3 is zero... skipping SIGMA3','W')
        GOTO 900
      ENDIF
      DO I = -1,1
        DO J = -1,1
          AVIETA = FLOAT(I)*EM3(I,J) + AVIETA
          AVIPHI = FLOAT(J)*EM3(I,J) + AVIPHI
        ENDDO
      ENDDO
      AVIETA = AVIETA/ESH3
      AVIPHI = AVIPHI/ESH3
      DO I = -1,1
        DO J = -1,1
          SIGMA3 = SIGMA3 + SQRT((FLOAT(J)-AVIPHI)**2 +
     &        (FLOAT(I)-AVIETA)**2)*EM3(I,J)/ESH3
        ENDDO
      ENDDO
C
C    Radius of showers in 5x5 em3
C
  900 CONTINUE
      AVIETA = 0.
      AVIPHI = 0.
      SIGMA5 = 0
      IF (ESH5.EQ.0) THEN
        CALL ERRMSG('CF3SIGMA','CF3SIGMA',
     &        'ESH5 is zero... skipping SIGMA5','W')
        GOTO 999
      ENDIF
      DO I = -2,2
        DO J = -2,2
          AVIETA = FLOAT(I)*EM3(I,J)/ESH5 + AVIETA
          AVIPHI = FLOAT(J)*EM3(I,J)/ESH5 + AVIPHI
        ENDDO
      ENDDO
      DO I = -2,2
        DO J = -2,2
          SIGMA5 = SIGMA5 + SQRT((FLOAT(J)-AVIPHI)**2 +
     &        (FLOAT(I)-AVIETA)**2)*EM3(I,J)/ESH5
        ENDDO
      ENDDO

  999 RETURN
      END
