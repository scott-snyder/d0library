      SUBROUTINE PUGET_MAXET(ZMAX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get max. et/pt from PMUO, PELC, PPHO, PTAU
C-                         and PNUT Banks
C-
C-   Outputs : ZMAX - Maximum Et or Pt of non-jets objects
C-
C-   Created   4-FEB-1994   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INTEGER GZPMUO,GZPELC,GZPPHO,GZPNUT
      INTEGER LPMUO,LPELC,LPTAU,LPPHO,LPNUT
      INTEGER NMUO,NELE,NTAU,NPHO,NUM_TAU
      INTEGER IPASS
      INTEGER IT,IER
      REAL    ETAU(4),ETTAU,THETAT,ETAT,PHIT,RMS_WIDTH
      REAL    ZMAX
      REAL    E,ET,PHI,ETA,P,PT
C----------------------------------------------------------------------
      ZMAX = 0.
C-
C====== Process PMUO ================================================
C-
      NMUO = 0
      LPMUO = GZPMUO(0)
      IF (LPMUO.EQ.0) THEN
        GO TO 600
      ENDIF
C-
C--- Start PMUO loop...
C-
   20 IF (LPMUO .LE. 0)     GO TO 600
      NMUO = NMUO + 1
      P    = ABS(Q(LPMUO+13))
      PT   = Q(LPMUO+14)
      ETA  = Q(LPMUO+16)
      PHI  = Q(LPMUO+17)
      IF (PT .GT. ZMAX)   ZMAX = PT
C--- GO TO THE NEXT PMUO BANK
C-
   40 LPMUO = LQ(LPMUO)
      GO TO 20
C-
C====== Process PELC ================================================
C-
  600 NELE = 0
C-
      LPELC = GZPELC()
      IF (LPELC .LE. 0) THEN
        GO TO 700
      ENDIF
C-
C--- Start PELC loop...
C-
   50 IF (LPELC .LE. 0)     GO TO 700
      NELE = NELE + 1
      E    = Q(LPELC+6)
      ET   = Q(LPELC+7)
      ETA  = Q(LPELC+9)
      PHI  = Q(LPELC+10)
      IF (ET .GT. ZMAX)   ZMAX = ET
C--- GO TO THE NEXT PELC BANK
C-
   55 LPELC = LQ(LPELC)
      GO TO 50
C-
C====== Process PTAU ================================================
C-
  700 NTAU = 0
C-
      IER = 0
      CALL GTPTAU_TOTAL(NUM_TAU,IER)
      IF (IER .EQ. 0) THEN
C-
C--- Start PTAU loop...
C-
        DO IT = 1,NUM_TAU
          CALL GTPTAU(IT,ETAU,ETTAU,THETAT,ETAT,PHIT,RMS_WIDTH,IER)
          NTAU = NTAU + 1
          E    = ETAU(4)
          ET   = ETTAU
          ETA  = ETAT
          PHI  = PHIT
          IF (ET .GT. ZMAX)   ZMAX = ET
        ENDDO
      ENDIF
C-
C====== Process PPHO ================================================
C-
      NPHO = 0
C-
      LPPHO = GZPPHO()
      IF (LPPHO .LE. 0) THEN
        GO TO 800
      ENDIF
C-
C--- Start PPHO loop...
C-
   52 IF (LPPHO .LE. 0)     GO TO 800
      NPHO = NPHO + 1
      E    = Q(LPPHO+6)
      ET   = Q(LPPHO+7)
      ETA  = Q(LPPHO+9)
      PHI  = Q(LPPHO+10)
      IF (ET .GT. ZMAX)   ZMAX = ET
C--- GO TO THE NEXT PPHO BANK
C-
   57 LPPHO = LQ(LPPHO)
      GO TO 52
C-
C====== Process PNUT ================================================
C-
  800 CONTINUE
C-
      DO 60 IPASS = 5,1,-1
        LPNUT = GZPNUT(IPASS)
        IF (LPNUT .GT. 0)    GO TO 70
   60 CONTINUE
      GO TO 999
C-
   70 E  = Q(LPNUT+6)
      ET = Q(LPNUT+7)
      IF (ET .GT. ZMAX)   ZMAX = ET
C-
  999 RETURN
      END
