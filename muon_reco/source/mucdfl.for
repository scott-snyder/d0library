      SUBROUTINE MUCDFL(IMUON,IZTRK,LMUCD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To book, calculate and fill the MUCD bank.
C-
C-   Inputs  : NTRK     MUON track number
C-             IZTRK    Pointer to link to ZTRK bank
C-
C-   Outputs : LMUCD    Link to MUCD bank
C-   Controls:
C-
C-   Created   05-JUN-1990   SHAHRIAR ABACHI
C-   Modified  10-OCT-1991   SHAHRIAR ABACHI  FDC and VTX are now considered
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:ZLINKA.INC/LIST'
      INTEGER IZTRK, IZ, NTRK, IMUON
      INTEGER LMUON, GZMUON, LZTRK, IFLG
      INTEGER LMUCD, I, ITRKN, LDTRK, LFTRK, LVTRK
      REAL THE_TRK,PHI_TRK,X0,Y0,Z0,R0,ERR_THE,ERR_PHI,ERR_XY,ERR_RZ
C
      CALL BKMUCD(0,IMUON,LMUCD)
C
      LMUON = LRLINK(IMUON)
C
      LZTRK = LSLINK(IZTRK)
      LDTRK = LQ(LZTRK - 7)
      LFTRK = LQ(LZTRK - 8)
      LVTRK = LQ(LZTRK - 6)
C
      IFLG = 0
      Z0 = 0.
      R0 = 0.
      ERR_THE = -1.
      ERR_PHI = -1.
      ERR_XY = -1.
      ERR_RZ = -1.
C
      IF(LFTRK .GT. 0) THEN
        IFLG = 100
        PHI_TRK = Q(LFTRK + 6)
        THE_TRK = Q(LFTRK + 22)
        ERR_THE = Q(LFTRK + 24)
        ERR_PHI = Q(LFTRK + 23)
        X0 = Q(LFTRK + 4)
        Y0 = Q(LFTRK + 5)
        IF(ABS(SIN(THE_TRK)*COS(PHI_TRK)) .GT. 0.0) THEN
          R0 = X0 / (SIN(THE_TRK)*COS(PHI_TRK))
        ELSEIF(ABS(SIN(THE_TRK)*SIN(PHI_TRK)) .GT. 0.0) THEN
          R0 = Y0 / (SIN(THE_TRK)*SIN(PHI_TRK))
        ENDIF
        IF(ABS(COS(THE_TRK)) .GT. 0.0) Z0 = R0 / COS(THE_TRK)
      ENDIF
C
      IF(LDTRK .GT. 0) THEN
        IFLG = IFLG + 10
        PHI_TRK = Q(LDTRK + 6)
        THE_TRK = Q(LDTRK + 9)
        X0 = Q(LDTRK + 7)
        Y0 = Q(LDTRK + 8)
        Z0 = Q(LDTRK + 11)
        R0 = Q(LDTRK + 10)
        ERR_THE = Q(LDTRK + 18)
        ERR_PHI = Q(LDTRK + 16)
        ERR_XY = Q(LDTRK + 17)
        ERR_RZ = Q(LDTRK + 19)
      ENDIF
C
      IF(LVTRK .GT. 0) THEN
        IFLG = IFLG + 1
        PHI_TRK = Q(LVTRK + 6)
        ERR_PHI = Q(LVTRK + 16)
        IF(LDTRK .LE. 0 .AND. LFTRK .LE. 0) THEN
          THE_TRK = Q(LVTRK + 9)
          ERR_THE = Q(LVTRK + 18)
        ENDIF
        X0 = Q(LVTRK + 7)
        Y0 = Q(LVTRK + 8)
        IF(ABS(SIN(THE_TRK)*COS(PHI_TRK)) .GT. 0.0) THEN
          R0 = X0 / (SIN(THE_TRK)*COS(PHI_TRK))
        ELSEIF(ABS(SIN(THE_TRK)*SIN(PHI_TRK)) .GT. 0.0) THEN
          R0 = Y0 / (SIN(THE_TRK)*SIN(PHI_TRK))
        ENDIF
        IF(ABS(COS(THE_TRK)) .GT. 0.0) Z0 = R0 / COS(THE_TRK)
        ERR_PHI = Q(LVTRK + 16)
        ERR_XY = Q(LVTRK + 17)
      ENDIF
C
C - reference pointer to ZTRK bank
C
      LQ(LMUCD - 1) = LZTRK
C
C - Data part
C
CC      IQ(LMUCD + 1)  = 1
      IQ(LMUCD + 2)  = 0
      IQ(LMUCD + 3)  = IFLG
      IQ(LMUCD + 4)  = 0
      IQ(LMUCD + 5)  = IQ(LZTRK - 5)
C
      Q(LMUCD + 6)  = PHI_TRK
      Q(LMUCD + 7)  = X0
      Q(LMUCD + 8)  = Y0
      Q(LMUCD + 9)  = THE_TRK
      Q(LMUCD + 10)  = R0
      Q(LMUCD + 11)  = Z0
      Q(LMUCD + 12)  = 0.
      Q(LMUCD + 13) = 0.
      Q(LMUCD + 14) = 0
      Q(LMUCD + 15) = ERR_PHI
      Q(LMUCD + 16) = ERR_XY
      Q(LMUCD + 17) = ERR_THE
      Q(LMUCD + 18) = ERR_RZ
      IQ(LMUCD + 19) = 0.
C
C----------------------------------------------------------------------
  999 RETURN
      END
