      SUBROUTINE DFLSEG
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to fill the histograms related with segments
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: called by DFLHST
C-
C-   Created   8-JUN-1991   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CDPARA.INC'
      INTEGER GZDTSG, LDTSG, NDTSG, IDTSG, GZDRFT, LDRFT, GZDSEC, KPDSEC
      INTEGER IP, NDGF, LAY, IDPHI, IDCHI
      INTEGER IPHIT, IDSWR
      INTEGER SEC, WIR, NUMHIT, ISIDE, NFADC, LHIT, STATUS, IWIRE, LABEL
      LOGICAL HSTEFF, HSTCHI, HSTSWR
      REAL    CHI, CHIDGF, PHI, WRRESS, YR
C----------------------------------------------------------------------
C
      CALL DTSEFF(HSTEFF)
      CALL DTSCHI(HSTCHI)
      CALL DTSSWR(HSTSWR)
C
      IF (HSTCHI .OR. HSTEFF .OR. HSTSWR) THEN
        DO 100 LAY = 0, 3
          LDTSG = GZDTSG(LAY)
          IF (LDTSG .LE. 0) GOTO 100
          NDTSG = IQ(LDTSG + 1)
          IDPHI = 1014 + LAY
          IDCHI = 1102 + LAY
          DO 200 IDTSG = 1, NDTSG
            IP = LDTSG + 2 + (IDTSG - 1) * IQ (LDTSG + 2)
            STATUS = IQ(IP+1)
            IF (STATUS .LE. 0) GOTO 200    ! segment is not on track
            IF (HSTEFF) THEN
              PHI = Q(IP+5)
              CALL HF1(IDPHI,PHI,1.)
            ENDIF
            IF (HSTCHI) THEN
              NDGF = IQ(IP+2)
              CHI = Q(IP+8)
              CHIDGF = CHI / NDGF
              CALL HF1(IDCHI,CHIDGF,1.)
            ENDIF
            IF (HSTSWR) THEN
             LDRFT = GZDRFT()
             IF (LDRFT .LE. 0) GOTO 310     ! no STP banks
              DO 300 IWIRE = 0, MXSENS
                LABEL = IQ(IP+9+IWIRE)
                IF (LABEL .LE. 0) GOTO 300   ! no hit from this wire
                SEC = IBITS(LABEL, 11, 5)
                WIR = IBITS(LABEL,  8, 3)
                IF (WIR .NE. IWIRE) GOTO 300  ! screw up
                NUMHIT = IBITS(LABEL,  1, 7)
                ISIDE  = IBITS(LABEL,  0, 1)
                KPDSEC = GZDSEC(SEC,LAY)
                LHIT   = IQ(KPDSEC + 3)
                NFADC  = IQ(KPDSEC + 2)
                IPHIT  = IQ(KPDSEC + NFADC + 4 + WIR) +
     &                     (NUMHIT-1) * LHIT + KPDSEC
                YR = Q(IPHIT + ISIDE + 2) - C(LDRFT + 26 + WIR) 
                WRRESS = Q(IP + 16 + IWIRE)
                IDSWR = 1230 + LAY * NBSENS + IWIRE
                CALL HFILL(IDSWR,YR,WRRESS,1.)
  300         CONTINUE
  310        CONTINUE
            ENDIF
  200     CONTINUE
  100   CONTINUE
      ENDIF
C
  999 RETURN
      END
