      SUBROUTINE LTRKFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Fill packed CDC/FDC tracks for micro STA
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   7-JUL-1993   Brent J. May
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INTEGER LLTRK, LDTRK, GZDTRK, NCDC, NFDC, ISKP, ITRK
      INTEGER LDTRH, GZDTRH, LFTRH, GZFTRH, LFDCT, GZFDCT
      REAL X0, Y0, Z0, THETA, PHI, CHISQ, IONIZ
      INTEGER NHIT, NZ, NFIT, ICHI_RZ, ICHI_XY, ICHINORM, IIONIZ
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
      INTEGER IWORD, STAT, ITHT, IPHI, ITEST
C----------------------------------------------------------------------
C
C Get CDC number of tracks
C
      LDTRH = GZDTRH()
      IF(LDTRH.LE.0)THEN
        CALL ERRMSG('MICRO_STA:CDC','LTRKFL',
     &    '       DTRH BANK DOES NOT EXIST FOR MICRO_STA' ,'W')
        NCDC = 0
      ELSE
        NCDC = IQ(LDTRH+2)
      ENDIF
C
C Get FDC number of tracks
C
      LFTRH = GZFTRH()
      IF(LFTRH.LE.0)THEN
        CALL ERRMSG('MICRO_STA:FDC','LTRKFL',
     &    '       FTRH BANK DOES NOT EXIST FOR MICRO-STA' ,'W')
        NFDC = 0
      ELSE
        NFDC = IQ(LFTRH+2)
      ENDIF
C
      IF (NCDC.EQ.0 .AND. NFDC.EQ.0 ) RETURN
      CALL BKLTRK(LLTRK,NCDC,NFDC)
C
C Loop over CDC tracks
C
      ITRK = 0
      IF ( NCDC.GT.0 ) THEN
        LDTRK = GZDTRK(0)
        DO WHILE (LDTRK.NE.0)
          ITRK = ITRK + 1
          ISKP = (ITRK-1) * 6 + 4
C
C Get CDC tracking info from DTRK
C
          X0 = Q(LDTRK+7)
          Y0 = Q(LDTRK+8)
          Z0 = Q(LDTRK+11)
          IIONIZ = NINT(Q(LDTRK+20)*1.0E4)
          THETA = Q(LDTRK+9)
          PHI = Q(LDTRK+6)
          NHIT = IQ(LDTRK+2)
          NZ = IQ(LDTRK+5)
          IF (NHIT.GT.2) THEN
            ICHI_XY = NINT(Q(LDTRK+12)/(NHIT-2))
            IF (ICHI_XY.GT.255) ICHI_XY = 255
          ELSE
            ICHI_XY = 255
          ENDIF
          IF (NZ.GT.2) THEN
            ICHI_RZ = NINT(Q(LDTRK+13)/(NZ-2))
            IF (ICHI_RZ.GT.255) ICHI_RZ = 255
          ELSE
            ICHI_RZ = 255
          ENDIF
C
C Fill packed LTRK bank
C
          Q(LLTRK+1+ISKP) = X0
          Q(LLTRK+2+ISKP) = Y0
          Q(LLTRK+3+ISKP) = Z0
          ITHT = NINT(10000.*THETA)  
          IPHI = NINT(10000.*PHI)
          IWORD = 0
          CALL MVBITS(ITHT,0,16,IWORD,0)
          CALL MVBITS(IPHI,0,16,IWORD,16)
          IQ(LLTRK+4+ISKP) = IWORD
          IWORD = 0
          CALL MVBITS(NHIT,0,8,IWORD,0)
          CALL MVBITS(ICHI_XY,0,8,IWORD,8)
          CALL MVBITS(NZ,0,8,IWORD,16)
          CALL MVBITS(ICHI_RZ,0,8,IWORD,24)
          IQ(LLTRK+5+ISKP) = IWORD
          IWORD = 0
          CALL MVBITS(IIONIZ,0,24,IWORD,0)
          IQ(LLTRK+6+ISKP) = IWORD
          LDTRK = LQ(LDTRK)
        ENDDO
      ENDIF
C
C Loop over FDC tracks
C
      IF ( NFDC.GT.0 ) THEN
        LFDCT = GZFDCT(0)
        DO WHILE (LFDCT.NE.0)
          ITRK = ITRK + 1
          ISKP = (ITRK-1) * 6 + 4
C
C Get FDC tracking info from FDCT
C
          X0 = Q(LFDCT+4)
          Y0 = Q(LFDCT+5)
          THETA = Q(LFDCT+22)
          PHI = Q(LFDCT+6)
          NHIT = IQ(LFDCT+2)
          NFIT = IQ(LFDCT+25)
          CHISQ = Q(LFDCT+19)
          STAT = IQ(LFDCT+1)
          IONIZ = Q(LFDCT+20)
C
C Fill packed LTRK bank
C
          Q(LLTRK+1+ISKP) = X0
          Q(LLTRK+2+ISKP) = Y0
          Q(LLTRK+3+ISKP) = IONIZ
          ITHT = NINT(10000.*THETA)
          IPHI = NINT(10000.*PHI)
          IWORD = 0
          CALL MVBITS(ITHT,0,16,IWORD,0)
          CALL MVBITS(IPHI,0,16,IWORD,16)
          IQ(LLTRK+4+ISKP) = IWORD
          IWORD = 0
          CALL MVBITS(NFIT,0,8,IWORD,0)
          ICHINORM = NINT(10.*(SQRT(2.*CHISQ)-SQRT(2.*(NFIT-4)-1)))+128
          ICHINORM = MIN(MAX(ICHINORM,0),255)
          CALL MVBITS(ICHINORM,0,8,IWORD,8)
          CALL MVBITS(NHIT,0,8,IWORD,16)
          IQ(LLTRK+5+ISKP) = IWORD
          IQ(LLTRK+6+ISKP) = STAT
          LFDCT = LQ(LFDCT)
        ENDDO
      ENDIF
C
  999 RETURN
      END
