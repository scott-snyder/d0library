      SUBROUTINE PD1TRK(LDTRK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : display one track DTRK
C-
C-   Inputs  : LDTRK: bank address of the track to be drawn
C-   Outputs : none
C-
C-   Created  26-JUL-1990   Qizhong Li-Demarteau modified from PDTRCK
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GRAPHF77.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CDPARA.INC'
      INCLUDE 'D0$INC:PXPARA.INC'
C
      INTEGER LDTRK
      INTEGER I
      INTEGER NHITS(0:NBSENS-1), NWIR, LHIT
      INTEGER KPWIRE(0:NBSENS-1), KP
      INTEGER LAY, SEC, IWIR, ISEG, GZDTRH, LDTRH, GZDSEC, GZDALS
      INTEGER NTRK, IPHIT, WRFLAG, ITRK
      INTEGER WIR, NUMHIT, ISIDE, HLABEL, J, PLDTTH, KPDSEC, NFADC
      INTEGER IFDSEC, IFDWIR, IFDHIT, IFDTRK, IFDLBL
      REAL    PHIW, CPHIW, SPHIW
      REAL    YR, DISTAN, SLOP
      REAL    DDIS1, DDIS
      REAL    XHPOS, YHPOS, DEGRAD
      REAL    RAYCEN, DELRAY, PHICEN, DELPHI
      REAL    X1, X2, Y1, Y2, R1, R2, PHI1, PHI2
      INTEGER LDRFT, LDALS, IPWIR
      REAL    XWIR, YWIR, TLSEG, XCENT, YCENT, XORI, YORI
      INTEGER NBWIR
      PARAMETER( DEGRAD = 3.1415926535/180.)
C
      DATA TLSEG / 13.0 /
C --------------------------------------------------------------------
C
      IF (LDTRK .LE. 0) GOTO 999
      KP = LDTRK
      CALL PUOPEN
      CALL PXCOLR('FOR')
C
C first, bold the hits on the track 
C
C      PLDTTH = LQ(KP-1)             ! pointer to bank 'DTTH'
C      DO 200 I = 0,27
C        WRFLAG = IBITS(IQ(KP+3),I,1)  ! XXflag=0 no hit on this wire;
C        IF (WRFLAG.NE.0) THEN
C          HLABEL = IQ(PLDTTH+1)                ! get hit label
C          PLDTTH = PLDTTH+2
C          LAY = IBITS(HLABEL, 16, 2)
C          SEC = IBITS(HLABEL, 11, 5)
C          WIR = IBITS(HLABEL,  8, 3)
C          NUMHIT = IBITS(HLABEL,  1, 7)
C          ISIDE  = IBITS(HLABEL,  0, 1)
C          KPDSEC = GZDSEC(SEC, LAY)
C          IF (KPDSEC .LE. 0) GOTO 200
C          LHIT   = IQ(KPDSEC + 3)
C          NFADC  = IQ(KPDSEC + 2)
C          IPHIT  = IQ(KPDSEC + NFADC + 4 + WIR) +
C     &                     (NUMHIT-1) * LHIT + KPDSEC
C          YR = Q(IPHIT + ISIDE + 2) - C(LC(LDGEH-3) +26+WIR) !
C                                        ! drift distance
C          LDALS = LC( LC( LC( LSCDC-5 ) -(LAY+1) ) -(SEC+1) )
C          CPHIW = C( LDALS+3 )
C          SPHIW = C( LDALS+4 )
C          IPWIR = LDALS + 6 + IC(LDALS+6) * WIR
C          XWIR = C( IPWIR+1 )
C          YWIR = C( IPWIR+2 )
C          XHPOS = XWIR + YR * CPHIW
C          YHPOS = YWIR + YR * SPHIW
C          CALL JCMARK(1)
C          CALL JJUST(2,2)
C          CALL JMARK( XHPOS, YHPOS )
C        ENDIF
C  200 CONTINUE
C
C  then draw the full tracks 
C
      XHPOS = Q( KP+7 ) + TLSEG * COS( Q(KP+6) )
      YHPOS = Q( KP+8 ) + TLSEG * SIN( Q(KP+6) )
      CALL JMOVE( XHPOS, YHPOS )
      XHPOS = Q( KP+7 ) - TLSEG * COS( Q(KP+6) )
      YHPOS = Q( KP+8 ) - TLSEG * SIN( Q(KP+6) )
      CALL JDRAW( XHPOS, YHPOS )
C
  212 CALL JRCLOS
C
  999 RETURN
      END
