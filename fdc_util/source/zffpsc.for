      SUBROUTINE ZFFPSC(HALF,SECTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill FPSC hits bank from FPDA data bank.
C-
C-   Inputs  : HALF, SECTOR
C-   Outputs : FPSC bank is filled
C-
C-   Created   6-FEB-1989   Jeffrey Bantly   based on OC original for CDC
C-   Updated  28-FEB-1990   Jeffrey Bantly   clean up
C-   Updated  24-JUL-1990   Jeffrey Bantly   set all the hit words in bank
C-   Updated  20-MAR-1991   Jeffrey Bantly   fill ionization words
C-   Updated   4-NOV-1991   Robert E. Avery  VAX intrinsice functions fix
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block. 
C-   Updated  13-MAY-1993   Robert E. Avery  Read global gains constants,
C-                            filled into FGUN banks (FGTH and FGPH).
C-   Updated   9-NOV-1993   Robert E. Avery  Only retain 1 micron precision 
C-    in drift positions (for consistancy with FHIT bank).
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER HALF,SECTOR,WIRE
      INTEGER LKFPDA, LKFPSC, LPUL, J,NHITS,IPFPSC,IPFPDA
      INTEGER CONT,Z,P1,P2,LSW,LSW1,T1,T2
      INTEGER I,LAY,SEC,NWHIT, NFPDA, NFPSC, LHIT, INIT, IER
      INTEGER MXNWHIT, IPSTART
      INTEGER GZFPDA, GZFPSC
      INTEGER LKFGUN,GZFGUN
C
      REAL TIME, GAIN, STGOFF, VELOC, TZERO, GAS_GAIN, MIPCONV
      REAL ETZERO, ATZERO, VELOP, VELOM
      REAL FSTAGR
C
      CHARACTER*80 VARMSG
C
      SAVE INIT,VELOC,TZERO,GAS_GAIN
      DATA INIT / 0 /
C----------------------------------------------------------------------
      IF( INIT .LE. 0 ) THEN
        INIT = 1
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('VELOCP',VELOC,IER)
        CALL EZGET('TZERO',TZERO,IER)
        CALL EZRSET
      ENDIF
C
C Get Gain correction
C
      LKFGUN=GZFGUN(HALF,1)
      GAS_GAIN = C(LKFGUN+1)  ! Read GLOBAL GAIN value
      IF ( (GAS_GAIN.LE.0.) .OR. (GAS_GAIN.GT.100.) ) 
     &  GAS_GAIN = 1.
C
C **** Locate FPDA and FPSC banks to be used
C
      LKFPDA=GZFPDA(HALF,SECTOR)
      IF( LKFPDA .LE. 0 ) THEN
        VARMSG = 'FDC - FPDA not booked and should have been'
        CALL ERRMSG('FDC-BANK NOT BOOKED','ZFFPSC',VARMSG,'I')
        GOTO 999
      ENDIF
      LKFPSC=GZFPSC(HALF,SECTOR)
      IF( LKFPSC .LE. 0 ) THEN
        VARMSG = 'FDC - FPSC not booked and should have been'
        CALL ERRMSG('FDC-BANK NOT BOOKED','ZFFPSC',VARMSG,'I')
        GOTO 999
      ENDIF
C
C ****  Fill FPSC bank from FPDA, with time => cm and FADC => M.I.P.
C ****  conversion .
C
      NFPDA = IQ( LKFPDA+2 )
      LPUL  = IQ( LKFPDA+3 )
      NHITS=0
      NFPSC = IQ( LKFPSC + 2 )
      LHIT  = IQ( LKFPSC + 3 )
      IPFPSC= 2*NFPSC + 4
      DO 10 WIRE=0,NFPSC-1
        MXNWHIT = IQ( LKFPDA + 4+WIRE )
        IQ (LKFPSC+4+WIRE) = 0
        IF (MXNWHIT .NE. 0) THEN
          IPSTART = IPFPSC     ! Save starting pointer in FPSC
          IPFPDA = IQ( LKFPDA+4+NFPDA+WIRE )
          STGOFF = FSTAGR(HALF,1,0,0,WIRE)
          CALL FGTLGN(HALF,1,0,SECTOR,WIRE,GAIN,MIPCONV)
          IF ( MIPCONV .LE. 0.0 ) MIPCONV = 1.0
          MIPCONV = GAS_GAIN * MIPCONV
          CALL FGTLTM(HALF,1,0,SECTOR,WIRE,
     &                ETZERO,ATZERO,VELOP,VELOM)
          IF ( VELOP.LE. 0.0 ) THEN
            VELOP=VELOC
            VELOM=-VELOC
            ATZERO=TZERO
          ENDIF
          VELOP=VELOP/10000.
          VELOM=VELOM/10000.
          NWHIT = 0
          DO 30 J = 1, MXNWHIT
            P1 = LKFPDA + IPFPDA - 1
            TIME = Q(P1+2)-ATZERO
            NWHIT = NWHIT + 1
            P2 = LKFPSC + IPFPSC - 1
            IQ(P2+1) = IQ( P1+1)
            Q (P2+2) = STGOFF + VELOP * TIME
            Q (P2+3) = STGOFF + VELOM * TIME
C
C  Only retain 1 micron precision (for consistancy with FHIT bank):
C
            Q (P2+2) = FLOAT(NINT( Q(P2+2)*10000 )) / 10000.
            Q (P2+3) = FLOAT(NINT( Q(P2+3)*10000 )) / 10000.
            Q (P2+4) = 0.0
            Q (P2+5) = VELOP * Q(P1+6)
            IF (ABS(Q(P2+5)) .GT. 99.) Q(P2+5) = 99.0
            Q (P2+6) = 9999.
            Q (P2+7) = MIPCONV * Q(P1+3)
            Q (P2+8) = MIPCONV * Q(P1+7)
            IQ(P2+9) = 0
            CALL MVBITS( IQ(P1+8), 0, 8, IQ(P2+9), 8 )    ! status byte
            IQ(P2+10)= IPFPDA - 1
            IQ(P2+11)= 0
            IQ(P2+12)= 0
            IPFPSC = IPFPSC  + LHIT
            IPFPDA = IPFPDA  + LPUL
   30     CONTINUE
          IF(NWHIT.GE.1) THEN
            IQ(LKFPSC+4+NFPSC+WIRE) = IPSTART     ! Insert pointers in FPSC
            NHITS = NHITS + NWHIT
            IQ (LKFPSC+4+WIRE) = NWHIT
          ELSE
            IQ(LKFPSC+4+NFPSC+WIRE) = 0
          ENDIF
        ELSE
          IQ(LKFPSC+4+NFPSC+WIRE) = 0
        ENDIF
   10 CONTINUE
      IQ (LKFPSC+1)=NHITS            ! Insert Total # Of Hits In Sector
C------------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
