      SUBROUTINE ZFFTSC(HALF,QUAD,SECTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill FTSC bank from FTDA datas
C-
C-   Inputs  : HALF,QUAD,SECTOR
C-   Outputs : FTSC bank is filled
C-
C-   Created   6-FEB-1989   Jeffrey Bantly  based on OC original for CDC
C-   Updated  24-JUL-1990   Jeffrey Bantly  set all the hit words in bank
C-   Updated  20-MAR-1991   Jeffrey Bantly  fill ionization words
C-   Updated  29-APR-1991   Jeffrey Bantly  use new RCP,PARAMS 
C-   Updated   4-NOV-1991   Robert E. Avery  VAX intrinsice functions fix
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block. 
C-   Updated  25-FEB-1992   Susan K. Blessing  Relax limit for making 
C-    mirror hits in sectors 0-2.
C-   Updated  13-MAY-1993   Robert E. Avery  Read global gains constants,
C-                            filled into FGUN banks (FGTH and FGPH).
C-   Updated   8-NOV-1993   Robert E. Avery  Make the  limit for making 
C-    mirror hits in sectors 0-2 a parameter in D0$PARAMS:FDPARA.PARAMS
C-    (also used in FHIT_DECODE).
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
      INTEGER HALF,QUAD,SECTOR,WIRE
      INTEGER LKFTDA, LKFTSC, LPUL, J,NHITS,IPFTSC,IPFTDA
      INTEGER CONT,Z,P1,P2,LSW,LSW1,T1,T2
      INTEGER I,LAY,SEC,NWHIT, NFTDA, NFTSC, LHIT, INIT, IER
      INTEGER IPSTART, MXNWHIT
      INTEGER GZFTDA, GZFTSC
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
        CALL EZGET('VELOCT',VELOC,IER)
        CALL EZGET('TZERO',TZERO,IER)
        CALL EZRSET
      ENDIF
C
C Get Gain correction
C
      LKFGUN=GZFGUN(HALF,0)
      GAS_GAIN = C(LKFGUN+1)  ! Read GLOBAL GAIN value
      IF ( (GAS_GAIN.LE.0.) .OR. (GAS_GAIN.GT.100.) ) 
     &  GAS_GAIN = 1.
C
C ****  Fill FTSC bank from FTDA, with time => cm and FADC => M.I.P.
C ****  conversion .
C
      LKFTDA=GZFTDA(HALF,QUAD,SECTOR)
      IF( LKFTDA .LE. 0 ) THEN
        VARMSG = 'FDC - FTDA not booked and should have been'
        CALL ERRMSG('FDC-BANK NOT BOOKED','ZFFTSC',VARMSG,'I')
        GOTO 999
      ENDIF
      LKFTSC=GZFTSC(HALF,QUAD,SECTOR)
      IF( LKFTSC .LE. 0 ) THEN
        VARMSG = 'FDC - FTSC not booked and should have been'
        CALL ERRMSG('FDC-BANK NOT BOOKED','ZFFTSC',VARMSG,'I')
        GOTO 999
      ENDIF
C
C ****  Fill FTSC bank from FTDA, with time => cm and FADC => M.I.P.
C ****  conversion .
C
      NFTDA = IQ( LKFTDA+2 )
      LPUL  = IQ( LKFTDA+3 )
      NHITS=0
      NFTSC = IQ( LKFTSC + 2 )
      LHIT  = IQ( LKFTSC + 3 )
      IPFTSC= 2*NFTSC + 4
      DO 10 WIRE=0,NFTSC-1
        MXNWHIT = IQ( LKFTDA + 4+WIRE )
        IQ (LKFTSC+4+WIRE) = 0
        IF (MXNWHIT .NE. 0) THEN
          IPSTART = IPFTSC      ! Save start pointer in FTSC
          IPFTDA = IQ( LKFTDA+4+NFTDA+WIRE )
          STGOFF = FSTAGR(HALF,0,QUAD,SECTOR,WIRE)
          CALL FGTLGN(HALF,0,QUAD,SECTOR,WIRE,GAIN,MIPCONV)
          IF ( MIPCONV .LE. 0.0 ) MIPCONV=1.0
          MIPCONV = GAS_GAIN * MIPCONV
          CALL FGTLTM(HALF,0,QUAD,SECTOR,WIRE,
     &                             ETZERO,ATZERO,VELOP,VELOM)
          IF ( VELOP.LE. 0.0 ) THEN
            VELOP=VELOC
            VELOM=-VELOC
            ATZERO=TZERO
          ENDIF
          VELOP=VELOP/10000.
          VELOM=VELOM/10000.
          NWHIT = 0
          DO 30 J = 1, MXNWHIT
            P1 = LKFTDA + IPFTDA - 1
            TIME = ABS(Q(P1+2)-ATZERO)
            NWHIT = NWHIT + 1
            P2 = LKFTSC + IPFTSC - 1
            IQ(P2+1) = IQ( P1+1)
            IF( SECTOR .LE. 2 ) THEN
              Q (P2+2) = VELOP * TIME
              Q (P2+3) = 0.0
              IF(VELOP*TIME.LE.XCUT_HSEC) THEN
                Q(P2+3) = VELOM * TIME
              ENDIF
            ELSE
              Q (P2+2) = STGOFF + VELOP * TIME
              Q (P2+3) = STGOFF + VELOM * TIME
            ENDIF
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
            IQ(P2+10)= IPFTDA - 1
            IQ(P2+11)= 0
            IQ(P2+12)= 0
            IPFTSC = IPFTSC  + LHIT
            IPFTDA = IPFTDA  + LPUL
   30     CONTINUE
          IF(NWHIT.GE.1) THEN
            NHITS = NHITS + NWHIT
            IQ (LKFTSC+4+WIRE) = NWHIT
            IQ(LKFTSC+4+NFTSC+WIRE) = IPSTART      ! Insert pointer in FTSC
          ELSE
            IQ(LKFTSC+4+NFTSC+WIRE) = 0
          ENDIF
        ELSE
          IQ(LKFTSC+4+NFTSC+WIRE) = 0
        ENDIF
   10 CONTINUE
      IQ (LKFTSC+1)=NHITS            ! Insert Total # Of Hits In Sector
C---------------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
