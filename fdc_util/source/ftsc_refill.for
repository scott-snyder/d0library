C VAX/DEC CMS REPLACEMENT HISTORY, Element FTSC_REFILL.FOR
C *1     4-NOV-1993 10:59:11 AVERY "FDC changes for v12 RECO"
C VAX/DEC CMS REPLACEMENT HISTORY, Element FTSC_REFILL.FOR
      SUBROUTINE FTSC_REFILL(HALF,QUAD,SECTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : RE-Fill FTSC POSITIONS from FTDA datas
C-
C-   Inputs  : HALF,QUAD,SECTOR
C-   Outputs : FTSC bank is modified
C-
C-   Created  28-MAY-1993   Robert E. Avery, based on ZFFTSC.FOR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER HALF,QUAD,SECTOR,WIRE
      INTEGER LKFTDA, LKFTSC, LPUL, J,IPFTSC,IPFTDA
      INTEGER CONT,Z,P1,P2,LSW,LSW1,T1,T2
      INTEGER I,LAY,SEC,NFTDA, NFTSC, LHIT, INIT, IER
      INTEGER MXNWHIT
      INTEGER GZFTDA, GZFTSC
      INTEGER LKFGUN,GZFGUN
C
      REAL TIME, STGOFF
      REAL ETZERO, ATZERO, VELOP, VELOM
      REAL FSTAGR
C
      CHARACTER*80 VARMSG
C
C----------------------------------------------------------------------
C
      LKFTDA=GZFTDA(HALF,QUAD,SECTOR)
      IF( LKFTDA .LE. 0 ) GOTO 999
      LKFTSC=GZFTSC(HALF,QUAD,SECTOR)
      IF( LKFTSC .LE. 0 ) GOTO 999
C
C ****  Fill FTSC bank from FTDA, with time => cm.
C
      NFTDA = IQ( LKFTDA+2 )
      LPUL  = IQ( LKFTDA+3 )
      NFTSC = IQ( LKFTSC + 2 )
      LHIT  = IQ( LKFTSC + 3 )
      IPFTSC= 2*NFTSC + 4
      DO WIRE=0,NFTSC-1
        MXNWHIT = IQ( LKFTDA + 4+WIRE )
        IF (MXNWHIT .NE. 0) THEN
          IPFTDA = IQ( LKFTDA+4+NFTDA+WIRE )
          STGOFF = FSTAGR(HALF,0,QUAD,SECTOR,WIRE)
          CALL FGTLTM( HALF,0,QUAD,SECTOR,WIRE,
     &                 ETZERO,ATZERO,VELOP,VELOM)
          VELOP=VELOP/10000.
          VELOM=VELOM/10000.
          DO J = 1, MXNWHIT
            P1 = LKFTDA + IPFTDA - 1
            P2 = LKFTSC + IPFTSC - 1
C
            TIME = Q(P1+2)-ATZERO
            IF( SECTOR .LE. 2 ) THEN
              Q (P2+2) = VELOP * TIME
              Q (P2+3) = 0.0
              IF(VELOP*TIME.LE. 0.45) THEN
                Q(P2+3) = VELOM * TIME
              ENDIF
            ELSE
              Q (P2+2) = STGOFF + VELOP * TIME
              Q (P2+3) = STGOFF + VELOM * TIME
            ENDIF
            Q (P2+5) = VELOP * Q(P1+6)
            IF (ABS(Q(P2+5)) .GT. 99.) Q(P2+5) = 99.0
            IPFTSC = IPFTSC  + LHIT
            IPFTDA = IPFTDA  + LPUL
          ENDDO
        ENDIF
      ENDDO
C---------------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
