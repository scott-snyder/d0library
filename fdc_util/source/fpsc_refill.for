C VAX/DEC CMS REPLACEMENT HISTORY, Element FPSC_REFILL.FOR
C *1     4-NOV-1993 10:58:13 AVERY "FDC changes for v12 RECO"
C VAX/DEC CMS REPLACEMENT HISTORY, Element FPSC_REFILL.FOR
      SUBROUTINE FPSC_REFILL(HALF,SECTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : RE-Fill FPSC POSITIONS from FPDA datas
C-
C-   Inputs  : HALF,SECTOR
C-   Outputs : FPSC bank is modified
C-
C-   Created  28-MAY-1993   Robert E. Avery, based on ZFFPSC.FOR
C-
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER HALF,SECTOR,WIRE
      INTEGER LKFPDA, LKFPSC, LPUL, J,IPFPSC,IPFPDA
      INTEGER CONT,Z,P1,P2,LSW,LSW1,T1,T2
      INTEGER I,LAY,SEC,NFPDA, NFPSC, LHIT, INIT, IER
      INTEGER MXNWHIT, IPSTART
      INTEGER GZFPDA, GZFPSC
      INTEGER LKFGUN,GZFGUN
C
      REAL TIME, STGOFF
      REAL ETZERO, ATZERO, VELOP, VELOM
      REAL FSTAGR
C
C----------------------------------------------------------------------
C
C **** Locate FPDA and FPSC banks to be used
C
      LKFPDA=GZFPDA(HALF,SECTOR)
      IF( LKFPDA .LE. 0 ) GOTO 999
      LKFPSC=GZFPSC(HALF,SECTOR)
      IF( LKFPSC .LE. 0 ) GOTO 999
C
C ****  Fill FPSC bank from FPDA, with time => cm.
C
      NFPDA = IQ( LKFPDA+2 )
      LPUL  = IQ( LKFPDA+3 )
      NFPSC = IQ( LKFPSC + 2 )
      LHIT  = IQ( LKFPSC + 3 )
      IPFPSC= 2*NFPSC + 4
      DO WIRE=0,NFPSC-1
        MXNWHIT = IQ( LKFPDA + 4+WIRE )
        IF (MXNWHIT .NE. 0) THEN
          IPFPDA = IQ( LKFPDA+4+NFPDA+WIRE )
          STGOFF = FSTAGR(HALF,1,0,0,WIRE)
          CALL FGTLTM(HALF,1,0,SECTOR,WIRE,
     &                ETZERO,ATZERO,VELOP,VELOM)
          VELOP=VELOP/10000.
          VELOM=VELOM/10000.
          DO J = 1, MXNWHIT
            P1 = LKFPDA + IPFPDA - 1
            P2 = LKFPSC + IPFPSC - 1
            TIME = Q(P1+2)-ATZERO
            Q (P2+2) = STGOFF + VELOP * TIME
            Q (P2+3) = STGOFF + VELOM * TIME
            Q (P2+5) = VELOP * Q(P1+6)
            IF (ABS(Q(P2+5)) .GT. 99.) Q(P2+5) = 99.0
            IPFPSC = IPFPSC  + LHIT
            IPFPDA = IPFPDA  + LPUL
          ENDDO
        ENDIF
      ENDDO
C------------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
