      SUBROUTINE ZFFPDA ( HALF, SECTOR, NPULSE )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill FPDA bank with one cell information
C-                         from array HITS.
C-
C-   Inputs  : HALF, SECTOR
C-             NPULSE(*)    = nb of hits per wire
C-
C-   Created   6-FEB-1989   Jeffrey Bantly
C-   Updated  28-FEB-1990   Jeffrey Bantly  clean up
C-   Updated   5-NOV-1990   Jeffrey Bantly  check for drift time too long
C-   Updated  20-MAR-1991   Jeffrey Bantly  correct FPDA filling extras
C-   Updated  29-APR-1991   Jeffrey Bantly  use new RCP,PARAMS 
C-   Updated   6-JUN-1991   Robert E. Avery  Use  SRTINT instead of SORTZV
C-   Updated  21-OCT-1991   Robert E. Avery  Move maxtime cut to FDPULS.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:FDEVNT.INC'
C
      INTEGER HALF, SECTOR, WIRE, INIT, NHIT, IER
      INTEGER NPULSE(0:NBPSEN-1), LKFPDA
      INTEGER IPTR, N, NHTOT, LPULS, NFADC, IHIT, IPULS
      INTEGER TIME(MX_HIT_WIRE),IORDER(MX_HIT_WIRE),INEXT
      INTEGER GZFPDA
      EXTERNAL GZFPDA
C
      CHARACTER*80 VARMSG
C
C----------------------------------------------------------------------
C
C  Locate FPDA bank to be used
C
      LKFPDA=GZFPDA(HALF,SECTOR)
      IF( LKFPDA .LE. 0 ) THEN
        VARMSG = 'FDC - FPDA not booked and should have been'
        CALL ERRMSG('FDC-BANK NOT BOOKED','ZFFPDA',VARMSG,'I')
        GOTO 999
      ENDIF
C
C  Fill FPDA bank
C
      NHTOT=0
      NFADC = IQ( LKFPDA + 2 )
      LPULS = IQ( LKFPDA + 3 )
      IPTR = LKFPDA + 2 * NFADC + 3
      DO 1 WIRE = 0 , NFADC-1
        IF ( NPULSE(WIRE) .NE. 0) THEN
          IQ (LKFPDA+WIRE+NFADC+4) = IPTR - LKFPDA + 1
          NHIT = 0
C
          DO IHIT = 1, NPULSE(WIRE)
            IORDER(IHIT)=IHIT
            TIME(IHIT)=HITS(2,IHIT,WIRE)
          ENDDO
C
          CALL SRTINT(TIME,NPULSE(WIRE),IORDER)
C
          DO 2 INEXT = 1, NPULSE(WIRE)
            IHIT = IORDER(INEXT)
            IQ(IPTR+1) = INT(HITS(1,IHIT,WIRE))
            DO 3 IPULS = 2, LPULS
              Q(IPTR+IPULS)=HITS(IPULS,IHIT,WIRE)
    3       CONTINUE
C            IQ(IPTR+8) = INT( HITS(8,IHIT,WIRE) )
            IQ(IPTR+8) = 0        ! temporary
            IPTR = IPTR + LPULS
            NHIT = NHIT + 1
    2     CONTINUE
          NHTOT = NHTOT + NHIT
          IQ( LKFPDA+WIRE+4 ) = NHIT
        ELSE
          IQ (LKFPDA+WIRE+NFADC+4)=0
        ENDIF
    1 CONTINUE
      IQ (LKFPDA+1)=NHTOT
C-----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
