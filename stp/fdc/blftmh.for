      SUBROUTINE BLFTMH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Build the time_to_position banks
C-
C-   Inputs  : None, but ZEBRA structure up to SFDC should exist.
C-   Outputs :
C-
C-   Created   5-AUG-1988   Jeffrey Bantly
C-   Updated  19-FEB-1990   Jeffrey Bantly  convert to logical system
C-   Updated   5-MAR-1991   Robert E. Avery  Add extra words. 
C-   Updated  20-MAR-1991   Jeffrey Bantly  put in Monte Carlo defaults 
C-   Updated  25-JUN-1991   Robert E. Avery  Fill in Delay line lengths 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FDCPRM.INC'
      INCLUDE 'D0$LINKS:IZFTMH.LINK'
      INTEGER MPFTHF(5), MPFTTH(5)
      INTEGER MPFTQD(5), MPFTPH(5), MPFTSE(5)
      INTEGER LOWRUN, HIGRUN, NUMTWR, NUMPWR
      INTEGER NPARWR, NPARDL, NUMTDL, NUMPDL
      INTEGER HALF, UNIT, QUAD, SECTOR
      INTEGER WIRE, DELAY
      INTEGER GZSFDC, LBANK, LKFTMH, LKFTSE
      INTEGER QUADTYPE(0:7,0:1)
      PARAMETER( NUMTWR = 8 )
      PARAMETER( NUMTDL = 1 )
      PARAMETER( NUMPWR = 16 )
      PARAMETER( NUMPDL = 0 )
      PARAMETER( NPARWR = 4 )
      PARAMETER( NPARDL = 4 )
      REAL TZELEC,TZELEC_SIG,TZABS
      REAL VELT_PLU,VELT_MIN,VELP_PLU,VELP_MIN,VELDL
      REAL DELLEN(0:5,2)
C
      LOGICAL FIRST
C
      DATA MPFTHF / 0,  2,  2,  2,  2 /
      DATA MPFTTH / 0,  8,  8,  2,  2 /
      DATA MPFTQD / 0,  6,  6,  2,  2 /
      DATA MPFTPH / 0, 36, 36,  2,  2 /
      DATA MPFTSE / 0,  0,  0,  0,  0 /
      DATA LOWRUN,HIGRUN/ 999999998, 999999999/
      DATA TZELEC,TZELEC_SIG,TZABS /3*0.0/
      DATA VELT_PLU,VELT_MIN,VELP_PLU,VELP_MIN /33.0,-33.0,33.0,-33.0/
      DATA VELDL / 2.2 /
      DATA QUADTYPE /
     &  2,   1,   2,   1,   2,   1,   2,   1,
     &  2,   1,   2,   1,   2,   1,   2,   1 /
      DATA DELLEN /
     &  328.320, 447.396, 566.471, 784.149, 828.802, 553.415,
     &  210.000, 328.320, 447.396, 566.471, 784.149, 553.415/
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL UCTOH('FTHF',MPFTHF(1),4,4)
        CALL UCTOH('FTTH',MPFTTH(1),4,4)
        CALL UCTOH('FTQD',MPFTQD(1),4,4)
        CALL UCTOH('FTPH',MPFTPH(1),4,4)
        CALL UCTOH('FTSE',MPFTSE(1),4,4)
      ENDIF
C
C  Create the permanent link area FDCPRM, if not already done.
C
      CALL FSPLNK
C
C  Make the top level Times bank
C
      IF(LSFDC.EQ.0) CALL BKSFDC(LBANK)
      CALL MZFORM( 'FTSE', '6I -F', MPFTSE(5) )
      IF ( LFTMH .LE. 0 ) LFTMH = LC( LSFDC-IZFTMH )
      LKFTMH = LFTMH
      IF ( LKFTMH .LE. 0 ) CALL BKFTMH(LKFTMH)
      IC(LFTMH-5) = 1
C
      DO 10 HALF = 0, 1
        CALL MZLIFT( IDVSTP, LFTHF(HALF), LKFTMH, -(HALF+1), MPFTHF, -1)
        IC( LFTHF(HALF)-5 ) = HALF
        IC( LFTHF(HALF)+1 ) = LOWRUN
        IC( LFTHF(HALF)+2 ) = HIGRUN
C
C  Make the Theta Times banks
C
        UNIT = 0
        CALL MZLIFT( IDVSTP, LFTUN(HALF,UNIT), LFTHF(HALF),
     &                 -(UNIT+1), MPFTTH, -1)
        IC( LFTUN(HALF,UNIT)-5 ) = UNIT
        IC( LFTUN(HALF,UNIT)+1 ) = LOWRUN
        IC( LFTUN(HALF,UNIT)+2 ) = HIGRUN
        DO 30 QUAD = 0, 7
          CALL MZLIFT( IDVSTP, LFTQU(HALF,QUAD),
     &                   LFTUN(HALF,UNIT), -(QUAD+1), MPFTQD,-1)
          IC( LFTQU(HALF,QUAD)-5 ) = QUAD
          IC( LFTQU(HALF,QUAD)+1 ) = LOWRUN
          IC( LFTQU(HALF,QUAD)+2 ) = HIGRUN
          DO 40 SECTOR = 0, 5
            MPFTSE(4) =  6 + NUMTWR*NPARWR + 2*NUMTDL*NPARDL
            CALL MZLIFT( IDVSTP, LFTSE(HALF,UNIT,QUAD,SECTOR),
     &               LFTQU(HALF,QUAD), -(SECTOR+1), MPFTSE, -1)
            IC( LFTSE(HALF,UNIT,QUAD,SECTOR)-5 ) = SECTOR
            IC( LFTSE(HALF,UNIT,QUAD,SECTOR)+1 ) = LOWRUN
            IC( LFTSE(HALF,UNIT,QUAD,SECTOR)+2 ) = HIGRUN
            IC( LFTSE(HALF,UNIT,QUAD,SECTOR)+3 ) = NUMTWR
            IC( LFTSE(HALF,UNIT,QUAD,SECTOR)+4 ) = NPARWR
            IC( LFTSE(HALF,UNIT,QUAD,SECTOR)+5 ) = NUMTDL
            IC( LFTSE(HALF,UNIT,QUAD,SECTOR)+6 ) = NPARDL
            LKFTSE = LFTSE(HALF,UNIT,QUAD,SECTOR) 
            LKFTSE = LKFTSE + 6
            DO WIRE = 0, NUMTWR-1
              C( LKFTSE+1 ) = TZELEC
              C( LKFTSE+2 ) = TZABS
              C( LKFTSE+3 ) = VELT_PLU
              C( LKFTSE+4 ) = VELT_MIN
              LKFTSE = LKFTSE + NPARWR
            ENDDO
            DO DELAY = 0, 2*NUMTDL-1
              C( LKFTSE+1 ) = TZELEC
              C( LKFTSE+2 ) = TZABS
              C( LKFTSE+3 ) = VELDL
              C( LKFTSE+4 ) = DELLEN(SECTOR,QUADTYPE(QUAD,HALF))
              LKFTSE = LKFTSE + NPARDL
            ENDDO
   40     CONTINUE
   30   CONTINUE
C
C  Make the Phi Times banks
C
        UNIT = 1
        QUAD = 0
        CALL MZLIFT( IDVSTP, LFTUN(HALF,UNIT), LFTHF(HALF),
     &                 -(UNIT+1), MPFTPH, -1)
        IC( LFTUN(HALF,UNIT)-5 ) = UNIT
        IC( LFTUN(HALF,UNIT)+1 ) = LOWRUN
        IC( LFTUN(HALF,UNIT)+2 ) = HIGRUN
        DO 50 SECTOR = 0, 35
          MPFTSE(4) =  6 + NUMPWR*NPARWR 
          CALL MZLIFT( IDVSTP, LFTSE(HALF,UNIT,QUAD,SECTOR),
     &                   LFTUN(HALF,UNIT), -(SECTOR+1), MPFTSE, -1)
          IC( LFTSE(HALF,UNIT,QUAD,SECTOR)-5 ) = SECTOR
          IC( LFTSE(HALF,UNIT,QUAD,SECTOR)+1 ) = LOWRUN
          IC( LFTSE(HALF,UNIT,QUAD,SECTOR)+2 ) = HIGRUN
          IC( LFTSE(HALF,UNIT,QUAD,SECTOR)+3 ) = NUMPWR
          IC( LFTSE(HALF,UNIT,QUAD,SECTOR)+4 ) = NPARWR
          IC( LFTSE(HALF,UNIT,QUAD,SECTOR)+5 ) = NUMPDL
          IC( LFTSE(HALF,UNIT,QUAD,SECTOR)+6 ) = NPARDL
          LKFTSE = LFTSE(HALF,UNIT,QUAD,SECTOR)
          LKFTSE = LKFTSE + 6
          DO WIRE = 0, NUMPWR-1
            C( LKFTSE+1 ) = TZELEC
            C( LKFTSE+2 ) = TZABS
            C( LKFTSE+3 ) = VELP_PLU
            C( LKFTSE+4 ) = VELP_MIN
            LKFTSE = LKFTSE + NPARWR
          ENDDO
   50   CONTINUE
        CONTINUE
   10 CONTINUE
C------------------------------------------------------------------------
  999 RETURN
      END
