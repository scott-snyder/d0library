      SUBROUTINE BLFGNH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Build the gain banks
C-
C-   Inputs  : None, but ZEBRA structure up to SFDC should exist.
C-   Outputs :
C-
C-   Created  24-JUN-1988   Jeffrey Bantly
C-   Updated  19-FEB-1990   Jeffrey Bantly  convert to logical system
C-   Updated  20-MAR-1991   Jeffrey Bantly  put in Monte Carlo defaults 
C-   Updated   2-JUL-1991   Jeffrey Bantly  shift gains so electron = 1 MIP
C-                                          instead of a pion 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FDCPRM.INC'
      INCLUDE 'D0$LINKS:IZFGNH.LINK'
      INTEGER MPFGHF(5), MPFGTH(5)
      INTEGER MPFGQD(5), MPFGPH(5), MPFGSE(5)
      INTEGER LOWRUN, HIGRUN, NUMTWR, NUMPWR
      INTEGER NPARWR, NPARDL, NUMTDL, NUMPDL
      INTEGER HALF, UNIT, QUAD, SECTOR
      INTEGER WIRE, DELAY
      INTEGER GZSFDC, LBANK, LKFGNH, LKFGSE
      PARAMETER( NUMTWR = 8 )
      PARAMETER( NUMTDL = 1 )
      PARAMETER( NUMPWR = 16 )
      PARAMETER( NUMPDL = 0 )
      PARAMETER( NPARWR = 2 )
      PARAMETER( NPARDL = 2 )
      REAL GAINT, GAIND, GAINP, CNMIPT, CNMIPD, CNMIPP
      DATA MPFGHF / 4HFGHF,  2,  2,  2,  3 /
      DATA MPFGTH / 4HFGTH,  8,  8,  2,  3 /
      DATA MPFGQD / 4HFGQD,  6,  6,  2,  3 /
      DATA MPFGPH / 4HFGPH, 36, 36,  2,  3 /
      DATA MPFGSE / 4HFGSE,  0,  0,  0,  0 /
      DATA LOWRUN,HIGRUN / 999999998, 999999999/
      DATA GAINT,GAIND,GAINP/3*1.0/
      DATA CNMIPT,CNMIPD,CNMIPP /0.00159, 0.0, 0.00139/
C----------------------------------------------------------------------
C
C   Create a permanent link area FDCPRM, if not already done.
C
      CALL FSPLNK
C
C  Make top level Gains bank FGNH
C
      IF(LSFDC.EQ.0) CALL BKSFDC(LBANK)
      CALL MZFORM( 'FGSE', '6I -F', MPFGSE(5) )
      IF ( LFGNH .LE. 0 ) LFGNH = LC( LSFDC-IZFGNH )
      LKFGNH = LFGNH
      IF ( LKFGNH .LE. 0 ) CALL BKFGNH(LKFGNH)
C
      DO 10 HALF = 0, 1
        CALL MZLIFT( IDVSTP, LFGHF(HALF), LKFGNH, -(HALF+1), MPFGHF, -1)
        IC( LFGHF(HALF)-5 ) = HALF
        C( LFGHF(HALF)+1 ) = 0.0     
        C( LFGHF(HALF)+2 ) = 0.0     
C
C  Make the Theta gains banks
C
        UNIT = 0
        CALL MZLIFT( IDVSTP, LFGUN(HALF,UNIT), LFGHF(HALF),
     &                 -(UNIT+1), MPFGTH, -1)
        IC( LFGUN(HALF,UNIT)-5 ) = UNIT
        C( LFGUN(HALF,UNIT)+1 ) = 0.0     
        C( LFGUN(HALF,UNIT)+2 ) = 0.0     
        DO 30 QUAD = 0, 7
          CALL MZLIFT( IDVSTP, LFGQD(HALF,QUAD),
     &                   LFGUN(HALF,UNIT), -(QUAD+1), MPFGQD,-1)
          IC( LFGQD(HALF,QUAD)-5 ) = QUAD
          C( LFGQD(HALF,QUAD)+1 ) = 0.0     
          C( LFGQD(HALF,QUAD)+2 ) = 0.0     
          DO 40 SECTOR = 0, 5
            MPFGSE(4) =  6 + NUMTWR*NPARWR + 2*NUMTDL*NPARDL
            CALL MZLIFT( IDVSTP, LFGSE(HALF,UNIT,QUAD,SECTOR),
     &          LFGQD(HALF,QUAD), -(SECTOR+1), MPFGSE, -1)
            IC( LFGSE(HALF,UNIT,QUAD,SECTOR)-5 ) = SECTOR
            IC( LFGSE(HALF,UNIT,QUAD,SECTOR)+1 ) = LOWRUN
            IC( LFGSE(HALF,UNIT,QUAD,SECTOR)+2 ) = HIGRUN
            IC( LFGSE(HALF,UNIT,QUAD,SECTOR)+3 ) = NUMTWR
            IC( LFGSE(HALF,UNIT,QUAD,SECTOR)+4 ) = NPARWR
            IC( LFGSE(HALF,UNIT,QUAD,SECTOR)+5 ) = NUMTDL
            IC( LFGSE(HALF,UNIT,QUAD,SECTOR)+6 ) = NPARDL
            LKFGSE = LFGSE(HALF,UNIT,QUAD,SECTOR)
            LKFGSE = LKFGSE + 6
            DO WIRE = 0, NUMTWR-1
              C( LKFGSE+1 ) = GAINT
              C( LKFGSE+2 ) = CNMIPT
              LKFGSE = LKFGSE + NPARWR
            ENDDO
            DO DELAY = 0, 2*NUMTDL-1
              C( LKFGSE+1 ) = GAIND
              C( LKFGSE+2 ) = CNMIPD
              LKFGSE = LKFGSE + NPARDL
            ENDDO
   40     CONTINUE
   30   CONTINUE
C
C   Make the Phi gains banks
C
        UNIT = 1
        QUAD = 0
        CALL MZLIFT( IDVSTP, LFGUN(HALF,UNIT), LFGHF(HALF), -(UNIT+1),
     &              MPFGPH, -1)
        IC( LFGUN(HALF,UNIT)-5 ) = UNIT
        C( LFGUN(HALF,UNIT)+1 ) = 0.0     
        C( LFGUN(HALF,UNIT)+2 ) = 0.0     
        DO 50 SECTOR = 0, 35
          MPFGSE(4) =  6 + NUMPWR*NPARWR 
          CALL MZLIFT( IDVSTP, LFGSE(HALF,UNIT,QUAD,SECTOR),
     &             LFGUN(HALF,UNIT), -(SECTOR+1), MPFGSE, -1)
          IC( LFGSE(HALF,UNIT,QUAD,SECTOR)-5 ) = SECTOR
          IC( LFGSE(HALF,UNIT,QUAD,SECTOR)+1 ) = LOWRUN
          IC( LFGSE(HALF,UNIT,QUAD,SECTOR)+2 ) = HIGRUN
          IC( LFGSE(HALF,UNIT,QUAD,SECTOR)+3 ) = NUMPWR
          IC( LFGSE(HALF,UNIT,QUAD,SECTOR)+4 ) = NPARWR
          IC( LFGSE(HALF,UNIT,QUAD,SECTOR)+5 ) = NUMPDL
          IC( LFGSE(HALF,UNIT,QUAD,SECTOR)+6 ) = NPARDL
          LKFGSE = LFGSE(HALF,UNIT,QUAD,SECTOR)
          LKFGSE = LKFGSE + 6
          DO WIRE = 0, NUMPWR-1
            C( LKFGSE+1 ) = GAINP 
            C( LKFGSE+2 ) = CNMIPP
            LKFGSE = LKFGSE + NPARWR
          ENDDO
   50   CONTINUE
        CONTINUE
   10 CONTINUE
C-----------------------------------------------------------------------
  999 RETURN
      END
