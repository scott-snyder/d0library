      SUBROUTINE BLFPDH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Build the pedestal banks
C-
C-   Inputs  : None, but ZEBRA structure up to SFDC should exist.
C-   Outputs : 
C-
C-   Created   5-AUG-1988   Jeffrey Bantly
C-   Updated  19-FEB-1990   Jeffrey Bantly  convert to logical system
C-   Updated  20-MAR-1991   Jeffrey Bantly  put in Monte Carlo defaults 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FDCPRM.INC'
      INCLUDE 'D0$LINKS:IZFPDH.LINK'
      INTEGER MPFPHF(5), MPFPTH(5)
      INTEGER MPFPQD(5), MPFPPH(5), MPFPSE(5)
      INTEGER LOWRUN, HIGRUN, NUMTWR, NUMPWR
      INTEGER NPARWR, NPARDL, NUMTDL, NUMPDL
      INTEGER HALF, UNIT, QUAD, SECTOR
      INTEGER WIRE, DELAY
      INTEGER GZSFDC, LBANK, LKFPDH, LKFPSE
      PARAMETER( NUMTWR = 8 )
      PARAMETER( NUMTDL = 1 )
      PARAMETER( NUMPWR = 16 )
      PARAMETER( NUMPDL = 0 )
      REAL TEPEDT,LEPEDT, TEPEDD, LEPEDD, TEPEDP, LEPEDP 
      DATA MPFPHF / 4HFPHF,  2,  2,  2,  2 /
      DATA MPFPTH / 4HFPTH,  8,  8,  2,  2 /
      DATA MPFPQD / 4HFPQD,  6,  6,  2,  2 /
      DATA MPFPPH / 4HFPPH, 36, 36,  2,  2 /
      DATA MPFPSE / 4HFPSE,  0,  0,  0,  0 /
      DATA LOWRUN,HIGRUN,NPARWR,NPARDL / 999999998, 999999999, 2, 2/
      DATA LEPEDT,LEPEDD,LEPEDP /3*20.0/
      DATA TEPEDT,TEPEDD,TEPEDP /3*1.5/
C----------------------------------------------------------------------
C
C  Create the permanent link area FDCPRM, if not already done.
C
      CALL FSPLNK
C
C  Make the top level FDC Pedestal Bank
C
      IF ( LSFDC.LE.0 ) CALL BKSFDC(LBANK)
      CALL MZFORM( 'FPSE', '6I -F', MPFPSE(5) ) ! names will be same
      IF ( LFPDH .LE. 0 ) LFPDH = LC( LSFDC-IZFPDH )
      LKFPDH = LFPDH
      IF ( LKFPDH .LE. 0 ) CALL BKFPDH(LKFPDH)
C
      DO 10 HALF = 0, 1
        CALL MZLIFT( IDVSTP, LFPHF(HALF), LKFPDH, -(HALF+1), MPFPHF, -1)
        IC( LFPHF(HALF)-5 ) = HALF
        IC( LFPHF(HALF)+1 ) = LOWRUN
        IC( LFPHF(HALF)+2 ) = HIGRUN
C
C  Make the Theta Pedestal Banks
C
        UNIT = 0
        CALL MZLIFT( IDVSTP, LFPUN(HALF,UNIT), LFPHF(HALF),
     &                 -(UNIT+1), MPFPTH, -1)
        IC( LFPUN(HALF,UNIT)-5 ) = UNIT
        IC( LFPUN(HALF,UNIT)+1 ) = LOWRUN
        IC( LFPUN(HALF,UNIT)+2 ) = HIGRUN
        DO 30 QUAD = 0, 7
          CALL MZLIFT( IDVSTP, LFPQD(HALF,QUAD), LFPUN(HALF,
     &        UNIT), -(QUAD+1), MPFPQD,-1)
          IC( LFPQD(HALF,QUAD)-5 ) = QUAD
          IC( LFPQD(HALF,QUAD)+1 ) = LOWRUN
          IC( LFPQD(HALF,QUAD)+2 ) = HIGRUN
          DO 40 SECTOR = 0, 5
            MPFPSE(4) =  26
            CALL MZLIFT( IDVSTP, LFPSE(HALF,UNIT,QUAD,SECTOR),
     &          LFPQD(HALF,QUAD), -(SECTOR+1), MPFPSE, -1)
            IC( LFPSE(HALF,UNIT,QUAD,SECTOR)-5 ) = SECTOR
            IC( LFPSE(HALF,UNIT,QUAD,SECTOR)+1 ) = LOWRUN
            IC( LFPSE(HALF,UNIT,QUAD,SECTOR)+2 ) = HIGRUN
            IC( LFPSE(HALF,UNIT,QUAD,SECTOR)+3 ) = NUMTWR
            IC( LFPSE(HALF,UNIT,QUAD,SECTOR)+4 ) = NPARWR
            IC( LFPSE(HALF,UNIT,QUAD,SECTOR)+5 ) = NUMTDL
            IC( LFPSE(HALF,UNIT,QUAD,SECTOR)+6 ) = NPARDL
            LKFPSE = LFPSE(HALF,UNIT,QUAD,SECTOR)
            LKFPSE = LKFPSE + 6
            DO WIRE = 0, NUMTWR-1
              C( LKFPSE+1 ) = LEPEDT 
              C( LKFPSE+2 ) = TEPEDT
              LKFPSE = LKFPSE + NPARWR
            ENDDO
            DO DELAY = 0, 2*NUMTDL-1
              C( LKFPSE+1 ) = LEPEDD 
              C( LKFPSE+2 ) = TEPEDD 
              LKFPSE = LKFPSE + NPARDL
            ENDDO
   40     CONTINUE
   30   CONTINUE
C
C  Make the Phi Pedestal Banks
C
        UNIT = 1
        QUAD = 0
        CALL MZLIFT( IDVSTP, LFPUN(HALF,UNIT), LFPHF(HALF), -(UNIT+1),
     &              MPFPPH, -1)
        IC( LFPUN(HALF,UNIT)-5 ) = UNIT
        IC( LFPUN(HALF,UNIT)+1 ) = LOWRUN
        IC( LFPUN(HALF,UNIT)+2 ) = HIGRUN
        DO 50 SECTOR = 0, 35
          MPFPSE(4) =  38
          CALL MZLIFT( IDVSTP, LFPSE(HALF,UNIT,QUAD,SECTOR),
     &             LFPUN(HALF,UNIT), -(SECTOR+1), MPFPSE, -1)
          IC( LFPSE(HALF,UNIT,QUAD,SECTOR)-5 ) = SECTOR
          IC( LFPSE(HALF,UNIT,QUAD,SECTOR)+1 ) = LOWRUN
          IC( LFPSE(HALF,UNIT,QUAD,SECTOR)+2 ) = HIGRUN
          IC( LFPSE(HALF,UNIT,QUAD,SECTOR)+3 ) = NUMPWR
          IC( LFPSE(HALF,UNIT,QUAD,SECTOR)+4 ) = NPARWR
          IC( LFPSE(HALF,UNIT,QUAD,SECTOR)+5 ) = NUMPDL
          IC( LFPSE(HALF,UNIT,QUAD,SECTOR)+6 ) = NPARDL
          LKFPSE = LFPSE(HALF,UNIT,QUAD,SECTOR)
          LKFPSE = LKFPSE + 6
          DO WIRE = 0, NUMPWR-1
            C( LKFPSE+1 ) = LEPEDP 
            C( LKFPSE+2 ) = TEPEDP 
            LKFPSE = LKFPSE + NPARWR
          ENDDO
   50   CONTINUE
   10 CONTINUE
C------------------------------------------------------------------------
  999 RETURN
      END
