      SUBROUTINE BLVPDH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Build the pedestal banks
C-
C-   Inputs  : None, but ZEBRA structure up to SVTX should exist.
C-   Outputs :
C-
C-   Created  17-FEB-1988   Olivier Callot
C-   Updated  30-SEP-1988   Ghita Rahal-Callot  : adapted for VTX
C-   Updated  10-NOV-1988   Peter Grudberg      : added z-strip banks
C-   Updated  19-MAR-1991   Peter Grudberg  use BKVPDH
C-   Updated   2-AUG-1992   Peter Grudberg   Remove strips
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVPDH.LINK'
      INCLUDE 'D0$LINKS:IZVRFT.LINK'
      INTEGER MPVPDL(5), LOWRUN, HIGRUN, NWFADC, NBFADC
      INTEGER LAY, SEC, ADC, IP, LVPDL, LVRFT, MAXLAY, SECMAX
      PARAMETER( NWFADC = 2 )
      PARAMETER( NBFADC = 16)
      REAL    RMEAN, SIGMA
      PARAMETER( RMEAN = 20.)
      PARAMETER( SIGMA = 1.5)
      DATA    MPVPDL / 4HVPDL, 0, 0, 0, 0 /
      DATA LOWRUN, HIGRUN / 0, 999999/
C----------------------------------------------------------------------
      CALL MZFORM( 'VPDL', '5I -F', MPVPDL(5) )
      IF ( LVPDH .EQ. 0 ) THEN
        CALL BKVPDH( LVPDH )
      ENDIF
C
C ****  Book the layer banks
C
      LVRFT = LC ( LVGEH - IZVRFT )
      MAXLAY = IC ( LVRFT + 1 )
      DO 10 LAY = 0, MAXLAY-1
        SECMAX = IC ( LVRFT + 2 + 7*LAY )
        MPVPDL(4) = 5 + SECMAX*NBFADC*NWFADC
        CALL MZLIFT( IDVSTP, LVPDL, LVPDH, -(LAY+1), MPVPDL, -1)
        IC( LVPDL-5 ) = LAY
        IC( LVPDL+1 ) = LOWRUN
        IC( LVPDL+2 ) = HIGRUN
        IC( LVPDL+3 ) = NWFADC
        IC( LVPDL+4 ) = NBFADC
        IC( LVPDL+5 ) = SECMAX
        DO 20 SEC = 0, SECMAX-1
          DO 30 ADC = 0, NBFADC-1
            IP = LVPDL + ( SEC*NBFADC + ADC ) * NWFADC + 5
            C( IP+1 ) = RMEAN
            C( IP+2 ) = SIGMA
   30     CONTINUE
   20   CONTINUE
   10 CONTINUE
C
  999 RETURN
      END
