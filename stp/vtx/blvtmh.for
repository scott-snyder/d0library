      SUBROUTINE BLVTMH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Build the time_to_position banks
C-
C-   Inputs  : None, but ZEBRA structure up to SVTX should exist.
C-   Outputs : 
C-
C-   Created  29-SEP-1988   Ghita Rahal_callot
C-   Updated  10-NOV-1988   Peter Grudberg    :include z-strip banks
C-   Updated  19-MAR-1991   Peter Grudberg  use BKVTMH
C-   Updated   2-AUG-1992   Peter Grudberg  remove strips
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVTMH.LINK'
      INCLUDE 'D0$LINKS:IZVGEH.LINK'
      INCLUDE 'D0$LINKS:IZVRFT.LINK'
      INTEGER MPVTMW(5)
      INTEGER LOWRUN, HIGRUN, NBSEC, NWWIRE, NBWIRE, NLAY
      INTEGER LAY, SEC, ADC, LVRFT, LVTMW, IP, NWLAYR
      DATA LOWRUN, HIGRUN / 0, 999999/
      PARAMETER( NWWIRE = 2 )
      REAL    TZWIR, VELWIR
      PARAMETER( TZWIR  = 200. )
C
C ****  velocity=cm/ns  ; t0 (tzwir) = ns
C ****  velocity along strip (VELSTR) taken to be 2/3 c
C
      PARAMETER( VELWIR = .00084)
      DATA    MPVTMW / 4HVTMW, 0, 0, 0, 0 /
C----------------------------------------------------------------------
      LVGEH = LC ( LSVTX - IZVGEH )
      LVRFT = LC ( LVGEH - IZVRFT )
      CALL MZFORM( 'VTMW', '5I -F', MPVTMW(5) )
      IF ( LVTMH .EQ. 0 ) THEN
        CALL BKVTMH( LVTMH )
      ENDIF
C
C ****  Book the layer banks
C
      NLAY = IC ( LVRFT + 1 )
      DO 10 LAY = 0, NLAY-1
        NBSEC  = IC ( LVRFT + 2 + 7*LAY )
        NBWIRE = IC ( LVRFT + 3 + 7*LAY )
        MPVTMW(4) = 5 + NBSEC*NBWIRE*NWWIRE
        CALL MZLIFT( IDVSTP, LVTMW, LVTMH, -(LAY+1), MPVTMW, -1)
        IC( LVTMW-5 ) = LAY
        IC( LVTMW+1 ) = LOWRUN
        IC( LVTMW+2 ) = HIGRUN
        IC( LVTMW+3 ) = NWWIRE
        IC( LVTMW+4 ) = NBWIRE
        IC( LVTMW+5 ) = NBSEC
        DO 20 SEC = 0, NBSEC-1
          DO 30 ADC = 0, NBWIRE-1
            IP = LVTMW + ( SEC*NBWIRE + ADC ) * NWWIRE + 5
            C( IP+1 ) = TZWIR
            C( IP+2 ) = VELWIR
   30     CONTINUE
   20   CONTINUE
   10 CONTINUE
C
  999 RETURN
      END
