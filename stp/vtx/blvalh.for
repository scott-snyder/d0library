      SUBROUTINE BLVALH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Build the Alignement banks ( similar to those of the
C-                         CDC)
C-
C-   Inputs  : None, but ZEBRA structure up to SVTX should exist.
C-   Outputs :
C-
C-   Created  15-SEP-1988   Ghita Rahal-Callot 
C-   Modified 09-NOV-1988   Peter Grudberg :Added banks for z-strips
C-   Modified 25-Sep-1992   P. Grudberg Remove z-strip banks!
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVALH.LINK'
      INCLUDE 'D0$LINKS:IZVRFT.LINK'
      INCLUDE 'D0$LINKS:IZVZST.LINK'
      INTEGER MPVALH(5), MPVALL(5), MPVALS(5)
      INTEGER LOWRUN, HIGRUN, NWWIRE, NBWIRE, NWLAYR
      INTEGER LAY, SEC, WIR, IP, LVRFT, LVALH, LVALL, LVALS
      INTEGER MAXL
      PARAMETER( MAXL = 3 )
      PARAMETER( NWWIRE = 7 )
      PARAMETER( NBWIRE = 8 )
      INTEGER MAXS(0:MAXL-1)
      DATA LOWRUN, HIGRUN / 0, 999999/
      DATA    MAXS / 16, 32, 32 /
      DATA    MPVALH / 4HVALH,  9,  9,  2,  2 /
      DATA    MPVALL / 4HVALL, 32, 32,  2,  2 /
      DATA    MPVALS / 4HVALS,  0,  0,  0,  0 /
      REAL    RAY( 0:NBWIRE-1, 0:MAXL-1 ), STAG( 0:NBWIRE-1 ), PHI
C----------------------------------------------------------------------
C
C ****  Build RAY and STAG from VRFT bank
C
      LVRFT = LC( LVGEH-IZVRFT)
      CALL UCOPY( C( LVRFT+31), STAG(0), NBWIRE)
      DO 60 LAY = 0, MAXL-1
        DO 70 WIR = 0, NBWIRE-1
          RAY( WIR, LAY ) = C( LVRFT+7+7*LAY ) + C( LVRFT+23+WIR )
   70   CONTINUE
   60 CONTINUE
C
      CALL MZFORM( 'VALH', '2I', MPVALH(5) )
      CALL MZFORM( 'VALL', '2I', MPVALL(5) )
      CALL MZFORM( 'VALS', '2I 2F 2I -F', MPVALS(5) )
      MPVALS(4) = 6 + NBWIRE*NWWIRE
      IF ( LC( LSVTX-IZVALH) .EQ. 0 ) THEN
        CALL MZLIFT( IDVSTP, LVALH, LSVTX, -IZVALH, MPVALH, -1 )
        IC( LVALH+1 ) = LOWRUN
        IC( LVALH+2 ) = HIGRUN
      ENDIF
C
C ****  Book the layer banks
C
      DO 10 LAY = 0, MAXL-1
        MPVALL(2) = MAXS(LAY)
        MPVALL(3) = MAXS(LAY)
        CALL MZLIFT( IDVSTP, LVALL, LVALH, -(LAY+1), MPVALL, -1)
        IC( LVALL-5 ) = LAY
        IC( LVALL+1 ) = LOWRUN
        IC( LVALL+2 ) = HIGRUN
        PHI = ( C(LVRFT+8+7*LAY) +90. ) * ACOS(-1.) / 180.
        DO 20 SEC = 0, MAXS(LAY)-1
          CALL MZLIFT( IDVSTP, LVALS, LVALL, -(SEC+1), MPVALS, -1)
          IC( LVALS-5 ) = LAY * 2**9 + SEC * 2**4
          IC( LVALS+1 ) = LOWRUN
          IC( LVALS+2 ) = HIGRUN
          C ( LVALS+3 ) = COS( PHI )
          C ( LVALS+4 ) = SIN( PHI )
          IC( LVALS+5 ) = NBWIRE
          IC( LVALS+6 ) = NWWIRE
          DO 50 WIR = 0, NBWIRE-1
            IP = LVALS + WIR * NWWIRE + 6
            C( IP+1 ) =   RAY(WIR,LAY) * SIN(PHI) + 
     &        (-1)**SEC * STAG(WIR) * COS(PHI)
            C( IP+2 ) = - RAY(WIR,LAY) * COS(PHI) + 
     &        (-1)**SEC * STAG(WIR) * SIN(PHI)
            C( IP+3 ) = 0.
            C( IP+4 ) = 0.
            C( IP+5 ) = 0.
            C( IP+6 ) = 0.
            C( IP+7 ) = 0.
   50     CONTINUE
          PHI = PHI + ACOS(-1.) * 2./MAXS(LAY)
   20   CONTINUE
   10 CONTINUE
  999 RETURN
      END
