      SUBROUTINE BLDALH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Build the Alignement banks
C-
C-   Inputs  : None, but ZEBRA structure up to SCDC should exist.
C-   Outputs :
C-
C-   Created  17-FEB-1988   Olivier Callot
C-   Updated  27-MAR-1992   Qizhong Li-Demarteau  added run limits 
C-   Updated  17-AUG-1992   Kim Kwee NG      For UNIX compatibility 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZDALH.LINK'
      INTEGER MPDALH(5), MPDALL(5), MPDALS(5)
      INTEGER LOWRUN, HIGRUN, NWWIRE, NBWIRE
      INTEGER LAY, SEC, WIR, IP, LDRFT, LDALL, LDALS
      PARAMETER( NWWIRE = 7 )
      PARAMETER( NBWIRE = 7 )
      DATA    MPDALH / 0, 4,  4,  2,  2 /
      DATA    MPDALL / 0, 32, 32,  2,  2 /
      DATA    MPDALS / 0, 0,  0,  0,  0 /
      REAL    RAY( 0:6, 0:3 ), STAG( 0:6 ), PHI
      CHARACTER*4 CHAR4,CHAS4,CHAT4
      EQUIVALENCE (CHAR4,MPDALH(1))
      EQUIVALENCE (CHAS4,MPDALL(1))
      EQUIVALENCE (CHAT4,MPDALS(1))
      DATA    CHAR4 /'DALH'/
      DATA    CHAS4 /'DALL'/
      DATA    CHAT4 /'DALS'/
      DATA LOWRUN,HIGRUN/0, 999999/
C----------------------------------------------------------------------
C
C ****  Build RAY and STAG from DRFT bank
C
      LDRFT = LC( LDGEH-3 )
      CALL UCOPY( C( LDRFT+26), STAG(0), 7)
      DO 60 LAY = 0, 3
        DO 70 WIR = 0, 6
          RAY( WIR, LAY ) = C( LDRFT+11+2*LAY ) + C( LDRFT+19+WIR )
   70   CONTINUE
   60 CONTINUE
C
      CALL MZFORM( 'DALS', '2I 2F 2I -F', MPDALS(5) )
      MPDALS(4) = 6 + NBWIRE*NWWIRE
      IF ( LC( LSCDC-IZDALH) .EQ. 0 ) THEN
        CALL MZLIFT( IDVSTP, LDALH, LSCDC, -IZDALH, MPDALH, -1 )
        IC( LDALH+1 ) = LOWRUN
        IC( LDALH+2 ) = HIGRUN
      ENDIF
C
C ****  Book the layer banks
C
      DO 10 LAY = 0, 3
        CALL MZLIFT( IDVSTP, LDALL, LDALH, -(LAY+1), MPDALL, -1)
        IC( LDALL-5 ) = LAY
        IC( LDALL+1 ) = LOWRUN
        IC( LDALL+2 ) = HIGRUN
        PHI = ( C(LDRFT+12+2*LAY) +90. ) * ACOS(-1.) / 180.
        DO 20 SEC = 0, 31
          CALL MZLIFT( IDVSTP, LDALS, LDALL, -(SEC+1), MPDALS, -1)
          IC( LDALS-5 ) = SEC
          IC( LDALS+1 ) = LOWRUN
          IC( LDALS+2 ) = HIGRUN
          C ( LDALS+3 ) = COS( PHI )
          C ( LDALS+4 ) = SIN( PHI )
          IC( LDALS+5 ) = NWWIRE
          IC( LDALS+6 ) = NBWIRE
          DO 50 WIR = 0, NBWIRE-1
            IP = LDALS + WIR * NWWIRE + 6
            C( IP+1 ) =   RAY(WIR,LAY) * SIN(PHI) + STAG(WIR) * COS(PHI)
            C( IP+2 ) = - RAY(WIR,LAY) * COS(PHI) + STAG(WIR) * SIN(PHI)
            C( IP+3 ) = 0.
            C( IP+4 ) = 0.
            C( IP+5 ) = 0.
            C( IP+6 ) = 0.
            C( IP+7 ) = 0.
   50     CONTINUE
          PHI = PHI + ACOS(-1.) / 16.
   40     CONTINUE
   20   CONTINUE
   10 CONTINUE
  999 RETURN
      END
