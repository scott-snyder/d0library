      SUBROUTINE BLDALH_0
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Build the 0th order Alignement banks using
C-                         survey results
C-
C-   Inputs  : None, but ZEBRA structure up to SCDC should exist.
C-   Outputs : none
C-
C-   Created   9-APR-1991   Qizhong Li-Demarteau  
C-   Updated  11-DEC-1992   Qizhong Li-Demarteau   changed transformation
C-                                      matrix because D0 uses CDC as the 
C-                                      coordinate system
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZDALH.LINK'
      INTEGER MPDALH(5), MPDALL(5), MPDALS(5)
      INTEGER LOWRUN, HIGRUN, NWWIRE, NBWIRE
      INTEGER LAY, SEC, WIR, IP, LDRFT, LDALL, LDALS, IER
      PARAMETER( NWWIRE = 7 )
      PARAMETER( NBWIRE = 7 )
      REAL    RAY( 0:6, 0:3 ), STAG( 0:6 ), PHI
      REAL    MATRIX(3,3), VECTOR(3), X0, Y0, Z0, COSPHI, SINPHI
      REAL    X00, Y00, Z00, X01, Y01
      LOGICAL FIRST, EZERROR
      DATA    MPDALH / 4HDALH,  4,  4,  2,  2 /
      DATA    MPDALL / 4HDALL, 32, 32,  2,  2 /
      DATA    MPDALS / 4HDALS,  0,  0,  0,  0 /
C      DATA    MATRIX/0.9952,-0.09797,0.0002291,0.09797,0.9952,
C     &               -0.0002292, -0.0002056,0.0002505,1.0/
C      DATA VECTOR/0.00594,0.01382,-0.00673/
      DATA LOWRUN,HIGRUN/0, 999999/
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('CDCSTP_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('CDCSTP','BLDALH_0',
     &    'Unable to find bank CDCSTP_RCP','F')
          GOTO 999
        ENDIF
C        CALL EZGET('MATRIX(1)',MATRIX(1,1),IER)
        CALL EZGET('MATRIX',MATRIX,IER)
        CALL EZGET('VECTOR(1)',VECTOR(1),IER)
        CALL EZRSET
      ENDIF
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
      IF (LSCDC .GT. 0) THEN
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
          IC( LDALS+5 ) = NWWIRE
          IC( LDALS+6 ) = NBWIRE
          DO 50 WIR = 0, NBWIRE-1
            IP = LDALS + WIR * NWWIRE + 6
            x0 =   RAY(WIR,LAY) * SIN(PHI) + STAG(WIR) * COS(PHI)
            y0 = - RAY(WIR,LAY) * COS(PHI) + STAG(WIR) * SIN(PHI)
            z0 = 0.
            C( IP+1 ) = MATRIX(1,1)*X0 + MATRIX(1,2)*Y0 + MATRIX(1,3)*Z0
     &                  + VECTOR(1)
            C( IP+2 ) = MATRIX(2,1)*X0 + MATRIX(2,2)*Y0 + MATRIX(2,3)*Z0
     &                  + VECTOR(2)
            C( IP+3 ) = MATRIX(3,1)*X0 + MATRIX(3,2)*Y0 + MATRIX(3,3)*Z0
     &                  + VECTOR(3)
            C( IP+4 ) = 0.
            C( IP+5 ) = 0.
            C( IP+6 ) = 0.
            C( IP+7 ) = 0.
   50     CONTINUE
          X00 =   RAY(6,LAY) * SIN(PHI) + (1.0+STAG(6)) * COS(PHI)
          Y00 = - RAY(6,LAY) * COS(PHI) + (1.0+STAG(6)) * SIN(PHI)
          X01 = MATRIX(1,1)*X00 + MATRIX(1,2)*Y00 + MATRIX(1,3)*Z00
     &                  + VECTOR(1)
          Y01 = MATRIX(2,1)*X00 + MATRIX(2,2)*Y00 + MATRIX(2,3)*Z00
     &                  + VECTOR(2)
          SINPHI = (Y01 - C(IP+2)) / 1.0
          COSPHI = (X01 - C(IP+1)) / 1.0
          IF (SINPHI .GT. 1.0) SINPHI = 1.0
          IF (COSPHI .GT. 1.0) COSPHI = 1.0
          C(LDALS+3) = COSPHI
          C(LDALS+4) = SINPHI
          PHI = PHI + ACOS(-1.) / 16.
   40     CONTINUE
   20   CONTINUE
   10 CONTINUE
C
  999 RETURN
      END
