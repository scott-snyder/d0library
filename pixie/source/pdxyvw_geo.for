      SUBROUTINE PDXYVW_GEO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : display X-Y view of the CDC geometry
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  27-JUL-1990   Qizhong Li-Demarteau  pick up from PDTRCK
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GRAPHF77.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CDPARA.INC'
      INCLUDE 'D0$INC:PXPARA.INC'
C
      INTEGER I, L, IS
      INTEGER LAY, SEC, IWIR, ISEG, GZDALS
      INTEGER IFDSEC, IFDWIR
      REAL    PHIW, CPHIW, SPHIW
      REAL    SIZDIS, DEGRAD
      REAL    RAYCEN, DELRAY, PHICEN, DELPHI
      REAL    X1, X2, Y1, Y2, R1, R2, PHI1, PHI2
      INTEGER LDRFT, LDALS, IPWIR
      REAL    XWIR, YWIR, TLSEG, XCENT, YCENT, XORI, YORI
      INTEGER NBWIR
      PARAMETER( DEGRAD = 3.1415926535/180.)
C
      DATA TLSEG / 13.0 /
      DATA SIZDIS / 0.2 /
C --------------------------------------------------------------------
C
      CALL PUGETV('CDC DRAW SECTORS', IFDSEC )
      CALL PUGETV('CDC DRAW WIRES', IFDWIR )
C
      DO 88 LAY = 0,3
C
C ****  Access DRFT bank for nominal geometry
C
        LDRFT = LC( LDGEH - 3 )
        RAYCEN = C( LDRFT+11+2*LAY )
        PHICEN = C( LDRFT+12+2*LAY ) * DEGRAD
        DELRAY = C( LDRFT+8 )
        DELPHI = C( LDRFT+9 ) * DEGRAD
        R1 = ( RAYCEN - DELRAY ) / COS( DELPHI )
        R2 = ( RAYCEN + DELRAY ) / COS( DELPHI )
        DO 89 IS = 0, 31
          SEC = IS
          IF( SEC .LT. 0  ) SEC = SEC + 32
          IF( SEC .GT. 31 ) SEC = SEC - 32
          CALL PUOPEN
          CALL PXCOLR('GRE')
          IF (IFDSEC .NE. 0) THEN
C
C ****  Draw the cell limits.
C
            PHI1 = PHICEN + (2*SEC-1) * DELPHI
            PHI2 = PHICEN + (2*SEC+1) * DELPHI
            X1 =  R1 * COS( PHI1 )
            Y1 =  R1 * SIN( PHI1 )
            CALL JMOVE( X1, Y1 )
            X2 =  R1 * COS( PHI2 )
            Y2 =  R1 * SIN( PHI2 )
            CALL JDRAW( X2, Y2 )
            X2 =  R2 * COS( PHI2 )
            Y2 =  R2 * SIN( PHI2 )
            IF (IFDSEC .LE. 1) THEN
              CALL JMOVE(X2,Y2)
            ELSE
              CALL JDRAW(X2,Y2)
            ENDIF
            X2 =  R2 * COS( PHI1 )
            Y2 =  R2 * SIN( PHI1 )
            CALL JDRAW( X2, Y2 )
            IF (IFDSEC .GT. 1) CALL JDRAW(X1,Y1)
          ENDIF
          CALL PXCOLR( 'MAG' )
          LDALS = GZDALS(LAY, SEC)
          CPHIW = C( LDALS+3 )
          SPHIW = C( LDALS+4 )
          DO 99 IWIR= 0,NBWIR-1
            IPWIR = LDALS + 6 + IC(LDALS+6) * IWIR
            XWIR = C( IPWIR+1 )
            YWIR = C( IPWIR+2 )
C
C  Mark wire positions with an "+"
C
            IF (IFDWIR .NE. 0) THEN
              CALL JCMARK(2)
              CALL JMARK( XWIR, YWIR )
            ENDIF
   99     CONTINUE
          CALL PUCLOSE
   89   CONTINUE
   88 CONTINUE
C
  999 RETURN
      END
