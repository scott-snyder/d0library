      SUBROUTINE PDCDC3D_GEO(SECMIN,SECMAX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make a 3D display of CDC geometry 
C-                         from SECMIN to SECMAX 
C-
C-   Inputs  : SECMIN, SECMAX
C-   Outputs : none
C-
C-   Created  01-FEB-1991   Qizhong Li-Demarteau
C-   Modified 24-JUN-1991   Nobuaki Oshima ( Adapts Combined 3D View. )
C-   Updated   9-JAN-1992   Qizhong Li-Demarteau  change sector color 
C-   Updated  22-OCT-1992   Qizhong Li-Demarteau  added draw CDC inner wall 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GRAPHF77.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CDPARA.INC'
      INCLUDE 'D0$INC:PXPARA.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER SECMIN, SECMAX, IS
      INTEGER I, L
      INTEGER NHITS(0:NBSENS-1), NWIR, LHIT
      INTEGER KPWIRE(0:NBSENS-1), KP
      INTEGER LAY, SEC, IWIR, ISEG, GZDTRH, LDTRH, GZDSEC, GZDALS
      INTEGER NTRK, IPHIT, WRFLAG, ITRK, GZDGEH
      INTEGER WIR, NUMHIT, ISIDE, HLABEL, J, PLDTTH, KPDSEC, NFADC
      INTEGER IFDSEC, IFDWIR, IFDHIT, IFDTRK, IFDLBL
      INTEGER IPATH, ERR
      CHARACTER*4 TRKNUM, DPATH
      EQUIVALENCE (IPATH, DPATH)
      LOGICAL TRKFLG, FIRST, NOZLIN, ALGNMT
      REAL    YR, EXTRAP, DISTAN, SLOP, VTXCUT
      REAL    PHIW, CPHIW, SPHIW
      REAL    DDIS1, DDIS
      REAL    DRANG1, DRANG2
      REAL    XHPOS, YHPOS, XTRKNM, YTRKNM, XNMOFF, YNMOFF
      PARAMETER( XNMOFF = 2.0 )
      PARAMETER( YNMOFF = 2.0 )
      REAL    SIZDIS
      REAL    RAYCEN, DELRAY, PHICEN, DELPHI
      REAL    X1, X2, Y1, Y2, R1, R2, PHI1, PHI2
      REAL    X3, Y3, X4, Y4, RWALL, RWALL1
      REAL    ZHPOS, RTRKHI, RTRKLO, Z1, Z2, TLTRK
      INTEGER LDRFT, LDALS, IPWIR
      REAL    XWIR, YWIR, TLSEG, XCENT, YCENT, XORI, YORI
      INTEGER NBWIR, PLDALS
      REAL    PCPHIW
C
      DATA TLSEG / 13.0 /
      DATA SIZDIS / 0.2 /
      DATA FIRST/.TRUE./, ALGNMT/.FALSE./
C --------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        PLDALS = GZDALS(0,0)
        PCPHIW = C(PLDALS+3)
        IF (ABS(PCPHIW) .LE. 0.001) ALGNMT = .TRUE.
      ENDIF
C-
      CALL PUGETV('CDC DRAW 3D SEC ', IFDSEC )
      CALL PUGETV('CDC DRAW WIRES  ', IFDWIR )
C
      IF (LDGEH .LE. 0) LDGEH = GZDGEH()
      RTRKHI = C(LDGEH + 12)
      RTRKLO = 0.0
      Z1 = C(LDGEH + 14)
      Z2 = -Z1
      RWALL = C(LDGEH + 12)
      RWALL1 = C(LDGEH + 10)
      CALL PUOPEN
      CALL PXCOLR('GRE')
      CALL JCIRCL(0., 0., Z1, RWALL, 0)
      CALL JCIRCL(0., 0., Z2, RWALL, 0)
      CALL JCIRCL(0., 0., Z1, RWALL1, 0)
      CALL JCIRCL(0., 0., Z2, RWALL1, 0)
      CALL J3MOVE(0., RWALL, Z1)
      CALL J3DRAW(0., RWALL, Z2)
      CALL J3MOVE(0.,-RWALL, Z1)
      CALL J3DRAW(0.,-RWALL, Z2)
      CALL J3MOVE(0., RWALL1, Z1)
      CALL J3DRAW(0., RWALL1, Z2)
      CALL J3MOVE(0.,-RWALL1, Z1)
      CALL J3DRAW(0.,-RWALL1, Z2)
      LDRFT = LC(LDGEH - 3)
C      IF ((SECMAX - SECMIN) .GT. 8) NOZLIN = .TRUE.
      CALL PXCOLR( 'BLU' )
      DO 88 LAY = 0,3
        RAYCEN = C( LDRFT+11+2*LAY )
        PHICEN = C( LDRFT+12+2*LAY ) * PI / 180.0
        DELRAY = C( LDRFT+8 )
        DELPHI = C( LDRFT+9 ) * PI / 180.0
        R1 = ( RAYCEN - DELRAY ) / COS( DELPHI )
        R2 = ( RAYCEN + DELRAY ) / COS( DELPHI )
        DO 89 IS = SECMIN, SECMAX
          SEC = IS
          IF( SEC .LT. 0  ) SEC = SEC + 32
          IF( SEC .GT. 31 ) SEC = SEC - 32
C-
          IF (IFDSEC .NE. 0) THEN
C
C ****  Draw the cell limits.
C
            PHI1 = PHICEN + (2*SEC-1) * DELPHI
            PHI2 = PHICEN + (2*SEC+1) * DELPHI
            X1 =  R1 * COS( PHI1 )
            Y1 =  R1 * SIN( PHI1 )
            IF (ALGNMT) CALL PDALGN(X1,Y1,Z1)
            CALL J3MOVE(X1,Y1,Z1)
            X2 =  R1 * COS( PHI2 )
            Y2 =  R1 * SIN( PHI2 )
            IF (ALGNMT) CALL PDALGN(X2,Y2,Z1)
            CALL J3DRAW(X2,Y2,Z1)
            X3 =  R2 * COS( PHI2 )
            Y3 =  R2 * SIN( PHI2 )
            IF (ALGNMT) CALL PDALGN(X3,Y3,Z1)
            IF (IFDSEC .LE. 1) THEN
              CALL J3MOVE(X3,Y3,Z1)
            ELSE
              CALL J3DRAW(X3,Y3,Z1)
            ENDIF
            X4 =  R2 * COS( PHI1 )
            Y4 =  R2 * SIN( PHI1 )
            IF (ALGNMT) CALL PDALGN(X4,Y4,Z1)
            CALL J3DRAW(X4,Y4,Z1)
            IF (IFDSEC .GT. 1) CALL J3DRAW(X1,Y1,Z1)
C
C ****  Draw the cell limits on the other side
C
            IF (IFDSEC .GE. 3) THEN
              CALL J3MOVE(X1,Y1,Z2)
              CALL J3DRAW(X2,Y2,Z2)
              IF (IFDSEC .EQ. 3) THEN
                CALL J3MOVE(X3,Y3,Z2)
              ELSE
                CALL J3DRAW(X3,Y3,Z2)
              ENDIF
              CALL J3DRAW(X4,Y4,Z2)
              IF (IFDSEC .GT. 3) CALL J3DRAW(X1,Y1,Z2)
              CALL J3MOVE(X1,Y1,Z1)
              CALL J3DRAW(X1,Y1,Z2)
              CALL J3MOVE(X2,Y2,Z1)
              CALL J3DRAW(X2,Y2,Z2)
              CALL J3MOVE(X3,Y3,Z1)
              CALL J3DRAW(X3,Y3,Z2)
              CALL J3MOVE(X4,Y4,Z1)
              CALL J3DRAW(X4,Y4,Z2)
            ENDIF
          ENDIF
C
          DO 99 IWIR= 0,NBWIR-1
            IPWIR = LDALS + 6 + IC(LDALS+6) * IWIR
            XWIR = C( IPWIR+1 )
            YWIR = C( IPWIR+2 )
C
C  Mark wire positions with an "+"
C
            IF (IFDWIR .NE. 0) THEN
              CALL JCMARK(2)
              CALL J3MARK(XWIR,YWIR,Z1)
              CALL J3DRAW(XWIR,YWIR,Z2)
            ENDIF
   99     CONTINUE
C-
   89   CONTINUE
   88 CONTINUE
      CALL JRCLOS
C
  999 RETURN
      END
