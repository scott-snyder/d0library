      SUBROUTINE PVTX3D_GEO(SECMIN, SECMAX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 3D displays the VTX geometry between the sectors 
C-                         SECMIN and SECMAX ( the # are given for the 
C-                         layers 2 and 3 which contain 32 sectors).
C-
C-   Inputs  : SECMIN, SECMAX
C-   Outputs : none
C-
C-   Created  05-FEB-1991   Qizhong Li-Demarteau 
C-   Modified 24-JUN-1991   Nobuaki Oshima ( Adapts Combined 3D View. )
C-   Updated   9-JAN-1992   Qizhong Li-Demarteau  changed sector color 
C-   Updated  16-MAR-1993   Alexandre Zinchenko - remove call to ZGVSEC
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GRAPHF77.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER NBSENS
      PARAMETER ( NBSENS = 8 )
C
C  Local Declarations:
C  ===================
C
      INTEGER SECMIN, SECMAX, SECMX, IS
      INTEGER I,L
      INTEGER NHITS(0:NBSENS-1),NWIR,LHIT
      INTEGER KPWIRE(0:NBSENS-1),KP
      INTEGER LAY, SEC, IWIR, ISEG
      INTEGER IFISAJ, IFVSEC, IFVWIR
      INTEGER NBWIR, NSEC, GZVGEH
      REAL    ZPOS(0:2), ZVTXW, RWALL, RBEAM, ZBMP
      REAL    XWIR, YWIR, XCENT, YCENT, XORI, YORI
      REAL    PHIW, CPHIW, SPHIW
      REAL    DDIS1,DDIS, YPOS
      REAL    XHPOS,YHPOS
      REAL    SIZDIS
      REAL    RAYCEN, DELRAY, PHICEN, DELPHI
      REAL    X1, X2, Y1, Y2, R1, R2, PHI1, PHI2, X3, Y3, X4, Y4
      INTEGER LVRFT, LVALS, IPWIR
C
C  Data Statements:
C  ================
C
      DATA SIZDIS / 0.05 /
C
C  Executable Code:
C  =================
C
      CALL PUGETV('VTX DRAW 3D SEC ', IFVSEC )
      CALL PUGETV('VTX DRAW WIRES  ', IFVWIR )
C
      IF (LVGEH .LE. 0) RETURN
      RWALL = C(LVGEH + 10)
      ZVTXW = C(LVGEH + 23)
      CALL PUOPEN
      CALL PXCOLR('GRE')
      CALL JCIRCL(0., 0., ZVTXW, RWALL, 0)
      CALL JCIRCL(0., 0., -ZVTXW, RWALL, 0)
      CALL J3MOVE(0., RWALL, ZVTXW)
      CALL J3DRAW(0., RWALL, -ZVTXW)
      CALL J3MOVE(0.,-RWALL, ZVTXW)
      CALL J3DRAW(0.,-RWALL, -ZVTXW)
      RBEAM = C(LVGEH + 9)
      ZBMP = C(LVGEH + 17)
      CALL JCIRCL(0., 0., ZBMP, RBEAM, 0)
      CALL JCIRCL(0., 0., -ZBMP, RBEAM, 0)
      CALL J3MOVE(0., RBEAM, ZBMP)
      CALL J3DRAW(0., RBEAM, -ZBMP)
      CALL J3MOVE(0.,-RBEAM, ZBMP)
      CALL J3DRAW(0.,-RBEAM, -ZBMP)
C
      LVRFT = LC( LVGEH - 3 )
      CALL PXCOLR( 'BLU' )
      DO 88 LAY = 0,2
        ZPOS(LAY) = C(LVGEH + 17 + 2* LAY)
C
C ****  Access VRFT bank for nominal geometry
C
        NSEC  = IC( LVRFT+ 2+ 7*LAY )
        SECMX = SECMAX
        IF ( SECMAX .GT. NSEC-1 ) SECMX = NSEC-1
        RAYCEN = C( LVRFT+ 7+ 7*LAY )
        PHICEN = C( LVRFT+ 8+ 7*LAY ) * PI / 180.0
        DELRAY = C( LVRFT+ 5+ 7*LAY )
        DELPHI = C( LVRFT+ 6+ 7*LAY ) * PI / 180.0
        R1 = ( RAYCEN - DELRAY ) / COS( DELPHI )
        R2 = ( RAYCEN + DELRAY ) / COS( DELPHI )
        DO 89 IS = SECMIN, SECMX
          SEC = IS
          IF( SEC .LT. 0  ) SEC = SEC + NSEC
          IF( SEC .GT. NSEC-1 ) SEC = SEC - NSEC
C-
          IF ( IFVSEC .NE. 0 ) THEN
C
C ****  Draw the cell limits.
C
            PHI1 = PHICEN + (2*SEC-1) * DELPHI
            PHI2 = PHICEN + (2*SEC+1) * DELPHI
            X1 =  R1 * COS( PHI1 )
            Y1 =  R1 * SIN( PHI1 )
            CALL J3MOVE(X1, Y1, ZPOS(LAY))
            X2 =  R1 * COS( PHI2 )
            Y2 =  R1 * SIN( PHI2 )
            CALL J3DRAW( X2, Y2, ZPOS(LAY))
            X3 =  R2 * COS( PHI2 )
            Y3 =  R2 * SIN( PHI2 )
            IF ( IFVSEC .EQ. 2 .OR. IFVSEC .EQ. 4) THEN   ! draw full cells
              CALL J3DRAW(X3, Y3, ZPOS(LAY))
            ELSE                        ! don't draw inter-sector boundaries
              CALL J3MOVE(X3, Y3, ZPOS(LAY))
            ENDIF
            X4 =  R2 * COS( PHI1 )
            Y4 =  R2 * SIN( PHI1 )
            CALL J3DRAW(X4, Y4, ZPOS(LAY))
            IF ( IFVSEC .EQ. 2 .OR. IFVSEC .EQ. 4)  
     &        CALL J3DRAW(X1, Y1, ZPOS(LAY))
          ENDIF
C
C ****  Draw the cell limits on the other side
C
          IF (IFVSEC .GE. 3) THEN
            CALL J3MOVE(X1,Y1,-ZPOS(LAY))
            CALL J3DRAW(X2,Y2,-ZPOS(LAY))
            IF (IFVSEC .EQ. 3) THEN
              CALL J3MOVE(X3,Y3,-ZPOS(LAY))
            ELSE
              CALL J3DRAW(X3,Y3,-ZPOS(LAY))
            ENDIF
            CALL J3DRAW(X4,Y4,-ZPOS(LAY))
            IF (IFVSEC .GT. 3) CALL J3DRAW(X1,Y1,-ZPOS(LAY))
            CALL J3MOVE(X1,Y1,ZPOS(LAY))
            CALL J3DRAW(X1,Y1,-ZPOS(LAY))
            CALL J3MOVE(X2,Y2,ZPOS(LAY))
            CALL J3DRAW(X2,Y2,-ZPOS(LAY))
            CALL J3MOVE(X3,Y3,ZPOS(LAY))
            CALL J3DRAW(X3,Y3,-ZPOS(LAY))
            CALL J3MOVE(X4,Y4,ZPOS(LAY))
            CALL J3DRAW(X4,Y4,-ZPOS(LAY))
          ENDIF
C
C          CALL ZGVSEC( LAY, SEC, NBWIR, KPWIRE(0), NHITS(0), LHIT)
          LVALS = LC( LC( LC( LSVTX-5 ) -(LAY+1) ) -(SEC+1) )
          CPHIW = C( LVALS+3 )
          SPHIW = C( LVALS+4 )
          DO 99 IWIR= 0, 7 !NBWIR-1
            IPWIR = LVALS + 6 + IC(LVALS+6) * IWIR
            XWIR = C( IPWIR+1 )
            YWIR = C( IPWIR+2 )
C
C  Mark wire positions with an "+"
C  ================================
C
            IF ( IFVWIR .NE. 0 ) THEN
              CALL JCMARK(1)
              CALL J3MARK(XWIR, YWIR, ZPOS(LAY))
              CALL J3DRAW(XWIR, YWIR, -ZPOS(LAY))
            ENDIF
   99     CONTINUE
C-
   89   CONTINUE
   88 CONTINUE
      CALL JRCLOS
C
  999 RETURN
      END
