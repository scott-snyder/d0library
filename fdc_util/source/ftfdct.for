      SUBROUTINE
     &  FTFDCT(HALF,LADDER,QTRAK,IQTRAK,QHSEC,IQHSEC,CHINORM,VERTEX)
C------------------------------------------------------------------------
C
C    Purpose and Methods : Fit an FDC track candidate to straight line
C
C        x = xG+(z-z0)*dx/dz        z0: a constant in FTRAKS.RCP
C        y = yG+(z-z0)*dy/dz
C
C      Minimize chisq = sum[f(i)-DIST(i)]**2*WT(i) where
C
C        f(i) = sum[AA(i,k)*PAR(k)+BB(i)],
C        PAR = [xG,yG,dx/dz,dy/dz]  (vector of parameters)
C        AA(i,1) = c(i), AA(i,2) = s(i), AA(i,3) = z(i)*c(i), AA(i,4) = z(i)*s(i),
C        BB(i) = -x(i)*c(i)-y(i)*s(i)
C        x(i),y(i),z(i) = coordinates of center of wire i
C        s(i),c(i)  = sin & cos of drift direction wrt x axis
C        ( for delay line: s(i),c(i) --> -c(i),s(i) )
C        DIST(i) drift distance (or delay line distance)
C
C    Input  : HALF        = FDC Half of track
C             LADDER(0:2) = segments in layers (0:2) on track candidate
C                          layers are: 0 - inner Theta chamber
C                                      1 - outer Theta chamber
C                                      2 - Phi chamber
C             VERTEX = 0 to not use vertex point in fit
C                    > 0 to use specified vertex in fit
C
C    Output : QTRAK(26),IQTRAK(26) = Track info from fit
C             QHSEC(3,34),IQHSEC(3,34) = Track hit info from fit
C             CHINORM     = Chi-square from track fit
C
C-   Created   x-DEC-1988   Daria Zieminska
C-   Updated   8-FEB-1990   Jeffrey W. Bantly  remove ZEBSTP refs
C-   Updated  23-JUL-1990   Jeffrey Bantly  add cross-sector segments,
C-                                          new chisq, fix stagger, etc.
C-   Updated  26-NOV-1990   Robert Avery  Fix bug in cross-sector
C-                                      segment hit addresses,
C-                                      and simplify treatment of
C-                                      cross-sector segments
C-   Updated  25-JAN-1991   Susan K. Blessing  modified for use with FTTRAK
C-                                             including changing outputs
C-   Updated  21-MAR-1991   Jeffrey Bantly  add ionization & speedups
C-   Updated  29-APR-1991   Jeffrey Bantly  cleanup, FSTAGR external
C-   Updated   6-JUN-1991   Jeffrey Bantly  Correct ionization angle correction
C-   Updated  17-JUN-1991   Susan K. Blessing  Change size of CONT array.
C-   Updated   1-JUL-1991   Robert E. Avery  Replace call to DRIDIR
C-                                              with FDRIFTDIR.
C-   Updated  12-JUL-1991   Susan K. Blessing  Speedup changes - Remove
C-    some VZERO calls and use (slightly faster) DO loops.
C-    Make calculation of COV double precision.
C-   Updated  17-SEP-1991   Susan K. Blessing  Add calculation of error
C-    for THETA and PHI.  Change size of (I)QTRAK to 26 (two errors and
C-    two spares).
C-   Updated  17-JAN-1992   Susan K. Blessing  Remove ionization loss
C-    calculation.
C-   Updated   9-MAR-1992   Susan K. Blessing  Add VERTEX to call.
C-   Updated  22-APR-1993   Susan K. Blessing  Change VERTEX to an integer
C-    identifying which vertex should be used.
C-   Updated  26-APR-1993   Susan K. Blessing  Add IQTRAK(25) = number of 
C-    points used in fit
C-   Updated  25-AUG-1994   Srini Rajagopalan  Remove calls to GTFSEG,GTFPSC
C-                          and GTFTSC, Replace with local zebra lookup.
C
C------------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
C  Input:
      INTEGER HALF,LADDER(0:2)
      INTEGER VERTEX
C
C  Output:
      INTEGER IQTRAK(26),IQHSEC(3,34)
      REAL QTRAK(26),QHSEC(3,34)
      REAL CHINORM
C
C  Local:
      INTEGER I,J
      INTEGER ICALL,IWIRE,LOGCHA
      INTEGER GZFPSC,GZFTSC
      INTEGER LSEGM,GZFSEG,LOC,LZFIND,LKFXSC
      INTEGER INDEX(4)
      INTEGER NEL,NWORDS,IPAR1,IPAR2
      INTEGER HITADD,WIRENB
      INTEGER UNIT,QUAD,SECTOR,UB,MODULE,LAYER
      INTEGER ITOT,NTOT,IHIT,IHITD,SECTD
      INTEGER ID,WIRE,LR,LRWIR
      INTEGER IFAIL,IER
      INTEGER ISECTD,IADD,NHITS
      INTEGER VCONT(10),NVERT
      INTEGER LISV1,GZISV1
      INTEGER VERTEX1
C
      REAL VERT(18),XVERT,YVERT,ZVERT,VWT_X,VWT_Y
      REAL Z0(2)
      REAL STAGGER
      REAL XWIRE(36),YWIRE(36),ZWIRE(36),SINE(36),COSINE(36),WT(36)
      REAL XC,YC,ZC
      REAL SDRIFT,CDRIFT,PAR(4),PHI,THETA,DRDZ,WNORM
      REAL DTDX,DTDY,ERR_THETA
      REAL DPDX,DPDY,ERR_PHI
      REAL AA(36,4),BB(36),DIST(36),DERR
      REAL CHISQ
      REAL WTDIST,WTAA,SUMAA
      REAL FSTAGR
      REAL RESID
C
      DOUBLE PRECISION COV(4,4),SUM(4),PAR8(4)
C
      LOGICAL FIRST
      LOGICAL FIT_NO_DL
      LOGICAL FIT_NO_DL_DUMMY
      LOGICAL FIT_3L_NO_DL
      LOGICAL THREE_LAYER
      LOGICAL INCLUDE_VERTEX,FOUND_VERTEX
C
      SAVE FIRST,Z0,FIT_NO_DL,FIT_3L_NO_DL,INCLUDE_VERTEX
C
      DATA FIRST/.TRUE./
      DATA FIT_NO_DL/.FALSE./
      DATA INCLUDE_VERTEX/.FALSE./
C
C------------------------------------------------------------------------
C
      IF (FIRST) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('Z0',Z0,IER)
        CALL EZGET_l('FIT_NO_DL',FIT_NO_DL,IER)
        CALL EZGET_l('FIT_3L_NO_DL',FIT_3L_NO_DL,IER)
        CALL EZGET_l('INCLUDE_VERTEX',INCLUDE_VERTEX,IER)
        CALL EZRSET
        FIRST = .FALSE.
      END IF
C
      IF (.NOT.INCLUDE_VERTEX) THEN
        VERTEX1 = VERTEX
      ELSE
        VERTEX1 = 1
      END IF
C
      ITOT = 0
      CHISQ = 0.
C
      DO I = 1, 4
        PAR8(I) = 0.D0
        SUM(I) = 0.D0
        COV(I,I) = 0.D0
        DO J = I+1, 4
          COV(I,J) = 0.D0
          COV(J,I) = 0.D0
        END DO
      END DO
C
      IQTRAK(3) = 0
C
C  Loop through segments on ladder and get hits assigned to them.
C
      THREE_LAYER = (LADDER(0)*LADDER(1)*LADDER(2)) .GT. 0
      DO 100 LAYER = 0,2
        IF (LADDER(LAYER).EQ.0) GO TO 100
C
C  Get contents of segment in LAYER
C
        MODULE = LAYER+3*HALF
C
        LSEGM = GZFSEG(HALF,LAYER)
        LOC = LZFIND(IXCOM,LSEGM,LADDER(LAYER),-5)
        ISECTD = IQ(LOC+1)
        IADD  = IQ(LOC+2)
        NHITS = IQ(LOC+3)
C
        CALL FCODER(IADD,HALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
        IHITD = ISECTD/1000
        IF (ABS(IHITD).NE.0) THEN
          SECTD = SECTOR+(ISECTD/ABS(ISECTD))
        ENDIF
C
C  Loop through hits on segment and get their coordinates
C
        CALL FDRIFTDIR(HALF,UNIT,QUAD,SECTOR,WIRE,SDRIFT,CDRIFT)
C
        DO 200 ID = 1,NHITS
          LRWIR = IQ(LOC+3+ID)
          IF ( (ID .GE. ABS(IHITD)) .AND.
     &        (IHITD .NE. 0) ) THEN
            SECTOR = SECTD
            CALL FCODER(IADD,HALF,UNIT,QUAD,SECTOR,0,0,2)
            CALL FDRIFTDIR(HALF,UNIT,QUAD,SECTOR,WIRE,SDRIFT,CDRIFT)
          ENDIF
          IF (UNIT.LE.0) THEN
            IHIT = IQ(LOC+11+ID)
            LKFXSC = GZFTSC(HALF,QUAD,SECTOR)
          ELSE
            IHIT = IQ(LOC+19+ID)             
            LKFXSC = GZFPSC(HALF,SECTOR)
          END IF
C
          HITADD = IADD*2+LRWIR
          WIRE = LRWIR/2
          LR = LRWIR-WIRE*2
          ITOT = ITOT+1
          SINE(ITOT) = SDRIFT
          COSINE(ITOT) = CDRIFT
          STAGGER = FSTAGR(HALF,UNIT,QUAD,SECTOR,WIRE)
          DIST(ITOT) = Q(LKFXSC+IHIT+2+LR) - STAGGER
          DERR = Q(LKFXSC+IHIT+5)
          IF ((DERR .GT. 0.0000001) .AND. (DERR .LE. 9999.)) THEN
            WT(ITOT) = 1./DERR**2
          ELSE
            WT(ITOT) = 0.05                 ! default in case of DIST_ERR = 0
          ENDIF
C
          CALL GTFALH(HALF,UNIT,QUAD,SECTOR,WIRE,XC,YC,ZC)
          IF (ZC.EQ. 0.0) GOTO 100
          XWIRE(ITOT) = XC
          YWIRE(ITOT) = YC
          ZWIRE(ITOT) = ZC-Z0(HALF+1)
          IQHSEC(1,ITOT) = HITADD
          IQHSEC(2,ITOT) = IHIT
          IF (UNIT.EQ.0 .AND. WIRE.EQ.0) THEN         ! FOR DELAY LINE
            ITOT = ITOT+1
            SINE(ITOT) = COSINE(ITOT-1)   ! original
            COSINE(ITOT) = -SINE(ITOT-1)
            DIST(ITOT) = Q(LKFXSC+IHIT+4)
            DERR = Q(LKFXSC+IHIT+6)
            IF (SECTOR.EQ.1) DIST(ITOT) = -DIST(ITOT)     ! drift
C                  ! direction is reversed for sector 1 so must reverse delay
C                  ! line direction to compensate the fact that it is NOT
C                  ! also reversed like the drift direction
            IF ( FIT_NO_DL
     &        .OR. (FIT_3L_NO_DL.AND.THREE_LAYER) ) THEN
              WT(ITOT) = 0.
            ELSE IF (DERR.GT.0.0)THEN
              WT(ITOT) = 1./DERR**2
            ELSE
              WT(ITOT) = 1./9999.**2
            ENDIF
            XWIRE(ITOT) = XC
            YWIRE(ITOT) = YC
            ZWIRE(ITOT) = ZC-Z0(HALF+1)
            IQHSEC(1,ITOT) = HITADD
            IQHSEC(2,ITOT) = IHIT
          END IF
          IF (LAYER.EQ.0) THEN
            WIRENB = 7-WIRE
          ELSE IF (LAYER.EQ.1) THEN
            WIRENB = WIRE+24
          ELSE
            WIRENB = WIRE+8
          END IF
          IQTRAK(3) = IBSET(IQTRAK(3),WIRENB)
  200   CONTINUE                        ! End of loop over hits in layer
  100 CONTINUE                          ! End of loop over layers in track
C
      IF (VERTEX1.GT.0) THEN
C Get vertex information.
        FOUND_VERTEX = .FALSE.
        CALL GTVERH(VCONT)
        NVERT = VCONT(2)
C
        IF (NVERT.EQ.0) THEN
          LISV1 = GZISV1()
          XVERT = 0.
          YVERT = 0.
          ZVERT = Q(LISV1+9)
          VWT_X = 1. / 0.2
          VWT_Y = 1. / 0.2
          FOUND_VERTEX = .TRUE.
        ELSE IF (NVERT.GE.VERTEX1) THEN
          CALL GTVERT(VERTEX1,VERT)
          XVERT = VERT(3)
          YVERT = VERT(4)
          ZVERT = VERT(5)
          VWT_X = 1. / SQRT(VERT(6)**2 + VERT(8)**2)
          VWT_Y = 1. / SQRT(VERT(7)**2 + VERT(8)**2)
          FOUND_VERTEX = .TRUE.
        END IF
C
C Use vertex point twice, once as X,Z point and once as Y,Z point.
C
        IF (FOUND_VERTEX) THEN
          ITOT = ITOT+1
          SINE(ITOT) = 0.
          COSINE(ITOT) = 1.
          DIST(ITOT) = 0.
          WT(ITOT) = VWT_X
          XWIRE(ITOT) = XVERT
          YWIRE(ITOT) = 0.
          ZWIRE(ITOT) = ZVERT-Z0(HALF+1)
C
          ITOT = ITOT+1
          SINE(ITOT) = 1.
          COSINE(ITOT) = 0.
          DIST(ITOT) = 0.
          WT(ITOT) = VWT_Y
          XWIRE(ITOT) = 0.
          YWIRE(ITOT) = YVERT
          ZWIRE(ITOT) = ZVERT-Z0(HALF+1)
        END IF
      END IF
C
      NTOT = ITOT
C
      WNORM = WT(1)
      DO 300 ITOT = 1,NTOT
        AA(ITOT,1) = COSINE(ITOT)
        AA(ITOT,2) = SINE(ITOT)
        AA(ITOT,3) = ZWIRE(ITOT)*COSINE(ITOT)
        AA(ITOT,4) = ZWIRE(ITOT)*SINE(ITOT)
        BB(ITOT)  = -XWIRE(ITOT)*COSINE(ITOT)-YWIRE(ITOT)*SINE(ITOT)
        WT(ITOT) = WT(ITOT)/WNORM ! to avoid big numbers
        WTDIST = (DIST(ITOT)-BB(ITOT))*WT(ITOT)
        DO 400 IPAR1 = 1,4
          SUM(IPAR1) = SUM(IPAR1)+ AA(ITOT,IPAR1)*WTDIST
  400   CONTINUE
        DO 500 IPAR1 = 1,4        ! calculate inverse of covariance matrix
          WTAA = AA(ITOT,IPAR1)*WT(ITOT)
          DO 501 IPAR2 = IPAR1,4
            COV(IPAR1,IPAR2) = COV(IPAR1,IPAR2) +
     &        DBLE(AA(ITOT,IPAR2))*DBLE(WTAA)
            COV(IPAR2,IPAR1) = COV(IPAR1,IPAR2)
  501     CONTINUE
C
  500   CONTINUE
  300 CONTINUE
C
C     invert covariance matrix:
      CALL DINV(4,COV,4,INDEX,IFAIL)
C
      DO 600 IPAR1 = 1,4
        DO 900 IPAR2 = 1,4
          PAR8(IPAR1) = PAR8(IPAR1)+COV(IPAR1,IPAR2)*SUM(IPAR2)
          COV(IPAR1,IPAR2) = COV(IPAR1,IPAR2)/WNORM
  900   CONTINUE
        PAR(IPAR1) = PAR8(IPAR1)
  600 CONTINUE
C
C  Fill QTRAK,QHSEC
C
      PHI = ATAN2(PAR(4),PAR(3))-PI*FLOAT(HALF-1)
      IF (PHI.LT.0.) PHI = PHI+TWOPI
      IF (PHI.GT.TWOPI) PHI = PHI-TWOPI
C
C Error on phi
      DPDX = 1./(1.+(PAR(4)/PAR(3))**2) * (-1.*PAR(4)/PAR(3)**2)
      DPDY = 1./(1.+(PAR(4)/PAR(3))**2) * (1./PAR(3))
      ERR_PHI = DPDX**2 * COV(3,3) +
     &          DPDY**2 * COV(4,4) +
     &          2.*DPDX*DPDY * COV(3,4)
      IF (ERR_PHI.LT.0.) ERR_PHI = 0.
      ERR_PHI = SQRT(ERR_PHI)
C
      DRDZ = SQRT(PAR(3)**2 + PAR(4)**2)
      THETA = ATAN(DRDZ)
      IF (HALF.EQ.0) THETA = PI-THETA
      IF (THETA.GT.PI) THETA = TWOPI-THETA
      IF (THETA.LT.0.0) THETA = ABS(THETA)
C
C Error on theta
C
      DTDX = PAR(3)/(DRDZ*(1.+DRDZ**2))
      DTDY = PAR(4)/(DRDZ*(1.+DRDZ**2))
      ERR_THETA = DTDX**2 * COV(3,3) +
     &            DTDY**2 * COV(4,4) +
     &            2.*DTDX*DTDY * COV(3,4)
      IF (ERR_THETA.LT.0.) ERR_THETA = 0.
      ERR_THETA = SQRT(ERR_THETA)
C
      IQTRAK(1) = 0
C***************
      IF (VERTEX1.GT.0) THEN
        IQTRAK(2) = NTOT-2                    ! number of hits on track
      ELSE
        IQTRAK(2) = NTOT
      END IF
      QTRAK(4) = PAR(1)                   ! x0
      QTRAK(5) = PAR(2)                   ! y0
      QTRAK(6) = PHI                      ! phi angle
      QTRAK(7) = PAR(3)                   ! dx/dz
      QTRAK(8) = PAR(4)                   ! dy/dz
      QTRAK(9) = COV(1,1)
      QTRAK(10) = COV(1,2)
      QTRAK(11) = COV(1,3)
      QTRAK(12) = COV(1,4)
      QTRAK(13) = COV(2,2)
      QTRAK(14) = COV(2,3)
      QTRAK(15) = COV(2,4)
      QTRAK(16) = COV(3,3)
      QTRAK(17) = COV(3,4)
      QTRAK(18) = COV(4,4)
      QTRAK(20) = 0.                      ! Calculated later in FDEDX
      QTRAK(21) = 0.                      ! Calculated later in FDEDX
      QTRAK(22) = THETA                   ! theta angle
      QTRAK(23) = ERR_PHI
      QTRAK(24) = ERR_THETA
      IQTRAK(25) = NTOT                    ! Number of points used in fit
C
C  Calculate fit residuals.
C
      DO 700 ITOT = 1,NTOT
        SUMAA = 0.0
        DO 800 IPAR1 = 1,4
          SUMAA = SUMAA + AA(ITOT,IPAR1)*PAR(IPAR1)
  800   CONTINUE
        RESID = DIST(ITOT) - BB(ITOT) - SUMAA
        IF (ITOT.LE.34) QHSEC(3,ITOT) = RESID
        CHISQ = CHISQ + RESID**2*WT(ITOT)
  700 CONTINUE
C
      QTRAK(19) = CHISQ*WNORM     ! back to proper normalisation
C
      CHINORM = SQRT(2*QTRAK(19))-SQRT(2*(FLOAT(NTOT)-4.)-1.)
C
C-----------------------------------------------------------------------
  999 CONTINUE
      RETURN
C-----------------------------------------------------------------------
      ENTRY FTFDCT_FIT_NO_DL(FIT_NO_DL_DUMMY)
      FIT_NO_DL = FIT_NO_DL_DUMMY
      RETURN
      END
