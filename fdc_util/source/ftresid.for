      SUBROUTINE FTRESID(HALF,LADDER,RESID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : refit to compute residual w/o point
C-   in fit for each hit on full track. Get hits from banks.
C-
C-   Inputs  :  HALF      = FDC Half
C-              LADDER    = FDC track ladder of segments
C-   Outputs :  RESID(34) = Hit residuals
C-
C-   Created   1-NOV-1990   Robert E. Avery
C-   Updated  17-JUN-1991   Susan K. Blessing  Change size of CONT, ICONT
C-    arrays.
C-   Updated   1-JUL-1991   Robert E. Avery  Replace call to DRIDIR 
C-                                              with FDRIFTDIR.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER MAX_POINTS
      PARAMETER( MAX_POINTS = 34 )
C INPUT
      INTEGER HALF
      INTEGER LADDER(0:2)
C OUTPUT
      REAL    RESID(MAX_POINTS)
C LOCAL
      INTEGER FIT_POINT, IPOINT
      INTEGER IPAR1,IPAR2
      INTEGER IFAIL
      INTEGER INDEX(4)
      INTEGER NEL,NWORDS,LWIRE
      INTEGER IER
      INTEGER LSECT,LSECTM1,LSECTP1
      INTEGER HITADD,WIRENB(32)
      INTEGER UNIT,QUAD,SECTOR,UB,MODULE,LAYER
      INTEGER ITOT,NTOT,I,IHIT,IHITD,SECTD
      INTEGER ID,WIRE,LR,LRWIR,NWLAY,LVRFT
      INTEGER ICALL,IWIRE,IH
C
      REAL Z0(2)
      REAL CONT(62)
      REAL FSTAGR,STAGGER
      EXTERNAL FSTAGR
      REAL XWIRE(MAX_POINTS),YWIRE(MAX_POINTS),ZWIRE(MAX_POINTS)
      REAL SINE(MAX_POINTS),COSINE(MAX_POINTS),WT(MAX_POINTS)
      REAL XC,YC,ZC,XCD,YCD,ZCD
      REAL SDRIFT,CDRIFT,PHI,THETA,DRDZ,WNORM
      REAL AA(MAX_POINTS,4),BB(MAX_POINTS),DIST(MAX_POINTS)
C
      DOUBLE PRECISION COV(4,4),DET,SUM(4),PAR(4)
C
      LOGICAL FCHEKL
C
      REAL QHIT(18)
      INTEGER IQHIT(18)
      EQUIVALENCE (IQHIT,QHIT)
C
      REAL FSECTD,FIADD,FNHITS
      INTEGER ISECTD,IADD,NHITS
      EQUIVALENCE (ISECTD,FSECTD),(IADD,FIADD),(NHITS,FNHITS)
C
      SAVE ICALL,Z0
      DATA ICALL/0/
C----------------------------------------------------------------------

      IF (ICALL.EQ.0) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('Z0',Z0,IER)
        CALL EZRSET
        ICALL=1
      END IF
C
C  Loop through segments on ladder and get hits assigned to them.
C
      IPOINT=0
      DO 100 LAYER=0,2
        IF (LADDER(LAYER).EQ.0) GO TO 100
C
C  Get contents of segment in LAYER
C
        MODULE=LAYER+3*HALF
        CALL GTFSEG(MODULE,LADDER(LAYER),CONT)
        FSECTD=CONT(1)
        FIADD=CONT(2)
        FNHITS=CONT(3)                    ! number of hits in segment
        CALL FCODER(IADD,HALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
        IHITD=ISECTD/1000
        IF(ABS(IHITD).NE.0) THEN
          SECTD=SECTOR+(ISECTD/ABS(ISECTD))
        ENDIF
C
C  Loop through hits on segment and get their coordinates
C
        CALL FDRIFTDIR(HALF,UNIT,QUAD,SECTOR,WIRE,SDRIFT,CDRIFT)
        DO 200 ID=1,NHITS
          LRWIR=CONT(3+ID)
          IF( (ID .GE. ABS(IHITD)) .AND.
     &        (IHITD .NE. 0) ) THEN
            SECTOR = SECTD
            CALL FCODER(IADD,HALF,UNIT,QUAD,SECTOR,0,0,2)
            CALL FDRIFTDIR(HALF,UNIT,QUAD,SECTOR,WIRE,SDRIFT,CDRIFT)
          ENDIF
          IF (UNIT.LE.0) THEN
            IHIT=CONT(11+ID)
            CALL GTFTSC(HALF,QUAD,SECTOR,'HIT',IHIT,NEL,NWORDS,QHIT)
          ELSE
            IHIT=CONT(19+ID)
            CALL GTFPSC(HALF,SECTOR,'HIT',IHIT,NEL,NWORDS,QHIT)
          END IF
          HITADD=IADD*2+LRWIR
          WIRE=LRWIR/2
          LR=LRWIR-WIRE*2
          IPOINT=IPOINT+1
          SINE(IPOINT)=SDRIFT
          COSINE(IPOINT)=CDRIFT
          STAGGER=FSTAGR(HALF,UNIT,QUAD,SECTOR,WIRE)
          DIST(IPOINT)=QHIT(2+LR)-STAGGER
          IF ((QHIT(5) .GT. 0.0000001) .AND. (QHIT(5) .LE. 9999.)) THEN
            WT(IPOINT)=1./QHIT(5)**2
          ELSE
            WT(IPOINT)=0.05                 ! default in case of QHIT(5)=0
          ENDIF
          CALL GTFALH(HALF,UNIT,QUAD,SECTOR,WIRE,XC,YC,ZC)
          IF(ZC.EQ. 0.0) GOTO 100
          XWIRE(IPOINT) = XC
          YWIRE(IPOINT) = YC 
          ZWIRE(IPOINT) = ZC-Z0(HALF+1)
          IF (UNIT.EQ.0 .AND. WIRE.EQ.0) THEN         ! FOR DELAY LINE 
            IPOINT=IPOINT+1
            SINE(IPOINT)=COSINE(IPOINT-1)   ! original
            COSINE(IPOINT)=-SINE(IPOINT-1)
            DIST(IPOINT)=QHIT(4)
            IF (SECTOR.EQ.1) DIST(IPOINT)=-DIST(IPOINT)     ! drift
            IF(QHIT(6).GT.0.0)THEN
              WT(IPOINT)=1./QHIT(6)**2
            ELSE
              WT(IPOINT)=1./9999.**2
            ENDIF
            XWIRE(IPOINT) = XC 
            YWIRE(IPOINT) = YC 
            ZWIRE(IPOINT) = ZC-Z0(HALF+1)
          END IF
  200   CONTINUE
  100 CONTINUE
      NTOT=IPOINT
C
      WNORM = WT(1)
      DO IPOINT = 1,NTOT
        AA(IPOINT,1) = COSINE(IPOINT)
        AA(IPOINT,2) = SINE(IPOINT)
        AA(IPOINT,3) = ZWIRE(IPOINT)*COSINE(IPOINT)
        AA(IPOINT,4) = ZWIRE(IPOINT)*SINE(IPOINT)
        BB(IPOINT)   = -XWIRE(IPOINT)*COSINE(IPOINT)
     &                 -YWIRE(IPOINT)*SINE(IPOINT)
        WT(IPOINT) = WT(IPOINT)/WNORM ! to avoid big numbers
      ENDDO
C
      DO FIT_POINT = 1,NTOT
        CALL VZERO(COV,32)
        CALL VZERO(SUM,8)
        CALL VZERO(PAR,8)
        DO IPOINT=1,NTOT
          IF (IPOINT .NE. FIT_POINT ) THEN
            DO IPAR1=1,4
              SUM(IPAR1)=SUM(IPAR1)
     &         + AA(IPOINT,IPAR1)
     &         *(DIST(IPOINT)-BB(IPOINT))*WT(IPOINT)
            ENDDO
            DO IPAR1=1,4
              DO IPAR2=IPAR1,4
                COV(IPAR1,IPAR2)=COV(IPAR1,IPAR2)
     &            + AA(IPOINT,IPAR1)*AA(IPOINT,IPAR2)*WT(IPOINT)
                COV(IPAR2,IPAR1)=COV(IPAR1,IPAR2)
              ENDDO
            ENDDO
          ENDIF
        ENDDO
C     invert covariance matrix:
        CALL DINV(4,COV,4,INDEX,IFAIL)   ! try new cernlib routine     
        DO IPAR1=1,4
          DO IPAR2=1,4
            PAR(IPAR1)=PAR(IPAR1)+COV(IPAR1,IPAR2)*SUM(IPAR2)
          ENDDO
        ENDDO
C
        RESID(FIT_POINT) = DIST(FIT_POINT)-BB(FIT_POINT)
        DO IPAR1=1,4
          RESID(FIT_POINT) = RESID(FIT_POINT)
     &      - AA(FIT_POINT,IPAR1)*PAR(IPAR1)
        ENDDO
      ENDDO
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
