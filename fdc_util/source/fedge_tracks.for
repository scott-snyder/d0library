      SUBROUTINE FEDGE_TRACKS(HALF,ZVTX,PMIN,PMAX,TMIN,TMAX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make FDC tracks using unused theta segments
C-    and the vertex location at edges of the FDC.
C-
C-   Inputs  : HALF
C-   Outputs :
C-   Controls:
C-
C-   Created   7-FEB-1992   Susan K. Blessing
C-   Updated  10-MAR-1992   Susan K. Blessing  Call FTFDCT with
C-    (new) vertex flag set to true rather than calling FIT_EDGE.
C-   Updated  23-APR-1993   Susan K. Blessing  Change call to FTFDCT.
C-    Change vertex flag to integer identifying vertex of interest.
C-    For these tracks, want to use most likely vertex which will be
C-    the first primary vertex: number 1.
C-    Remove calculation of number of points and number of hits.  FTFDCT
C-    does this correctly now.
C-   Updated  12-MAY-1993   Susan K. Blessing  Change call to include
C-    road.  Call FDC_IN_ROAD to check if track is in road before loading.
C-   Updated  27-JUL-1993   Susan K. Blessing  Add call to FDC_EDGE_CHK
C-    to check that edge track doesn't go through a second theta sector.
C-   Updated  17-AUG-1994   Susan K. Blessing  Remove FCHEKL call, duplicate
C-    its function here and check if segment is in road using FSECTOR_IN_ROAD.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER HALF,H,L
      INTEGER UNIT,QDRT,SCTR,WIRE,UBIT
      INTEGER IADD,IER
      INTEGER MODULE,LAYER
      INTEGER LFSEG,GZFSEG
      INTEGER SEG,NSEG(0:1),NZBANK
      INTEGER LAD(0:2)
      INTEGER NLADD,LADDRS(0:2,MXTRAK),LADDER(0:2)
      INTEGER NDELAY
      INTEGER IQTRAK(26),IQHSEC(3,34)
      INTEGER VCONT(10),NVERT
      INTEGER LISV1,GZISV1
      INTEGER IVERTEX
      INTEGER LFTRH
C
      REAL CHINORM
      REAL QTRAK(26),QHSEC(3,34)
      EQUIVALENCE (IQTRAK,QTRAK),(IQHSEC,QHSEC)
      REAL CHIFEDGE,NFEDGE
      REAL CHIMAX_EDGE
      REAL ZVTX,PMIN,PMAX,TMIN,TMAX
C
      LOGICAL FIRST
      LOGICAL LAYER_OK,FDC_MISS_SEG_CHK
      LOGICAL USE_ISA_VERT,ONE_PRIM_VERT
      LOGICAL FDC_IN_ROAD
      LOGICAL FDC_EDGE_CHK
      LOGICAL OK
      LOGICAL FSECTOR_IN_ROAD
C
      SAVE FIRST,CHIMAX_EDGE,USE_ISA_VERT,ONE_PRIM_VERT,IVERTEX
C
      DATA FIRST/.TRUE./
      DATA CHIMAX_EDGE/100./
      DATA USE_ISA_VERT/.FALSE./
      DATA ONE_PRIM_VERT/.TRUE./
      DATA IVERTEX/1/
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('CHIMAX_EDGE',CHIMAX_EDGE,IER)
        CALL EZGET('USE_ISA_VERT',USE_ISA_VERT,IER)
        CALL EZGET('ONE_PRIM_VERT',ONE_PRIM_VERT,IER)
        CALL EZRSET
        FIRST = .FALSE.
      END IF
C
      NLADD = 0
C Get vertex information.
      CALL GTVERH(VCONT)
      NVERT = VCONT(2)
C
      IF (NVERT.EQ.0) THEN
        IF (USE_ISA_VERT) THEN
          LISV1 = GZISV1()
          IF (LISV1.GT.0) THEN
            CALL ERRMSG('FDC-use-isa-vert','FEDGE_TRACKS',
     &        ' Using Isajet vertex','I')
          ELSE
            CALL ERRMSG('FDC-no-prim-vert','FEDGE_TRACKS',
     &        ' No primary vertex','I')
            GO TO 999
          END IF
C
        ELSE
          CALL ERRMSG('FDC-no-prim-vert','FEDGE_TRACKS',
     &      ' No primary vertex','I')
          GO TO 999
        END IF
C
      ELSE IF (NVERT.GT.1) THEN
        IF (ONE_PRIM_VERT) THEN
          CALL ERRMSG('FDC-many-prim-vert','FEDGE_TRACKS',
     &      ' Too many primary vertices','I')
          GO TO 999
        END IF
      END IF
C
C Loop over segments in inner layer
C
      LFSEG = GZFSEG(HALF,0)
      NSEG(0) = NZBANK(IXCOM,LFSEG)
      DO SEG = 1, NSEG(0)
C
C Check if this segment has been used
        IF (.NOT.BTEST(IQ(LFSEG),IUSED)) THEN
C
          SCTR = MOD(IQ(LFSEG+1),1000)
C In inner theta, only use segments from sectors 4 and 5.
          IF (SCTR.GE.4) THEN
C
C Check if this sector is in road
            IADD = IQ(LFSEG+2)
            CALL FCODER(IADD,H,UNIT,QDRT,SCTR,WIRE,UBIT,1)
C
            IF (FSECTOR_IN_ROAD(H,UNIT,QDRT,SCTR)) THEN
C
C Only allow segments with delay lines
              CALL FCHECK_DL(HALF,0,SEG,NDELAY)
C
              IF (NDELAY.GT.0) THEN
                NLADD = NLADD + 1
                LADDRS(0,NLADD) = SEG
                LADDRS(1,NLADD) = 0
                LADDRS(2,NLADD) = 0
                IF (NLADD.GE.MXTRAK) GO TO 200
              END IF
            END IF
C
          END IF
        END IF
        LFSEG = LQ(LFSEG)
      END DO
C
C Loop over segments in outer layer
      LFSEG = GZFSEG(HALF,1)
      NSEG(1) = NZBANK(IXCOM,LFSEG)
      DO SEG = 1, NSEG(1)
C
C Check if this segment has been used
        IF (.NOT.BTEST(IQ(LFSEG),IUSED)) THEN
C
          SCTR = MOD(IQ(LFSEG+1),1000)
C In outer theta, only use segments from sector 0.
          IF (SCTR.EQ.0) THEN
C
C Check if this sector is in road
            IADD = IQ(LFSEG+2)
            CALL FCODER(IADD,H,UNIT,QDRT,SCTR,WIRE,UBIT,1)
C
            IF (FSECTOR_IN_ROAD(H,UNIT,QDRT,SCTR)) THEN
C
C Only allow segments with delay lines
              CALL FCHECK_DL(HALF,1,SEG,NDELAY)
C
              IF (NDELAY.GT.0) THEN
                NLADD = NLADD + 1
                LADDRS(0,NLADD) = 0
                LADDRS(1,NLADD) = SEG
                LADDRS(2,NLADD) = 0
                IF (NLADD.GE.MXTRAK) GO TO 200
              END IF
            END IF
          END IF
        END IF
        LFSEG = LQ(LFSEG)
      END DO
C
  200 CONTINUE
C
      DO L = 1, NLADD
        LADDER(0) = LADDRS(0,L)
        LADDER(1) = LADDRS(1,L)
        LADDER(2) = LADDRS(2,L)
        CALL FTFDCT(HALF,LADDER,QTRAK,IQTRAK,QHSEC,IQHSEC,
     &    CHINORM,IVERTEX)
C
C  Check if segments should have been found in missing layers.
        DO LAYER = 0, 2
          IF (LADDER(LAYER).EQ.0) THEN
            LAYER_OK = FDC_MISS_SEG_CHK(HALF,QTRAK,LAYER)
            IF (.NOT.LAYER_OK) GO TO 100
          END IF
        END DO
C
        IF (CHINORM.LT.CHIMAX_EDGE) THEN
C
C Calculate correct residuals if requested.
C            IF (TRKRES) THEN
C              CALL FTRESID(HALF,LADDER,RESID)
C              DO I = 1, 34
C                QHSEC(3,I) = RESID(I)
C              END DO
C            END IF
C
          IQTRAK(1) = HALF
C
C Check that track doesn't go through a second theta sector
          OK = FDC_EDGE_CHK(HALF,LADDER,QTRAK)
C
          IF (OK) THEN
C Check if in road
            OK = FDC_IN_ROAD(HALF,QTRAK,ZVTX,PMIN,PMAX,TMIN,TMAX)
            IF (OK) THEN
C Mark track as an edge track.
              IQTRAK(1) = IBSET(IQTRAK(1),1)
              CALL LDFDCT(QTRAK,QHSEC,HALF,LADDER)  ! Store track
            END IF
          END IF
C
        END IF
  100   CONTINUE
      END DO
C
  999 RETURN
      END
