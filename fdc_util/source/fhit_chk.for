      SUBROUTINE FHIT_CHK(ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX,
     &     NHIT_IN_ROAD, NWIRE_IN_ROAD,  NWIRE_HT_IN_ROAD, CHECK_ALL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check how many hits are consistent with being
C-      inside an FDC road. EM roads for which NWIRE_HT_IN_ROAD/NWIRE_IN_ROAD
C-      is less than ~ 0.8  have a high probability of being a Gammma.
C-      I use FDC compressed hits, so can be used with
C-      STA file, but not DST file.
C-
C-   Inputs  :  ZVTX,             Z-Vertex to use.
C-              PHIMIN,PHIMAX,    Tracking Road to be checked.
C-              THEMIN,THEMAX   
C-   Outputs :  NHIT_IN_ROAD,     Total number of hits in road 
C-              NWIRE_IN_ROAD     Number of wires (at most 32) for which road 
C-                                center passes though FDC fiducial volume.
C-              NWIRE_HT_IN_ROAD, Number of those wires that have 1 or more
C-                                hit witin the road.
C-              CHECK_ALL         .TRUE., then check all hits.
C-                                .FALSE., then check only hits on 
C-                                              track segments.
C-
C-   Created  13-MAY-1993   Robert E. Avery
C-   Updated  18-NOV-1993   Robert E. Avery  Change call of PFGETHITS 
C-                                              to  FGETHITS.
C-   Updated  13-DEC-1993   Robert E. Avery  Add option 'CHECK_ALL' 
C-   Updated  17-AUG-1994   Susan K. Blessing  Remove call to FLFSEC 
C-    (done in FDROAD), replace ON array with call to FSECTOR_IN_ROAD.
C-   Updated   9-SEP-1994   Susan K. Blessing  Put FLFSEC call back in
C-    to set up ON array.  Forgot this was a stand alone routine.
C-   Updated   25-MAR-1995   Yi-Cheng Liu  Define DATA RUNSAV,IDSAV 
C-                           before use. 
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      REAL    ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX
      INTEGER WIRE_HIT,NHIT_IN_ROAD,NWIRE_HT_IN_ROAD
      INTEGER WIRE_IN_ROAD ,NWIRE_IN_ROAD 
      LOGICAL CHECK_ALL 
C
      INTEGER LFTRH, GZFTRH, LFHIT, GZFHIT
      INTEGER HALF,UNIT,QUAD,SECT,WIRE 
      INTEGER LAYER 
      INTEGER MAX_QUAD,MAX_SECT,MAX_WIRE 
      INTEGER IHIT, N_HITS(0:MXWIRP)
      INTEGER WL, WL0, LR 
      INTEGER RUN,ID,RUNSAV,IDSAV
C
      REAL    TEMP
      REAL    PAVE, TANAVE, T1,T2
      REAL    THETCV(2),THETFV(2),THETFF(2)
      REAL    XPOS,YPOS
      REAL    Z0(0:1),ZW(0:31,0:1)
      REAL    X1, Y1, X2, Y2 
      REAL    DX1, DY1, DX2, DY2 
      REAL    X_MIN, X_MAX 
      REAL    X_MIN_0, DX_MIN, X_MAX_0, DX_MAX 
      REAL    STAGGER, FSTAGR
      REAL    X_DIR, Z
      REAL    X2_R, Y2_R, DX2_R, DY2_R 
      REAL    X1_R, Y1_R, DX1_R, DY1_R 
      REAL    XC, YC, DXC, DYC 
      REAL    DRIFT 
      REAL    DR_DIST(0:1, MX_HIT_WIRE, 0:MXWIRP)
      REAL    MAX_DIST
      PARAMETER ( MAX_DIST = 6.0 )
C
      LOGICAL FIRST 
      LOGICAL FDC_ALIGNCHK, FALH_CHANGED 
      LOGICAL FSECTOR_IN_ROAD
      LOGICAL DL_OK(MX_HIT_WIRE, 0:MXWIRP)
      LOGICAL ON_SEG(MX_HIT_WIRE, 0:MXWIRP)
C
      DATA FIRST /.TRUE./
      DATA RUNSAV,IDSAV/-1,-1/
C----------------------------------------------------------------------
      NHIT_IN_ROAD = 0
      NWIRE_IN_ROAD = 0
      NWIRE_HT_IN_ROAD = 0
C
      LFTRH = GZFTRH()
      IF ( LFTRH .LE. 0 ) GOTO 999
C
      LFHIT = GZFHIT()
      IF ( LFHIT.LE.0 ) GOTO 999
C
C Check if road overlaps FDC:
C
      CALL EVNTID(RUN,ID)
      IF(RUN.NE.RUNSAV.OR.ID.NE.IDSAV) THEN
        RUNSAV=RUN
        IDSAV=ID
        CALL ZSEPRT(ZVTX,THETCV,THETFV,THETFF)
      ENDIF
      IF ( THEMAX.LE.THEMIN ) THEN
        T2 = THEMIN
        T1 = THEMAX
      ELSE
        T1 = THEMIN
        T2 = THEMAX
      ENDIF
      IF (   (T1 .GT. THETCV(1) .OR. T2 .LT. THETFF(1) )
     &  .AND.(T1 .GT. THETFF(2) .OR. T2 .LT. THETCV(2) ) ) GOTO 999
C
C Some initialization
C
      FALH_CHANGED = FDC_ALIGNCHK()
      IF ( FIRST .OR. FALH_CHANGED ) THEN
        FIRST = .FALSE.
        DO HALF =  0, 1
          DO LAYER = 0,2
            IF ( LAYER.LE.1 ) THEN
              UNIT = 0
              MAX_WIRE = MXWIRT
              QUAD = 4*LAYER
            ELSE
              UNIT = 1
              MAX_WIRE = MXWIRP
              QUAD = 0
            ENDIF
            DO WIRE =  0, MAX_WIRE
              WL= WIRE+8*LAYER
              CALL GTFALH(
     &              HALF,UNIT,QUAD,0,WIRE,
     &              XPOS,YPOS,ZW(WL,HALF))
            ENDDO
          ENDDO
        ENDDO
        Z0(0)= Q(LFTRH+3)
        Z0(1)= Q(LFTRH+4)
      ENDIF
C
C Call FLFSEC to set ON array for sectors in road
      CALL FLFSEC(ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX)
C
C Define road limits, and road center:
C
      PAVE = (PHIMIN+PHIMAX)/2.
      TANAVE = TAN( (T1+T2)/2. )
      IF ( TANAVE.LT.0.0 ) THEN
        TEMP = T1
        T1 = T2
        T2 = TEMP
        HALF = 0 
      ELSE
        HALF = 1
      ENDIF
C
      WIRE_HIT = 0
      DO UNIT = 0, MXUNIT
        IF ( UNIT.EQ.0 ) THEN
          MAX_QUAD = MXQUAD
          MAX_SECT = MXSECT
          MAX_WIRE = MXWIRT
          DX1 = COS(PAVE)*TAN(T1)
          DY1 = SIN(PAVE)*TAN(T1)
          DX2 = COS(PAVE)*TAN(T2)
          DY2 = SIN(PAVE)*TAN(T2)
        ELSE
          MAX_QUAD = 0
          MAX_SECT = MXSECP
          MAX_WIRE = MXWIRP
          DX1 = COS(PHIMIN)*TANAVE
          DY1 = SIN(PHIMIN)*TANAVE
          DX2 = COS(PHIMAX)*TANAVE
          DY2 = SIN(PHIMAX)*TANAVE
        ENDIF
        Z = Z0(HALF) - ZVTX 
        X1 = Z * DX1 
        Y1 = Z * DY1 
        X2 = Z * DX2 
        Y2 = Z * DY2 
C
        DO QUAD =  0, MAX_QUAD
          LAYER = QUAD/4 + 2*UNIT
          WL0 = 8*LAYER 
C
          DO SECT =  0, MAX_SECT
C
C Check all sectors on road
C
            IF ( FSECTOR_IN_ROAD(HALF, UNIT, QUAD, SECT) ) THEN
C
              IF ( ( UNIT.EQ.0 ) .AND.( SECT.EQ.1 ) ) THEN
                X_DIR = -1
              ELSE
                X_DIR = 1
              ENDIF
              STAGGER = FSTAGR(HALF, UNIT, QUAD, SECT, 0)
C
C Find Road limits in local cell coordinates:
C
              CALL FDC_CELL(
     &         HALF, UNIT, QUAD, SECT, 0,
     &         X1, Y1, DX1, DY1,
     &         X1_R, Y1_R, DX1_R, DY1_R )
              DX_MIN = DX1_R 
              X_MIN_0 = X1_R - DX_MIN * ZW(WL0,HALF) + STAGGER
C
              CALL FDC_CELL(
     &         HALF, UNIT, QUAD, SECT, 0,
     &         X2, Y2, DX2, DY2,
     &         X2_R, Y2_R, DX2_R, DY2_R )
              DX_MAX = DX2_R 
              X_MAX_0 = X2_R - DX_MAX * ZW(WL0,HALF) + STAGGER
C
C Get all the hits in the cell:
C
              CALL FGETHITS(HALF,UNIT,QUAD,SECT,
     &                   N_HITS,DR_DIST,DL_OK,ON_SEG)
C
              DO WIRE =  0, MAX_WIRE
C
                WL = WIRE + 8*LAYER
                X_MIN = X_MIN_0 + DX_MIN * ZW(WL,HALF) 
                X_MIN = MAX(MIN(X_MIN,MAX_DIST),-MAX_DIST)
                X_MAX = X_MAX_0 + DX_MAX * ZW(WL,HALF) 
                X_MAX = MAX(MIN(X_MAX,MAX_DIST),-MAX_DIST)
                IF ( X_MIN.NE.X_MAX  ) THEN
C              
C Check for hits within road limits:
C
                  DO IHIT = 1, N_HITS(WIRE)
                    IF ( CHECK_ALL .OR. ON_SEG(IHIT,WIRE) ) THEN
                      DO LR =  0, 1
                        DRIFT = X_DIR * DR_DIST(LR,IHIT,WIRE)
                        IF (   (DRIFT.GT.X_MIN)
     &                   .AND. (DRIFT.LT.X_MAX) ) THEN
C
                          NHIT_IN_ROAD = NHIT_IN_ROAD + 1
                          WIRE_HIT=IBSET(WIRE_HIT,WL)
C
                        ENDIF
                      ENDDO
                    ENDIF
                  ENDDO
                ENDIF
              ENDDO
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C Check how many wire layers are in fiducial 
C
      NWIRE_IN_ROAD = 0
      WIRE_IN_ROAD = 0
      DXC = COS(PAVE)*TAN(TANAVE)
      DYC = SIN(PAVE)*TAN(TANAVE)
      DO LAYER = 0,2
        IF ( LAYER.LE.1) THEN
          MAX_WIRE = MXWIRT
        ELSE
          MAX_WIRE = MXWIRP
        ENDIF
        DO WIRE =  0, MAX_WIRE
          WL = WIRE + 8*LAYER
          XC = DXC * (ZW(WL,HALF)-ZVTX) 
          YC = DYC * (ZW(WL,HALF)-ZVTX) 
          CALL FGET_SECTOR(XC,YC,HALF,LAYER,QUAD,SECT)
          IF ( SECT.NE.-1 ) THEN
            NWIRE_IN_ROAD = NWIRE_IN_ROAD + 1
            WIRE_IN_ROAD = IBSET(WIRE_IN_ROAD,WL)
          ENDIF
        ENDDO
      ENDDO
C
      DO WL =  0, 31
        IF ( BTEST(WIRE_HIT,WL)
     &    .AND. BTEST(WIRE_IN_ROAD,WL)) THEN
          NWIRE_HT_IN_ROAD = NWIRE_HT_IN_ROAD + 1
        ENDIF
      ENDDO
C      
  999 RETURN
      END
