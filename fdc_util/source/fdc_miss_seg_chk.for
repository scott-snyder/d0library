      FUNCTION FDC_MISS_SEG_CHK(HALF,QTRAK,LAYER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check if a track that is missing a segment
C-   has a good reason for missing the segment.
C-
C-   Returned value  : .true. if good, .false. if bad.
C-   Inputs  :  HALF = track half
C-              QTRAK = track description
C-              LAYER = Missing layer
C-
C-   Created   1-JUL-1991   Robert E. Avery
C-   Updated  17-SEP-1991   Susan K. Blessing  Change size of (I)QTRAK to 
C-     26 (two errors and two spares).
C-   Updated  22-NOV-1991   Robert E. Avery  Mark track as bad if 
C-      hit finding has not been done for sector that it passes through.
C-   Updated  28-FEB-1992   Robert E. Avery  Increase track road size
C-      to account for track measurement error. 
C-      MISS_SEG_ROAD determines the minimum road size.
C-      This mostly will affect one layer Theta tracks, where the error 
C-      is more or less the delay line measurement error.
C-   Updated  21-JUL-1992   Susan K. Blessing  The errors on theta and 
C-    phi were not being calculated in FIT_SEGTRK, so the new roads weren't
C-    related to anything for two layer tracks.  One layer tracks were okay.
C-    The roads are now very large, so remove the factor of 2.
C-   Updated  24-JUL-1992   Susan K. Blessing  Check value of X_DRIFT in 
C-    loop over LR.  Drift distance=0 for LR=2 for half cells!
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL FDC_MISS_SEG_CHK
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
C   Inputs:
      INTEGER HALF,LAYER
      REAL QTRAK(26)
C
C   Local:      
      INTEGER LFTRH,LFXSC
      INTEGER UNIT,QUAD,SECTOR,WIRE
      INTEGER QUAD_END,SECTOR_END,MAXWIRE 
      INTEGER ADJ_SECTOR
      INTEGER IER
      INTEGER NWRDS,NHIT,NSW 
      INTEGER IPTR, HIT, LR , LRMAX
      INTEGER NUM_EMPTY_WIRES 
      INTEGER MAX_EMPTY_WIRES
      INTEGER DIR 
C
      REAL    STAGGER
      REAL    X_DRIFT 
      REAL    X_RESIDUAL 
      REAL    XWIRE,YWIRE,ZWIRE
      REAL    X_TRACK,Y_TRACK
      REAL    DX_TRACK,DY_TRACK
      REAL    Z0(0:1)
      REAL    DIMEN(6)
      REAL    TRACK_ROAD
      REAL    TRACK_ROAD_MIN
      REAL    BOUND_THETA
C
      REAL    MAX_X_THETA(0:MXSECT) 
      REAL    MIN_X_THETA(0:MXSECT) 
      REAL    MAX_Y_THETA(0:MXSECT,2) 
      REAL    MAX_X_PHI 
      REAL    MAX_Y_PHI
      REAL    MIN_Y_PHI
C
      REAL    TAN_PI_36 
      PARAMETER( TAN_PI_36  =  0.08749 )        ! Tan(pi/36)
C
      LOGICAL FIRST 
      LOGICAL CHECK_MISS_SEG
      LOGICAL OK
C
C   Functions:
      INTEGER GZFTRH
      INTEGER GZFXSC
      REAL    FSTAGR
      LOGICAL FSECTOR_DEAD
      INTEGER QUADTYPE 
      INTEGER FDC_QUADTYPE
C
      SAVE FIRST,Z0
      SAVE MAX_X_THETA,MIN_X_THETA,MAX_Y_THETA
      SAVE MAX_Y_PHI,MIN_Y_PHI
      SAVE CHECK_MISS_SEG 
      SAVE TRACK_ROAD_MIN, MAX_EMPTY_WIRES
      SAVE BOUND_THETA
C
      DATA FIRST /.TRUE./
      DATA MIN_X_THETA / -0.40, -5.33,-0.40, -5.33, -5.33, -5.33/
      DATA MAX_X_THETA /  5.33,  0.40, 5.33,  5.33,  5.33,  5.33/
      DATA MIN_Y_PHI /0./
      DATA MAX_Y_PHI /100./
C
      DATA CHECK_MISS_SEG /.TRUE./
      DATA MAX_EMPTY_WIRES /4/
      DATA TRACK_ROAD_MIN/0.5/
      DATA BOUND_THETA /0.5/
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('CHECK_MISS_SEG',CHECK_MISS_SEG,IER) 
        CALL EZGET('MISS_SEG_ROAD',TRACK_ROAD_MIN,IER) 
        CALL EZGET('MISS_SEG_WIRES',MAX_EMPTY_WIRES,IER) 
        CALL EZGET('MISS_SEG_BOUND_THETA',BOUND_THETA,IER) 
        CALL EZRSET
        IF ( CHECK_MISS_SEG ) THEN
          LFTRH = GZFTRH()
          Z0(0)= Q(LFTRH+3)
          Z0(1)= Q(LFTRH+4)
C
C   Set up sector size arrays
          DO SECTOR =  0, MXSECT
C
            MAX_X_THETA(SECTOR) = MAX_X_THETA(SECTOR) - BOUND_THETA
            MIN_X_THETA(SECTOR) = MIN_X_THETA(SECTOR) - BOUND_THETA
C  A type
            CALL GTFWTX(0,5,SECTOR,DIMEN)
            MAX_Y_THETA(SECTOR,1) =  DIMEN(1) - BOUND_THETA
C  B type
            CALL GTFWTX(0,4,SECTOR,DIMEN)
            MAX_Y_THETA(SECTOR,2) =  DIMEN(2) - BOUND_THETA
          ENDDO
        ENDIF
      ENDIF
C
      IF ( .NOT. CHECK_MISS_SEG ) THEN
        FDC_MISS_SEG_CHK = .TRUE.
        GOTO 999
      ENDIF 
C
      UNIT = 0
      MAXWIRE = MXWIRT
      IF ( LAYER .EQ. 2 ) THEN
        UNIT = 1
        MAXWIRE = MXWIRP 
      ENDIF
C
      QUAD = 0
      IF ( LAYER .EQ. 1 ) QUAD = 4
      NUM_EMPTY_WIRES = 0
      DO 100 WIRE =  0, MAXWIRE 
C
C first check what sector track is in for this wire:
C
        CALL GTFALH( HALF,UNIT,QUAD,0,WIRE,
     &               XWIRE,YWIRE,ZWIRE)
        X_TRACK = QTRAK(4) + QTRAK(7)* (ZWIRE - Z0(HALF) ) 
        Y_TRACK = QTRAK(5) + QTRAK(8)* (ZWIRE - Z0(HALF) )
        CALL FGET_SECTOR(X_TRACK,Y_TRACK,HALF,LAYER,QUAD,SECTOR)
C
        IF ( SECTOR .LT. 0  ) THEN
          QUAD = 0
          IF ( LAYER .EQ. 1 ) QUAD = 4
          GOTO 100
        ENDIF
C
C  Check if SECTOR is being read out, and is alive:
C
        IF ( FSECTOR_DEAD(HALF,UNIT,QUAD,SECTOR) ) THEN
          GOTO 100
        ENDIF
C
C  If hit finding not done for this cell, track should not be kept.
C
        CALL FHITCHK( HALF,UNIT,QUAD,SECTOR,2,OK )
        IF ( .NOT. OK ) THEN
          FDC_MISS_SEG_CHK = .FALSE.
          GOTO 999
        ENDIF
C
C  Get track position in cell coordinates
C
        CALL FDC_CELL(
     &              HALF, UNIT, QUAD, SECTOR, WIRE,
     &              QTRAK(4),QTRAK(5),QTRAK(7),QTRAK(8),
     &              X_TRACK,Y_TRACK ,DX_TRACK ,DY_TRACK)
        STAGGER = FSTAGR(HALF,UNIT,QUAD,SECTOR,WIRE)
        X_TRACK = X_TRACK + STAGGER     ! Measure rel. to cell center
C
C  Look in road determined by track errors, if they exist.
C 
        IF ( UNIT.EQ.0 ) THEN
          TRACK_ROAD = ABS(ZWIRE) * QTRAK(24)
        ELSE
          TRACK_ROAD = Y_TRACK * QTRAK(23)
        ENDIF
C        TRACK_ROAD = MAX( 2.0 * TRACK_ROAD, TRACK_ROAD_MIN)
        TRACK_ROAD = MAX( TRACK_ROAD, TRACK_ROAD_MIN)
C
C  Check if too close to cell wall
C
        ADJ_SECTOR = -1
        IF ( UNIT .EQ. 0 ) THEN
          QUADTYPE = FDC_QUADTYPE(QUAD,HALF)
          IF (   (X_TRACK .LT. MIN_X_THETA(SECTOR))
     &      .OR. (X_TRACK .GT. MAX_X_THETA(SECTOR)) 
     &      .OR. (ABS(Y_TRACK) .GT. MAX_Y_THETA(SECTOR,QUADTYPE))) THEN
            GOTO 100
          ENDIF
        ELSE
          MAX_X_PHI = Y_TRACK*TAN_PI_36 - TRACK_ROAD 
          IF ( ABS(X_TRACK) .GT. MAX_X_PHI ) THEN
            ADJ_SECTOR = SECTOR + SIGN(1.0,X_TRACK)
            ADJ_SECTOR = MOD(ADJ_SECTOR+36,36)
          ENDIF
        ENDIF
C
C  Check if there are hits in track road:
C
        LFXSC = GZFXSC(HALF,UNIT,QUAD, SECTOR)
        IF ( LFXSC .GT. 0 ) THEN
C
          IF ((UNIT.EQ.0).AND.(SECTOR.EQ.1)) THEN
            DIR = -1
          ELSE
            DIR = 1
          ENDIF
          NSW = IQ(LFXSC+2)
          NWRDS = IQ(LFXSC+3)
          NHIT = IQ(LFXSC+4+WIRE)
          IPTR = LFXSC+IQ(LFXSC+4+NSW+WIRE)-1
          DO HIT =  1, NHIT
            DO LR =  1, 2
              X_DRIFT = Q(IPTR+1+LR)*DIR
              IF (X_DRIFT.NE.0.0) THEN
                X_RESIDUAL = X_DRIFT - X_TRACK
                IF (  ABS(X_RESIDUAL ) .LT. TRACK_ROAD ) THEN
                  GOTO 100
                ENDIF
              END IF
            ENDDO
            IPTR = IPTR + NWRDS
          ENDDO
        ENDIF
C
C  Check Adjacent sector if necessary:
C
        IF ( ADJ_SECTOR  .GE. 0) THEN
          CALL FDC_CELL(
     &              HALF, UNIT, QUAD, ADJ_SECTOR, WIRE,
     &              QTRAK(4),QTRAK(5),QTRAK(7),QTRAK(8),
     &              X_TRACK,Y_TRACK ,DX_TRACK ,DY_TRACK)
          STAGGER = FSTAGR(HALF,UNIT,QUAD,ADJ_SECTOR,WIRE)
          X_TRACK = X_TRACK + STAGGER     ! Measure rel. to cell center
          LFXSC = GZFXSC(HALF,UNIT,QUAD,ADJ_SECTOR)
          IF ( LFXSC .GT. 0 ) THEN
C
            IF ((UNIT.EQ.0).AND.(SECTOR.EQ.1)) THEN
              DIR = -1
            ELSE
              DIR = 1
            ENDIF
            NSW = IQ(LFXSC+2)
            NWRDS = IQ(LFXSC+3)
            NHIT = IQ(LFXSC+4+WIRE)
            IPTR = LFXSC+IQ(LFXSC+4+NSW+WIRE)-1
            DO HIT =  1, NHIT
              DO LR =  1, 2
                X_DRIFT = Q(IPTR+1+LR)*DIR
                IF (X_DRIFT.NE.0.0) THEN
                  X_RESIDUAL = X_DRIFT - X_TRACK
                  IF (  ABS(X_RESIDUAL ) .LT. TRACK_ROAD ) THEN
                    GOTO 100
                  ENDIF
                END IF
              ENDDO
              IPTR = IPTR + NWRDS
            ENDDO
          ENDIF
        ENDIF
C
C  No hit in road on this wire:
C
        NUM_EMPTY_WIRES = NUM_EMPTY_WIRES + 1
        IF ( NUM_EMPTY_WIRES  .GT. MAX_EMPTY_WIRES ) THEN
C
C  Not enough hits in road for track to be considered good.
C
          FDC_MISS_SEG_CHK = .FALSE.
          GOTO 999
        ENDIF
  100 CONTINUE
C
      FDC_MISS_SEG_CHK = .TRUE.
C
  999 RETURN
      END
