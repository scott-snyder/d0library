      SUBROUTINE FTHIRD_LAYER
     &  (HALF,LADDER,QTRAK,NLADD,LADDER_NEW,N_NEW,SAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   5-SEP-1991   Susan K. Blessing
C-   Updated  14-OCT-1991   Susan K. Blessing   Only build new three layer
C-    ladders from two layer tracks, don't fit them now.
C-   Updated  16-DEC-1991   Susan K. Blessing  Check last wire in missing
C-    layer to see if a neighboring sector should be searched for segments.
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block.
C-   Updated   6-MAR-1992   Susan K. Blessing  Make sure within a sector
C-    in missing layer before calling FCODER.
C-   Updated   2-APR-1993   Susan K. Blessing  Include a check on the
C-    theta slopes if the missing layer is a theta.  Add SAME flag so that
C-    routine can be called more than once if MXTRAK is hit.
C-  Updated 5-MAR-1994  Roy C. Thatcher  Moved subroutine name so it will
C-    be listed by D0entry
C-   Updated  24-AUG-1994   Susan K. Blessing   Remove GTFSEG call, replace
C-    with LZFIND and direct access to ZEBCOM.
C-   Updated   2-SEP-1994   Susan K. Blessing   Add check on slope of
C-    segments.
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER H,I,J,K,L
      INTEGER HALF
      INTEGER TU,TQ,TQ1,TS1,TL,TW,TQ2,TS2
      INTEGER IER,LSEG,NSEG,ISEG,MXTSEG
      PARAMETER (MXTSEG = 100)
      INTEGER NTEST,TESTSG(MXTSEG)
      INTEGER NLADD,LADDER(0:2,MXTRAK)
      INTEGER TLADDER(0:2)
      INTEGER LOC,ISEG_LOGCHA,TLOGCHA1,TLOGCHA2,MISS_LYR
      INTEGER GZFSEG,NZBANK
      INTEGER N_NEW,LADDER_NEW(0:2,MXTRAK)
      INTEGER TRAK
      INTEGER MODULE
      INTEGER SAVE_TRAK,SAVE_TRAK1
      INTEGER LZFIND,LFSEG0,LFSEG1
C
      REAL QTRAK(26,MXTRAK)
      REAL Z0(2)
      REAL XC,YC,ZC,XPOS,YPOS,ZPOS(0:1,0:2,0:1)
      REAL SLOPE,TOLSLOPE,CONT(62)
      REAL THETA_SLOPE_MAX,PHI_SLOPE_MAX
C
      LOGICAL FIRST
      LOGICAL PRODUC, PRODFL
      LOGICAL SAME
      LOGICAL SLOPE_OK
C
      SAVE FIRST,Z0,TOLSLOPE,PRODFL
      SAVE THETA_SLOPE_MAX,PHI_SLOPE_MAX
C
      DATA  FIRST/.TRUE./
      DATA THETA_SLOPE_MAX,PHI_SLOPE_MAX/2*100./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET_rarr('Z0',Z0,IER)
        CALL EZGET('TOLSLOPE',TOLSLOPE,IER)
        CALL EZGET('THETA_SLOPE_MAX',THETA_SLOPE_MAX,IER)
        CALL EZGET('PHI_SLOPE_MAX',PHI_SLOPE_MAX,IER)
        CALL EZRSET
C
C Fill array ZPOS with z position of first and last wires in each layer
        DO H = 0, 1
          DO L = 0, 2
C
            TU = L/2
C
            TQ = 0
            IF (L.EQ.1) TQ = 4
            IF (L.LE.1) THEN
              TW = 7
            ELSE
              TW = 15
            END IF
C
C First wire
            CALL GTFALH(H,TU,TQ,0,0,XC,YC,ZC)
            ZPOS(H,L,0) = ZC - Z0(H+1)
C
C Last wire
            CALL GTFALH(H,TU,TQ,0,TW,XC,YC,ZC)
            ZPOS(H,L,1) = ZC - Z0(H+1)
C
          END DO
        END DO
C
        PRODFL = PRODUC()
        FIRST = .FALSE.
      END IF
C
      N_NEW = 0
C
      IF (.NOT.SAME) THEN
        SAVE_TRAK = 1
      END IF
C
      LFSEG0 = GZFSEG(HALF,0)
      LFSEG1 = GZFSEG(HALF,1)
C
      DO TRAK = SAVE_TRAK, NLADD
C
        DO I = 0,2
          TLADDER(I) = LADDER(I,TRAK)
          IF (TLADDER(I).EQ.0) MISS_LYR = I
        END DO
C
C Get segment information if missing layer is a theta so that slopes can
C be matched
        IF (MISS_LYR.EQ.0) THEN
          LOC = LZFIND(IXCOM,LFSEG1,LADDER(1,TRAK),-5)
          SLOPE = Q(LOC+30)
C
        ELSE IF (MISS_LYR.EQ.1) THEN
          LOC = LZFIND(IXCOM,LFSEG0,LADDER(0,TRAK),-5)
          SLOPE = Q(LOC+30)
        END IF
C
        TU = MISS_LYR/2
C
        TQ = 0
        IF (MISS_LYR.EQ.1) TQ = 4
        IF (MISS_LYR.LE.1) THEN
          TW = 7
        ELSE
          TW = 15
        END IF
C
C ****  Check on which sector overlaps the track containing the first and
C ****  second segments.
C Check at wire 0
        XPOS = QTRAK(4,TRAK)+QTRAK(7,TRAK)*ZPOS(HALF,MISS_LYR,0)
        YPOS = QTRAK(5,TRAK)+QTRAK(8,TRAK)*ZPOS(HALF,MISS_LYR,0)
        CALL FGET_SECTOR(XPOS,YPOS,HALF,MISS_LYR,TQ1,TS1)
        IF (TQ1.GE.0.AND.TS1.GE.0) THEN
          CALL FCODER(TLOGCHA1,HALF,TU,TQ1,TS1,0,0,2)
        ELSE
          TLOGCHA1 = -1
        END IF
C
C Check at last wire (7 or 15)
        XPOS = QTRAK(4,TRAK)+QTRAK(7,TRAK)*ZPOS(HALF,MISS_LYR,1)
        YPOS = QTRAK(5,TRAK)+QTRAK(8,TRAK)*ZPOS(HALF,MISS_LYR,1)
        CALL FGET_SECTOR(XPOS,YPOS,HALF,MISS_LYR,TQ2,TS2)
        IF (TQ2.NE.TQ1.OR.TS2.NE.TS1) THEN
          IF (TQ2.GE.0.AND.TS2.GE.0) THEN
            CALL FCODER(TLOGCHA2,HALF,TU,TQ2,TS2,0,0,2)
          END IF
        ELSE
          TLOGCHA2 = -1
        END IF
C
        IF (TLOGCHA1.EQ.-1.AND.TLOGCHA2.EQ.-1) GO TO 111
C
C ****  Find all segments in matching overlapping sectors.
C
        TL = MISS_LYR
        LSEG = GZFSEG(HALF,TL)
        IF (LSEG.LE.0) THEN              ! no segments in third layer
          GO TO 111
        END IF
        NSEG = NZBANK(IXCOM,LSEG)
        NTEST = 0
C
        LOC = LSEG
        DO 20 ISEG = 1,NSEG
          ISEG_LOGCHA = IQ(LOC+2)
          IF (ISEG_LOGCHA.EQ.TLOGCHA1.OR.ISEG_LOGCHA.EQ.TLOGCHA2) THEN
            IF (.NOT.BTEST(IQ(LOC),IUSED)) THEN
C
C Check slope of segment
              SLOPE_OK = .FALSE.
              IF (MISS_LYR.LE.1) THEN
                IF (ABS(Q(LOC+30)).LE.THETA_SLOPE_MAX) SLOPE_OK = .TRUE.
              ELSE
                IF (ABS(Q(LOC+55)).LE.PHI_SLOPE_MAX) SLOPE_OK = .TRUE.
              END IF
C
              IF (SLOPE_OK) THEN
C Check theta slopes if missing layer is 0 or 1
                IF ((MISS_LYR.LE.1.AND.ABS(SLOPE-Q(LOC+30)).LE.TOLSLOPE)
     &             .OR. MISS_LYR.EQ.2) THEN
                  IF (NTEST.LT.MXTSEG) THEN
                    NTEST = NTEST+1
                    TESTSG(NTEST) = ISEG
                  END IF
                END IF
              END IF
            END IF
          END IF
          LOC = LQ(LOC)
   20   CONTINUE
C
        DO ISEG = 1, NTEST
          N_NEW = N_NEW + 1
          LADDER_NEW(0,N_NEW) = LADDER(0,TRAK)
          LADDER_NEW(1,N_NEW) = LADDER(1,TRAK)
          LADDER_NEW(2,N_NEW) = LADDER(2,TRAK)
          LADDER_NEW(TL,N_NEW) = TESTSG(ISEG)
          IF (N_NEW.EQ.MXTRAK) GO TO 999
        END DO
C
        SAVE_TRAK1 = TRAK
C
  111   CONTINUE
      END DO
C
  999 CONTINUE
C
      SAVE_TRAK = SAVE_TRAK1
C
      IF ( .NOT.PRODFL ) THEN
        IF (N_NEW.EQ.MXTRAK ) THEN
          CALL ERRMSG('FDC-Too-Many-Ladders','FTHIRD_LAYER',
     &      ' number of potential FDC tracks greater than MXTRAK',
     &      'W')
        ENDIF
      ENDIF
C
      RETURN
C
      END
