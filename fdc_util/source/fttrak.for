      SUBROUTINE FTTRAK(HALF,NLADD,LADDRS,ZVTX,PMIN,PMAX,TMIN,TMAX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Control FDC first pass track fitting
C-
C-   Inputs  : HALF
C-             NLADD = Number of ladders
C-             LADDRS = Array of ladders
C-   Outputs : Fill track banks
C-
C-   Created  28-DEC-1990   Susan K. Blessing
C-   Updated  29-APR-1991   Jeffrey Bantly  use new RCP,PARAMS files
C-   Updated  16-MAY-1991   Robert E. Avery  Add FDC_TRACK_CHK, checks if
C-                              track goes through the right sectors
C-   Updated   6-JUN-1991   Susan K. Blessing  Try to add third layer to
C-    a successful two layer track right away.
C-   Updated  11-JUL-1991   Susan K. Blessing  Change size of LADDRS,NLADD
C-    arrays.  Don't need HALF information.
C-   Updated  29-AUG-1991   Robert E. Avery  Try out Segment fitting.
C-   Updated  14-OCT-1991   Susan K. Blessing  Change manner of making
C-    three layer tracks from two layer tracks so that more possibilities
C-    are saved.  Allow for separate chinorm cuts for two and three layer
C-    tracks.  Store chisq value from FIT_SEGTRK (if being used) for
C-    two layer tracks (all other track information is from FTFDCT).
C-   Updated  22-OCT-1991   Susan K. Blessing   Add NFIT, the number of
C-    points in the track fit, to the FBTRAK call.
C-   Updated   9-MAR-1992   Susan K. Blessing  Add USE_VERTEX=.FALSE.
C-    to the FTFDCT call.
C-   Updated  23-APR-1993   Susan K. Blessing   Change USE_VERTEX in
C-    FTFDCT call to IVERTEX=0 to indicate which primary vertex to use.
C-    (0 means no primary vertex.)
C-   Updated  26-APR-1993   Susan K. Blessing  Remove IQTRAK(25).  Loaded
C-    in FTFDCT.
C-   Updated  12-MAY-1993   Susan K. Blessing  Change call to include
C-    road.  Call FDC_IN_ROAD to check if track is in road before loading.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER I,J,K,L
      INTEGER HALF
      INTEGER IER
      INTEGER ILADD
      INTEGER LADDER(0:2)
      INTEGER LADDRS(0:2,MXTRAK)
      INTEGER LAYER
      INTEGER NFSEG
      INTEGER NFIT(MXTRAK)            ! Number of points used to fit track
      INTEGER NLADD
      INTEGER NTRAK
      INTEGER TLADDER(0:2,MXTRAK)
      INTEGER TRAK
      INTEGER TSTAT(MXTRAK)
      INTEGER IQTRAK(26),IQHSEC(3,34)
      INTEGER TIQTRAK(26,MXTRAK),TIQHSEC(3,34,MXTRAK)
      INTEGER IVERTEX
C
      REAL QTRAK(26),QHSEC(3,34)
      EQUIVALENCE (IQTRAK,QTRAK),(IQHSEC,QHSEC)
      REAL TQTRAK(26,MXTRAK),TQHSEC(3,34,MXTRAK)
      EQUIVALENCE (TIQTRAK,TQTRAK),(TIQHSEC,TQHSEC)
      REAL CHIMAX,CHIMAX_SEGTRK,CHIMAX_2LAY
      REAL CHINORM,CHIN(MXTRAK),CHIFSEG
      REAL RESID(34)
      REAL TPHI(MXTRAK),TPHI_X0Y0(MXTRAK)
      REAL X0,Y0,DX0,DY0
      REAL ZVTX,PMIN,PMAX,TMIN,TMAX
C
      LOGICAL THREE_LAYER
      LOGICAL FIRST
      LOGICAL TRKRES
      LOGICAL GOOD_TRACK
      LOGICAL FDC_TRACK_CHK
      LOGICAL USE_FIT_SEGTRK
      LOGICAL FDC_IN_ROAD
C
      SAVE FIRST,CHIMAX,CHIMAX_SEGTRK,CHIMAX_2LAY,TRKRES,USE_FIT_SEGTRK
      SAVE IVERTEX
C
      DATA FIRST/.TRUE./
      DATA TRKRES/.FALSE./
      DATA USE_FIT_SEGTRK /.FALSE./
      DATA IVERTEX/0/
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('CHIMAX',CHIMAX,IER)
        CHIMAX_SEGTRK = 2.*CHIMAX
        CALL EZGET('CHIMAX_SEGTRK',CHIMAX_SEGTRK,IER)
        CHIMAX_2LAY = CHIMAX
        CALL EZGET('CHIMAX_2LAY',CHIMAX_2LAY,IER)
        CALL EZGET('TRKRES',TRKRES,IER)
        CALL EZGET('USE_FIT_SEGTRK',USE_FIT_SEGTRK,IER)
        CALL EZRSET
        FIRST = .FALSE.
C
      END IF
C
      NTRAK = 0
C
C  Fit linked segments (ladders)
C
      DO ILADD = 1,NLADD
C
        THREE_LAYER = .TRUE.
        GOOD_TRACK = .FALSE.
C
        DO LAYER = 0,2
          LADDER(LAYER) = LADDRS(LAYER,ILADD)
          IF (LADDER(LAYER).EQ.0) THREE_LAYER = .FALSE.
        END DO
C
        IF ( USE_FIT_SEGTRK ) THEN
          CALL FIT_SEGTRK(HALF,LADDER,QTRAK,IQTRAK,CHINORM)
C
C Tracks which require a full fit are flagged with CHINORM=-9999.
          IF (CHINORM.EQ.-9999.) THEN
            CALL FTFDCT(HALF,LADDER,QTRAK,IQTRAK,QHSEC,
     &        IQHSEC,CHINORM,IVERTEX)
          END IF
C
C Cut on CHINORM for three layer tracks, no cut on two layer tracks.
C
          IF ((CHINORM.LT.CHIMAX_SEGTRK.AND.THREE_LAYER).OR.
     &        (.NOT.THREE_LAYER) ) THEN
C
            GOOD_TRACK = FDC_TRACK_CHK(HALF,LADDER,QTRAK)
C
            IF (GOOD_TRACK.AND.THREE_LAYER) THEN
              CALL FTFDCT(HALF,LADDER,QTRAK,IQTRAK,QHSEC,
     &          IQHSEC,CHINORM,IVERTEX)
              IF (CHINORM.LT.CHIMAX) THEN
                GOOD_TRACK = .TRUE.
              ELSE
                GOOD_TRACK = .FALSE.
              END IF
            END IF
          END IF
C
        ELSE
          CALL FTFDCT(HALF,LADDER,QTRAK,IQTRAK,QHSEC,
     &      IQHSEC,CHINORM,IVERTEX)
          IF ((CHINORM.LT.CHIMAX.AND.THREE_LAYER) .OR.
     &        (CHINORM.LT.CHIMAX_2LAY.AND..NOT.THREE_LAYER)) THEN
            GOOD_TRACK = FDC_TRACK_CHK(HALF,LADDER,QTRAK)
          ELSE
            GOOD_TRACK = .FALSE.
          END IF
        END IF
C
        IF (GOOD_TRACK) THEN
C
          NTRAK = NTRAK + 1
          CHIN(NTRAK) = CHINORM
          NFIT(NTRAK) = IQTRAK(2)
          TLADDER(0,NTRAK) = LADDER(0)
          TLADDER(1,NTRAK) = LADDER(1)
          TLADDER(2,NTRAK) = LADDER(2)
C
          DO I = 1, 26
            TQTRAK(I,NTRAK) = QTRAK(I)
          END DO
C
          IF (THREE_LAYER.OR..NOT.USE_FIT_SEGTRK) THEN
            TPHI(NTRAK) = QTRAK(6)
            DO I = 1, 34
              DO J = 1, 3
                TQHSEC(J,I,NTRAK) = QHSEC(J,I)
              END DO
            END DO
          ELSE
            DX0 = QTRAK(7)
            DY0 = QTRAK(8)
            TPHI(NTRAK) = ATAN2(DY0,DX0) - PI*FLOAT(HALF-1)
            IF (TPHI(NTRAK).LT.0.)
     &          TPHI(NTRAK) = TPHI(NTRAK) + TWOPI
          END IF
C
          X0 = QTRAK(4)
          Y0 = QTRAK(5)
          TPHI_X0Y0(NTRAK) = ATAN2(Y0,X0)
          IF (TPHI_X0Y0(NTRAK).LT.0.)
     &          TPHI_X0Y0(NTRAK) = TPHI_X0Y0(NTRAK) + TWOPI
C
          IF (NTRAK.EQ.MXTRAK) GO TO 111
C
        END IF
      END DO
C
  111 CONTINUE
C
C Try to make two layer tracks into three layer tracks
      CALL FTWO_TO_THREE(HALF,TLADDER,TQTRAK,TIQTRAK,TQHSEC,TIQHSEC,
     &  CHIN,NFIT,TPHI,TPHI_X0Y0,NTRAK)
C
      IF (NTRAK.GT.0)
     &  CALL FBTRAK(TLADDER,TPHI,TPHI_X0Y0,NTRAK,CHIN,NFIT,TSTAT)
C
C Save good tracks
C
      DO TRAK = 1, NTRAK
        IF (TSTAT(TRAK).EQ.1) THEN
C
          THREE_LAYER = .TRUE.
          GOOD_TRACK = .TRUE.
C
          DO LAYER = 0,2
            LADDER(LAYER) = TLADDER(LAYER,TRAK)
            IF (LADDER(LAYER).EQ.0) THREE_LAYER = .FALSE.
          END DO
C
          IF (THREE_LAYER.OR..NOT.USE_FIT_SEGTRK) THEN
            DO I = 1, 26
              QTRAK(I) = TQTRAK(I,TRAK)
            END DO
            GOOD_TRACK =
     &        FDC_IN_ROAD(HALF,QTRAK,ZVTX,PMIN,PMAX,TMIN,TMAX)
            IF (GOOD_TRACK) THEN
              DO I = 1, 34
                DO J = 1, 3
                  QHSEC(J,I) = TQHSEC(J,I,TRAK)
                END DO
              END DO
            END IF
          ELSE
            IF (CHIN(TRAK).GE.CHIMAX_2LAY) THEN
              GOOD_TRACK = .FALSE.
            ELSE
C Save number of points used in fit and chisq for fit.
              NFSEG = TIQTRAK(2,TRAK)
              CHIFSEG = TQTRAK(19,TRAK)
              CALL FTFDCT(HALF,LADDER,QTRAK,IQTRAK,QHSEC,
     &          IQHSEC,CHINORM,IVERTEX)
              GOOD_TRACK =
     &          FDC_IN_ROAD(HALF,QTRAK,ZVTX,PMIN,PMAX,TMIN,TMAX)
C Want to store chisq value from FIT_SEGTRK for two layer tracks.
              QTRAK(19) = CHIFSEG
              IQTRAK(25) = NFSEG
            END IF
          END IF
C
          IF (GOOD_TRACK) THEN
C Calculate correct residuals if requested.
            IF (TRKRES) THEN
              CALL FTRESID(HALF,LADDER,RESID)
              DO I = 1, 34
                QHSEC(3,I) = RESID(I)
              END DO
            END IF
            IQTRAK(1) = HALF
            CALL LDFDCT(QTRAK,QHSEC,HALF,LADDER)  ! Store track
C
          ENDIF
C
        END IF
      END DO
C---------------------------------------------------------------------
  999 CONTINUE
C
      RETURN
      END
