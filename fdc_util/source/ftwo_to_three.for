      SUBROUTINE FTWO_TO_THREE(HALF,TLADDER,TQTRAK,TIQTRAK,
     &  TQHSEC,TIQHSEC,CHIN,NFIT,TPHI,TPHI_X0Y0,NTRAK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make three layer ladders from successful
C-    two layer tracks using overlapping sectors.
C-
C-   Inputs  : HALF
C-             TLADDER
C-             TQTRAK
C-             TIQTRAK
C-             TQHSEC
C-             TIQHSEC
C-             CHIN
C-             TPHI
C-             TPHI_X0Y0
C-             NTRAK
C-
C-   Outputs : TLADDER
C-             TQTRAK
C-             TIQTRAK
C-             TQHSEC
C-             TIQHSEC
C-             CHIN
C-             TPHI
C-             TPHI_X0Y0
C-             NTRAK
C-   Controls:
C-
C-   Created   5-SEP-1991   Susan K. Blessing
C-   Updated  22-OCT-1991   Susan K. Blessing   Add NFIT, the number of points
C-    used in the track fit.
C-   Updated  17-DEC-1991   Susan K. Blessing  Three layer tracks must
C-    pass FDC_TRACK_CHK.
C-   Updated  17-DEC-1991   Susan K. Blessing  Add RCP options for trying
C-    to make three layer tracks from theta-theta, inner theta-phi, phi-outer
C-    theta tracks.  Add entry point FTWO_TO_THREE_FIT_ALL to allow for
C-    overridding RCP options when FTWO_TO_THREE is called for ladders
C-    built by FLINSG (called by FLINSG and FOVERLAD).
C-   Updated   9-MAR-1992   Susan K. Blessing  Add USE_VERTEX=.FALSE.
C-    to the FTFDCT call.
C-   Updated  16-APR-1993   Susan K. Blessing  Call FTHIRD_LAYER multiple
C-    times if MXTRAK limit was reached.
C-   Updated  23-APR-1993   Susan K. Blessing  Change USE_VERTEX in FTFDCT
C-    call to IVERTEX=0 to indicate which vertex to use (none).
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER I,J
      INTEGER N2,N3
      INTEGER HALF
      INTEGER NTRAK,TRAK
      INTEGER NFIT(MXTRAK)
      INTEGER LAD(0:2,MXTRAK)
      INTEGER LADDR(0:2,MXTRAK)
      INTEGER LADDER(0:2)
      INTEGER TLADDER(0:2,MXTRAK)
      INTEGER IQTRAK(26),IQHSEC(3,34)
      INTEGER TIQTRAK(26,MXTRAK),TIQHSEC(3,34,MXTRAK)
      INTEGER IER
      INTEGER IVERTEX
C
      REAL NQTRAK(26,MXTRAK)
      REAL QTRAK(26),QHSEC(3,34)
      REAL TQTRAK(26,MXTRAK),TQHSEC(3,34,MXTRAK)
      EQUIVALENCE (IQTRAK,QTRAK),(IQHSEC,QHSEC)
      REAL CHIMAX,CHIMAX_SEGTRK
      REAL CHINORM
      REAL CHIN(MXTRAK)
      REAL TPHI(MXTRAK),TPHI_X0Y0(MXTRAK)
      REAL X0,Y0
C
      LOGICAL FIRST,SUCCESS
      LOGICAL FDC_TRACK_CHK
      LOGICAL USE_FIT_SEGTRK
      LOGICAL GOOD_TRACK
      LOGICAL TT_TO_3LAYER,PT_TO_3LAYER,TP_TO_3LAYER
      LOGICAL THREE_LAYER,FIT_ALL,FIT_ALL_DUMMY
      LOGICAL AGAIN
C
      SAVE FIRST,CHIMAX,CHIMAX_SEGTRK,USE_FIT_SEGTRK
      SAVE TT_TO_3LAYER,PT_TO_3LAYER,TP_TO_3LAYER
      SAVE IVERTEX
C
      DATA FIRST/.TRUE./
      DATA USE_FIT_SEGTRK/.FALSE./
      DATA TT_TO_3LAYER,PT_TO_3LAYER,TP_TO_3LAYER/3*.TRUE./
      DATA FIT_ALL/.TRUE./
      DATA IVERTEX/0/
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('CHIMAX',CHIMAX,IER)
        CHIMAX_SEGTRK = 2.*CHIMAX
        CALL EZGET('CHIMAX_SEGTRK',CHIMAX_SEGTRK,IER)
        CALL EZGET('USE_FIT_SEGTRK',USE_FIT_SEGTRK,IER)
        CALL EZGET('TT_TO_3LAYER',TT_TO_3LAYER,IER)
        CALL EZGET('PT_TO_3LAYER',PT_TO_3LAYER,IER)
        CALL EZGET('TP_TO_3LAYER',TP_TO_3LAYER,IER)
        CALL EZRSET
        FIRST = .FALSE.
      END IF
C
      N2 = 0
C
      DO TRAK = 1, NTRAK
C
        THREE_LAYER = .TRUE.
        IF (TLADDER(0,TRAK)*TLADDER(1,TRAK)*TLADDER(2,TRAK).EQ.0)
     &    THREE_LAYER = .FALSE.
C
        IF ( (.NOT.THREE_LAYER.AND.FIT_ALL) .OR.
     &       (TLADDER(0,TRAK).EQ.0.AND.PT_TO_3LAYER) .OR.
     &       (TLADDER(1,TRAK).EQ.0.AND.TP_TO_3LAYER) .OR.
     &       (TLADDER(2,TRAK).EQ.0.AND.TT_TO_3LAYER) ) THEN
C
          N2 = N2 + 1
          LAD(0,N2) = TLADDER(0,TRAK)
          LAD(1,N2) = TLADDER(1,TRAK)
          LAD(2,N2) = TLADDER(2,TRAK)
C
          DO I = 1, 26
            NQTRAK(I,N2) = TQTRAK(I,TRAK)
          END DO
C
        END IF
      END DO
C
      AGAIN = .FALSE.
      N3 = 0
      DO WHILE (N3.EQ.0.OR.N3.EQ.MXTRAK)
C
        CALL FTHIRD_LAYER(HALF,LAD,NQTRAK,N2,LADDR,N3,AGAIN)
        AGAIN = .TRUE.
C
C Make a track fit of new three layer ladders.
C
        DO TRAK = 1, N3
C
          LADDER(0) = LADDR(0,TRAK)
          LADDER(1) = LADDR(1,TRAK)
          LADDER(2) = LADDR(2,TRAK)
C
          GOOD_TRACK = .FALSE.
C
          IF ( USE_FIT_SEGTRK ) THEN
            CALL FIT_SEGTRK(HALF,LADDER,QTRAK,IQTRAK,CHINORM)
C
            IF (CHINORM.LT.CHIMAX_SEGTRK) THEN
C
              GOOD_TRACK = FDC_TRACK_CHK(HALF,LADDER,QTRAK)
C
              IF (GOOD_TRACK)
     &          CALL FTFDCT(HALF,LADDER,QTRAK,IQTRAK,QHSEC,
     &          IQHSEC,CHINORM,IVERTEX)
C
            END IF
          ELSE
            CALL FTFDCT(HALF,LADDER,QTRAK,IQTRAK,QHSEC,
     &        IQHSEC,CHINORM,IVERTEX)
            GOOD_TRACK = FDC_TRACK_CHK(HALF,LADDER,QTRAK)
          ENDIF
C
          IF (GOOD_TRACK.AND.CHINORM.LT.CHIMAX) THEN
C
            NTRAK = NTRAK + 1
            CHIN(NTRAK) = CHINORM
            NFIT(NTRAK) = IQTRAK(2)
            TLADDER(0,NTRAK) = LADDER(0)
            TLADDER(1,NTRAK) = LADDER(1)
            TLADDER(2,NTRAK) = LADDER(2)
            DO I = 1, 26
              TQTRAK(I,NTRAK) = QTRAK(I)
            END DO
            DO I = 1, 34
              DO J = 1, 3
                TQHSEC(J,I,NTRAK) = QHSEC(J,I)
              END DO
            END DO
C
            TPHI(NTRAK) = QTRAK(6)
            X0 = QTRAK(4)
            Y0 = QTRAK(5)
            TPHI_X0Y0(NTRAK) = ATAN2(Y0,X0)
            IF (TPHI_X0Y0(NTRAK).LT.0.)
     &        TPHI_X0Y0(NTRAK) = TPHI_X0Y0(NTRAK) + TWOPI
C
            IF (NTRAK.EQ.MXTRAK) GO TO 999
          END IF
C
        END DO
C
        IF (N3.EQ.0) N3 = -1
C
      END DO
C
  999 CONTINUE
C
      RETURN
C-----------------------------------------------------------------------
      ENTRY FTWO_TO_THREE_FIT_ALL(FIT_ALL_DUMMY)
C
      FIT_ALL = FIT_ALL_DUMMY
C
      RETURN
      END
