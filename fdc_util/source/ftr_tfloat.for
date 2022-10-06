C VAX/DEC CMS REPLACEMENT HISTORY, Element FTR_TFLOAT.FOR
C *1     4-NOV-1993 10:59:03 AVERY "FDC changes for v12 RECO"
C VAX/DEC CMS REPLACEMENT HISTORY, Element FTR_TFLOAT.FOR
      SUBROUTINE FTR_TFLOAT(HALF,NLADD,LADDRS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Control FDC floating t0 track fitting
C-                              (for cosmic ray data).
C-
C-   Inputs  : HALF
C-             NLADD = Number of ladders
C-             LADDRS = Array of ladders
C-   Outputs : Fill track banks
C-
C-   Created  9-MAY-1993   Robert E. Avery   Based on Sue Blessing's
C-      FTTRAK.
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER HALF
      INTEGER IER
      INTEGER ILADD
      INTEGER LADDRS(0:2,MXTRAK)
      INTEGER LAYER
      INTEGER NLADD
      INTEGER NTRAK
      INTEGER TLADDER(0:2,MXTRAK)
      INTEGER TRAK
      INTEGER TSTAT(MXTRAK)
      INTEGER TLIST(MXTRAK)
      INTEGER NUSE_SEG(0:MXTRAK,0:2)   
C
      INTEGER IQTRAK(32),IQHSEC(3,34)
      REAL QTRAK(32),QHSEC(3,34)
      EQUIVALENCE (IQTRAK,QTRAK),(IQHSEC,QHSEC)
C
      INTEGER TIQTRAK(32,MXTRAK),TIQHSEC(3,34,MXTRAK)
      REAL TQTRAK(32,MXTRAK),TQHSEC(3,34,MXTRAK)
      EQUIVALENCE (TIQTRAK,TQTRAK),(TIQHSEC,TQHSEC)
C
      REAL CHIMAX
      REAL CHINORM,CHIN(MXTRAK)
C
      LOGICAL FIRST
      LOGICAL GOOD_TRACK
      LOGICAL FDC_TRACK_CHK
      LOGICAL USE_VERTEX
      SAVE FIRST,CHIMAX
      SAVE USE_VERTEX
C
      DATA FIRST/.TRUE./
      DATA USE_VERTEX/.FALSE./
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('CHIMAX',CHIMAX,IER)
        CALL EZRSET
        FIRST = .FALSE.
      END IF
C
C  Fit linked segments (ladders)
C
      NTRAK = 0
      DO ILADD = 1,NLADD
C
        CALL FIT_TRK_T0( HALF,LADDRS(0,ILADD),
     &        QTRAK,IQTRAK,QHSEC,IQHSEC,CHINORM)
C
        IF (CHINORM.LT.CHIMAX) THEN
          GOOD_TRACK = FDC_TRACK_CHK(HALF,LADDRS(0,ILADD),QTRAK)
        ELSE
          GOOD_TRACK = .FALSE.
        END IF
C
        IF (GOOD_TRACK) THEN
C
          NTRAK = NTRAK + 1
          TLIST(NTRAK) = NTRAK 
          CHIN(NTRAK) = CHINORM
          CALL UCOPY_i(LADDRS(0,ILADD),TLADDER(0,NTRAK),3)
          CALL UCOPY(QTRAK,TQTRAK(1,NTRAK),32)
          CALL UCOPY(QHSEC,TQHSEC(1,1,NTRAK),102)
C
          IF (NTRAK.EQ.MXTRAK) GO TO 111
        END IF
      END DO
  111 CONTINUE
C
      CALL VZERO(TSTAT,(MXTRAK))
      CALL VZERO(NUSE_SEG,MXTRAK*3)
      IF (NTRAK.GT.0) THEN
        CALL FSORT_TRACKS(NTRAK,TLIST,CHIN,NUSE_SEG,TLADDER,TSTAT)
C
C Save good tracks
C
        DO TRAK = 1, NTRAK
          IF (TSTAT(TRAK).EQ.1) THEN
            CALL LDFDCT_COS( TQTRAK(1,TRAK),
     &                       TQHSEC(1,1,TRAK),
     &                       HALF,
     &                       TLADDER(0,TRAK) )  ! Store track
          END IF
        END DO
      ENDIF
C---------------------------------------------------------------------
  999 CONTINUE
C
      RETURN
      END
