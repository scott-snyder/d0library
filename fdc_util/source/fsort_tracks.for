      SUBROUTINE FSORT_TRACKS(N,TLIST,CHIN,NUSE_SEG,TLADDER,TSTAT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sort a list of tracks by their normalized 
C-    CHI and choose the best ones.
C-
C-   Inputs  : N          = Number of tracks
C-             TLIST = 
C-             CHIN       = List of normalized chi values
C-             NUSE_SEG   = List of number of times a segment may be used
C-             TLADDER    = List of ladders on possible tracks
C-   Outputs : TSTAT   = Status of candidate track.
C-                       TSTAT(I)=1 for an accepted track.
C-   Controls: 
C-
C-   Created  22-OCT-1991   Susan K. Blessing
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER L,L1,LTWICE
      INTEGER IER
      INTEGER T,TRAK,T1,TRAK1
      INTEGER N,N_OLD
      INTEGER NUSE_SEG(0:MXTRAK,0:2)    ! NUMBER OF TIMES A SEGMENT HAS BEEN
                                        ! USED
      INTEGER SEG(0:2)
      INTEGER TLADDER(0:2,MXTRAK)       ! LIST OF LADDERS ON POSSIBLE TRACKS
      INTEGER TLIST(MXTRAK)
      INTEGER TSTAT(MXTRAK)             ! STATUS OF POSSIBLE TRACKS
      INTEGER TPOINT(MXTRAK)
      INTEGER NALLOW_SEG
C
      REAL CHIN(MXTRAK)                 ! CHI NORM OF POSSIBLE TRACKS
C
      LOGICAL FIRST
      LOGICAL GOOD_TRACK,THREE_LAYER
      LOGICAL OFF
C
      SAVE FIRST,NALLOW_SEG
C
      DATA FIRST/.TRUE./
      DATA NALLOW_SEG/1/
      DATA N_OLD/MXTRAK/
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('NALLOW_SEG',NALLOW_SEG,IER)
        CALL EZRSET
        FIRST = .FALSE.
      END IF
C
      DO T = 1, N_OLD
        TPOINT(T) =  T
      END DO
C
      N_OLD = N
C
C Sort list of chi norms, SRTFLT sorts into ascending order
      CALL SRTFLT(CHIN,N,TPOINT)
C
      DO T = 1, N
C
        TRAK = TLIST(TPOINT(T))
        GOOD_TRACK = .TRUE.
        LTWICE = -1
        OFF = .FALSE.
C
        SEG(0) = TLADDER(0,TRAK)
        SEG(1) = TLADDER(1,TRAK)
        SEG(2) = TLADDER(2,TRAK)
C
        IF (SEG(0)*SEG(1)*SEG(2).EQ.0) THEN
          THREE_LAYER = .FALSE.
        ELSE
          THREE_LAYER = .TRUE.
        END IF
C
        IF (THREE_LAYER) THEN
C Only allow one segment of three to be used more than once
          DO L = 0, 2
            IF (NUSE_SEG(SEG(L),L).GT.0) THEN
C Do not use segments more than NALLOW_SEG times
              IF (NUSE_SEG(SEG(L),L).GE.NALLOW_SEG) THEN
                GOOD_TRACK = .FALSE.
                OFF = .FALSE.
                GO TO 100
              ELSE
                OFF = .TRUE.
                LTWICE = L
                DO L1 = L+1, 2
                  IF (NUSE_SEG(SEG(L1),L1).GT.0) THEN
                    GOOD_TRACK = .FALSE.
                    OFF = .FALSE.
                    GO TO 100
                  ELSE
                    OFF = .TRUE.
                  END IF
                END DO
              END IF
            END IF
          END DO
C
  100     CONTINUE
C
        ELSE
C Do not allow any segments on a two layer ladder to be used more than once
          DO L = 0, 2
            IF (SEG(L).GT.0.AND.NUSE_SEG(SEG(L),L).GT.0) THEN
              GOOD_TRACK = .FALSE.
              GO TO 200
            END IF
          END DO
C
  200     CONTINUE
C
        END IF
C
        IF (GOOD_TRACK) THEN
C Keep track
          TSTAT(TRAK) = 1
C
C Increment number of times segments have been used
          DO L = 0, 2
            NUSE_SEG(SEG(L),L) = NUSE_SEG(SEG(L),L) + 1
          END DO
C
          IF (OFF) THEN
            DO L = 0, 2
              IF (L.NE.LTWICE) THEN
                NUSE_SEG(SEG(L),L) = 99
              END IF
            END DO
C
C Turn off segments in tracks which have already been accepted.
            DO T1 = 1, N-1
              TRAK1 = TLIST(TPOINT(T1))
              IF (TSTAT(TRAK1).EQ.1) THEN
                IF (SEG(LTWICE).EQ.TLADDER(LTWICE,TRAK1)) THEN
                  DO L = 0, 2
                    IF (L.NE.LTWICE) THEN
                      NUSE_SEG(TLADDER(L,TRAK1),L) = 99
                    END IF
                  END DO
                END IF
              END IF
            END DO
C
          END IF
C
        ELSE
C Discard track
          TSTAT(TRAK) = -1
        END IF

      END DO
C
  999 RETURN
      END
