      SUBROUTINE FBTRAK(TLADDER,TPHI,TPHI_X0Y0,NTRAK,CHIN,NFIT,TSTAT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Select best FDC track
C-
C-   Inputs  : TLADDER = List of ladders used to make candidate tracks
C-             NTRAK   = Number of candidate tracks
C-             CHIN    = Normalized Chi of candidate tracks
C-             NFIT    = Number of points used in track fit
C-   Outputs : TSTAT   = Status of candidate track.
C-                     TSTAT(I)=1 for an accepted track.
C-
C-   Created  28-DEC-1990   Susan K. Blessing
C-   Updated  30-APR-1991   Jeffrey Bantly  use new PARAMS file
C-   Updated  28-MAY-1991   Susan K. Blessing  Completely change method
C-   Updated  30-AUG-1991   Susan K. Blessing  Use PHI from track and
C-    PHI from X0,Y0 to help eliminate bad tracks which don't come from
C-    the IR.
C-   Updated  22-OCT-1991   Susan K. Blessing  Add NFIT.  Only allow two layer
C-    tracks when delay lines are involved.
C-    Start to set up to allow segments with the average number of MIPs/hit
C-    above MIPCUT to be used twice.
C-   Updated  12-NOV-1991   Susan K. Blessing   Fix bug in filling NUSE_SEG
C-    array.
C-   Updated  19-NOV-1991   Susan K. Blessing   Check CHIN for two layer
C-    tracks against CHIMAX_2LAY before sorting.
C-   Updated  20-DEC-1991   Susan K. Blessing   Use NUSE_SEGS as the number 
C-    of times a segment has been used rather than number of times left for 
C-    use.  Will not use MIP_CUT for deciding if segments may be used more  
C-    than once.  All segments may be used more than once if allowed in RCP.
C-    This works about as well as cutting on the number of MIPs/segment and 
C-    is much faster.                                                       
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER I,J,K,L,T
      INTEGER IER
      INTEGER N1,N2,N3,N4,N5,N6        ! NUMBER OF TRACKS IN VARIOUS CATEGORIES
      INTEGER NFIT(MXTRAK)
      INTEGER NTRAK                    ! NUMBER OF POSSIBLE TRACKS
      INTEGER NTRAK_OLD
      INTEGER TLADDER(0:2,MXTRAK)      ! LIST OF LADDERS ON POSSIBLE TRACKS
      INTEGER TRAK
      INTEGER TSTAT(MXTRAK)            ! STATUS OF POSSIBLE TRACKS
      INTEGER TLIST_1(MXTRAK),TLIST_2(MXTRAK)
      INTEGER TLIST_3(MXTRAK),TLIST_4(MXTRAK)
      INTEGER TLIST_5(MXTRAK),TLIST_6(MXTRAK)
      INTEGER NUSE_SEG(0:MXTRAK,0:2)   ! NUMBER OF TIMES A SEGMENT HAS BEEN USED
C
      REAL CHIN(MXTRAK)                ! CHI NORM OF POSSIBLE TRACKS
      REAL CHIN_1(MXTRAK),CHIN_2(MXTRAK)
      REAL CHIN_3(MXTRAK),CHIN_4(MXTRAK)
      REAL CHIN_5(MXTRAK),CHIN_6(MXTRAK)
      REAL CHIMAX_2LAY
      REAL TPHI(MXTRAK),TPHI_X0Y0(MXTRAK)
      REAL PHI_DIFF,DIFF
C
      LOGICAL FIRST
      LOGICAL PASS
C
      SAVE FIRST,NTRAK_OLD,CHIMAX_2LAY
C
      DATA FIRST/.TRUE./
      DATA NTRAK_OLD/MXTRAK/
      DATA CHIMAX_2LAY/99./
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('PHI_DIFF',PHI_DIFF,IER)
        CALL EZGET('CHIMAX_2LAY',CHIMAX_2LAY,IER)
        CALL EZRSET
        FIRST = .FALSE.
      END IF
C
      IF (NTRAK.EQ.0) GO TO 999
C
      CALL VZERO(TSTAT,NTRAK_OLD)
      CALL VZERO(NUSE_SEG,(MXTRAK+1)*3)
C
      NTRAK_OLD = NTRAK
C
C Loop over all tracks to count how many layers were used and put tracks
C into categories.
C
C 1 = Three layer tracks which pass the angle cut
C 2 = Three layer tracks which fail the angle cut
C 3 = Two layer tracks which pass the angle cut and have more than zero DOF
C 4 = Two layer tracks which pass the angle cut and have zero DOF
C 5 = Two layer tracks which fail the angle cut and have more than zero DOF
C 6 = Two layer tracks which fail the angle cut and have zero DOF
C
      N1 = 0
      N2 = 0
      N3 = 0
      N4 = 0
      N5 = 0
      N6 = 0
C
      DO TRAK = 1, NTRAK
C
        DIFF = ABS(TPHI(TRAK) - TPHI_X0Y0(TRAK))
        IF (DIFF.GT.PI) DIFF = ABS(DIFF - TWOPI)
C
        IF (DIFF.LE.PHI_DIFF) THEN
          PASS = .TRUE.
        ELSE
          PASS = .FALSE.
        END IF
C
        IF (TLADDER(0,TRAK).GT.0.AND.
     &      TLADDER(1,TRAK).GT.0.AND.
     &      TLADDER(2,TRAK).GT.0) THEN
C
          IF (PASS) THEN
C
C 1 = Three layer tracks which pass the angle cut
            N1 = N1 + 1
            TLIST_1(N1) = TRAK
            CHIN_1(N1) = CHIN(TRAK)
          ELSE
C
C 2 = Three layer tracks which fail the angle cut
            N2 = N2 + 1
            TLIST_2(N2) = TRAK
            CHIN_2(N2) = CHIN(TRAK)
          END IF
        ELSE
C
C Check CHIN of 2 layer tracks before proceeding
          IF (CHIN(TRAK).LT.CHIMAX_2LAY) THEN
C
            IF (PASS) THEN
              IF (NFIT(TRAK).GT.4) THEN
C
C 3 = Two layer tracks which pass the angle cut and have more than zero DOF
                N3 = N3 + 1
                TLIST_3(N3) = TRAK
                CHIN_3(N3) = CHIN(TRAK)
              ELSE
C
C 4 = Two layer tracks which pass the angle cut and have zero DOF
                N4 = N4 + 1
                TLIST_4(N4) = TRAK
                CHIN_4(N4) = CHIN(TRAK)
              END IF
            ELSE
              IF (NFIT(TRAK).GT.4) THEN
C
C 5 = Two layer tracks which fail the angle cut and have more than zero DOF
                N5 = N5 + 1
                TLIST_5(N5) = TRAK
                CHIN_5(N5) = CHIN(TRAK)
              ELSE
C
C 6 = Two layer tracks which fail the angle cut and have zero DOF
                N6 = N6 + 1
                TLIST_6(N6) = TRAK
                CHIN_6(N6) = CHIN(TRAK)
              END IF
            END IF
          END IF
        END IF
C
      END DO
C
C 1 = Three layer tracks which pass the angle cut
      IF (N1.GT.0)
     &  CALL FSORT_TRACKS(N1,TLIST_1,CHIN_1,NUSE_SEG,TLADDER,TSTAT)
C
C 2 = Three layer tracks which fail the angle cut
      IF (N2.GT.0)
     &  CALL FSORT_TRACKS(N2,TLIST_2,CHIN_2,NUSE_SEG,TLADDER,TSTAT)
C
C 3 = Two layer tracks which pass the angle cut and have more than zero DOF
      IF (N3.GT.0)
     &  CALL FSORT_TRACKS(N3,TLIST_3,CHIN_3,NUSE_SEG,TLADDER,TSTAT)
C
C 5 = Two layer tracks which fail the angle cut and have more than zero DOF
      IF (N5.GT.0)
     &  CALL FSORT_TRACKS(N5,TLIST_5,CHIN_5,NUSE_SEG,TLADDER,TSTAT)
C
C 4 = Two layer tracks which pass the angle cut and have zero DOF
C Do not allow these tracks.
C      IF (N4.GT.0)
C     &  CALL FSORT_TRACKS(N4,TLIST_4,CHIN_4,NUSE_SEG,TLADDER,TSTAT)
C
C 6 = Two layer tracks which fail the angle cut and have zero DOF
C Do not allow these tracks.
C      IF (N6.GT.0)
C     &  CALL FSORT_TRACKS(N6,TLIST_6,CHIN_6,NUSE_SEG,TLADDER,TSTAT)
C
C--------------------------------------------------------------------------
  999 CONTINUE
C                                                         
      RETURN
      END
