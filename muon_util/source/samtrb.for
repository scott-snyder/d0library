C+
      SUBROUTINE SAMTRB (DIR, NTRK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : S/R for tracks reconstruction before magnet.
C-
C-   Inputs  : DIR - direction number.
C-   Outputs : NTRK - number of tracks.
C-   Controls: none.
C-
C-   Created   9-SEP-1992   Alexander Efimov
C-   Updated   6-FEB-1994   Alexander Efimov   
C-   Updated  11-JAN-1995   Andrei Mayorov  clean up unused variables and calls
C-                                          to GZ* 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER DIR, NTRK
      INTEGER NPL, N_PLANES
      PARAMETER (NPL=12, N_PLANES=3)
      INTEGER MTRK, NPTMIN
      INTEGER HITS, LINK, NHIT, WORK
      INTEGER LSAHS
      INTEGER GZSAHS, GZSATW, GZSTNB, GZSTSB
      INTEGER LD, IND, NST, IPL
      INTEGER I, J, K, L, N
      INTEGER PL, LT, NT, JTR
      REAL    DRFMX, DIST
      INTEGER NHTMX, NTRMX, NHTBB, NHTMB
      SAVE    NHTMX, NTRMX, NHTBB, NHTMB, DRFMX
      INTEGER IERR
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST /.TRUE./
C
C ****  define constants
C
      IF (FIRST) THEN
        CALL EZPICK ('SAMUS_UTIL_PARAM')
        CALL EZGET ('NHTMX', NHTMX, IERR)
        CALL EZGET ('NTRMX', NTRMX, IERR)
        CALL EZGET ('NHTBB', NHTBB, IERR)
        CALL EZGET ('NHTMB', NHTMB, IERR)
        CALL EZGET ('DRFMX', DRFMX, IERR)
        CALL EZRSET
        FIRST = .FALSE.
      END IF
      LSAHS = GZSAHS()
      HITS = 1 + GZSATW()
      IF (DIR .EQ. 1) THEN
        JTR = 1 + GZSTNB()
      ELSE
        JTR = 1 + GZSTSB()
      END IF
      NST = 3 * DIR - 2
C
C ****  tracks reconstruction without time information
C
      LINK = HITS + NHTMX * NPL
      NHIT = LINK + NHTMX * NPL
      WORK = NHIT + NPL + 1
      DO PL = 1, NPL
        IQ(NHIT+PL) = 0       ! reset number of hits in planes
      END DO
      DO IPL = 1, N_PLANES
        IND = 3 * (NST - 1) + IPL
        N = IQ(LSAHS+IND)           ! number of hits in plane
        LD = LQ(LSAHS-IND)          ! hits address
        DO I = 1, N                                 ! hits loop
          IF (IQ(LD+1) .EQ. 2) THEN                 ! good hit
            CALL SATRPL (LD, PL)
            L = IQ(NHIT+PL)                 ! number of hits in plane
            L = L + 1
            IQ(NHIT+PL) = L
            K = (PL - 1) * NHTMX + L
            IQ(HITS+K) = LD
            IQ(LINK+K) = 0
          END IF
          LD = LD + 13
        END DO
      END DO
      NTRK = 0
      IQ(WORK) = 0
      NPTMIN = 8
  100 CONTINUE
      MTRK = NTRK
      CALL SATR2L (DIR, NPTMIN, NTRK, NTRMX, 1)
      IF (NTRK .GE. NTRMX) GO TO 200
      IF (NTRK .GT. MTRK) THEN
        NPTMIN = NPTMIN - 1
        IF (NPTMIN .LT. NHTBB) GO TO 200
        GO TO 100
      END IF
      NPTMIN = IQ(WORK)
      IF (NPTMIN .LT. NHTBB) GO TO 200
      CALL SATF2L (DIR, NPTMIN, NTRK, NTRMX, 1)
      IQ(WORK) = 0
      IF (NTRK .GE. NTRMX) GO TO 200
      GO TO 100
  200 CONTINUE
C
C ****  finding track after magnet with time information
C
      LINK = HITS + NPL
      NHIT = LINK + NPL
      NPTMIN = NHTMB
      DO NT = 1, NTRK
        LT = JTR + (NT - 1) * 64
        DO PL = 1, NPL
          IQ(NHIT+PL) = 0       ! reset number of hits in planes
        END DO
        N = IQ(LT+1)                    ! number of tubes on track
        DO J = 1, N
          LD = IQ(LT+J+8)                     ! hit address
          DIST = Q(LD+4)
          IF (DIST .LT. DRFMX) THEN
            CALL SATRPL (LD, PL)
            IQ(HITS+PL) = LD
            IQ(NHIT+PL) = 1
          END IF
        END DO
        CALL SATC2L (DIR, NPTMIN, N, NT)
      END DO
C
      RETURN
      END
