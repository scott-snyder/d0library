C+
      SUBROUTINE SAL3BM (DIR, NTRK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : S/R for tracks reconstruction after magnet.
C-
C-   Inputs  : DIR - direction number.
C-   Outputs : NTRK - number of tracks.
C-   Controls: none.
C-
C-   Created   9-SEP-1992   Alexander Efimov
C-   Updated  12-JAN-1994   Alexander Efimov   
C-   Updated   4-FEB-1994   Alexei Volkov   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER DIR, NTRK
      INTEGER NPL, N_PLANES
      PARAMETER (NPL=12, N_PLANES=3)
      INTEGER MHITMX, NPTMIN, HITS, LINK, NHIT, WORK
      INTEGER LSAHS, GZSAHS, GZSATW
      INTEGER LD, IND, IST, IPL, NST
      INTEGER I, J, K, L, N, PL, MTRK
      REAL    DIST
      INTEGER NHTMX, NHTBB
      SAVE    NHTMX, NHTBB
      LOGICAL FIRST
      SAVE FIRST
      INTEGER IERR
      DATA FIRST /.TRUE./
C
C ****  define constants
C
      IF (FIRST) THEN
        CALL EZPICK ('SAMUS_UTIL_PARAM')
        CALL EZGET ('NHTMX', NHTMX, IERR)
        CALL EZGET ('NHTBB', NHTBB, IERR)
        CALL EZRSET
        FIRST = .FALSE.
      END IF
      LSAHS = GZSAHS()
      HITS = 1 + GZSATW()
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
      NPTMIN = 6
  100 CONTINUE
      MTRK = NTRK
      CALL SATR2L (DIR, NPTMIN, NTRK, 1, 0)
      IF (NTRK .GE. 1) GO TO 200
      IF (NTRK .GT. MTRK) THEN
        NPTMIN = NPTMIN - 1
        IF (NPTMIN .LT. NHTBB) GO TO 200
        GO TO 100
      END IF
      NPTMIN = IQ(WORK)
      IF (NPTMIN .LT. NHTBB) GO TO 200
      CALL SATF2L (DIR, NPTMIN, NTRK, 1, 0)
      IQ(WORK) = 0
      IF (NTRK .GE. 1) GO TO 200
      GO TO 100
  200 CONTINUE
C
      RETURN
      END
