C+
      SUBROUTINE SATF4L (DIR, NPTMIN, NTRK, NTRMX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find tracks after magnet without using
C-                         time information. 
C-
C-   Inputs  : DIR - direction number,
C-             NPTMIN - minimum number of hits on track.
C-             NTRMX - maximum number of tracks.
C-   Outputs : NTRK - number of reconstructed tracks.
C-   Controls: none.
C-
C-   Created   2-JUN-1992   Alexander Efimov
C-   Updated  25-NOV-1993   Alexander Efimov   
C-   Updated  11-JAN-1995   Andrei Mayorov  cleanup - delete not used variables
C-                                                    & calls to GZ* routines 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER DIR, NPTMIN, NTRK, NTRMX
      INTEGER NPL
      PARAMETER (NPL=24)
      INTEGER GZSATW, GZSTNA, GZSTSA
      INTEGER HITS, NHIT, LINK, WORK
      COMMON /COMSAFCN4/ ZMIN, ZMAX, ROAD, ROAD2, 
     &                   MONITOR, HADR(3,NPL)
      REAL    ZMIN, ZMAX, ROAD, ROAD2
      INTEGER MONITOR, HADR
      REAL    LINE(6), DIST2, ZZ, W, W1, W2
      INTEGER N_PAR
      PARAMETER (N_PAR=4)
      REAL    PAR(N_PAR), STEP_PAR(N_PAR), FUN(100)
      INTEGER PL, PL1, PL2, PL3, PL4
      INTEGER J1, J2, J3, J4, L1, L2, L3, L4
      INTEGER JC1, JC2, JC3, JC4
      INTEGER LD1, LD2, LD3, LD4, JTR, LT, IERR
      INTEGER NN, LL, J, JJ, LD, JC, MON, MON_OLD, OK
      INTEGER NHTMX
      REAL    RDRTB
      SAVE    NHTMX, RDRTB
      EXTERNAL SAFCN4
      LOGICAL FIRST
      SAVE    FIRST
      DATA    FIRST /.TRUE./
C
C ****  Initialyzing
C
      HITS = 1 + GZSATW()
      IF (DIR .EQ. 1) THEN
        JTR = 1 + GZSTNA()
      ELSE
        JTR = 1 + GZSTSA()
      END IF
      IF (FIRST) THEN
        CALL EZPICK ('SAMUS_UTIL_PARAM')
        CALL EZGET ('NHTMX', NHTMX, IERR)
        CALL EZGET ('RDRTB', RDRTB, IERR)
        CALL EZRSET
        FIRST = .FALSE.
      END IF
      LINK = HITS + NHTMX * NPL
      NHIT = LINK + NHTMX * NPL
      WORK = NHIT + NPL + 1
      IF (NPTMIN .LT. 4) GO TO 999
      IF (NTRK .GE. NTRMX) GO TO 999
      ROAD = RDRTB
      ROAD2 = ROAD * ROAD
C
C ****  base planes and hits
C
      PL1 = IQ(WORK+1)
      L1 = IQ(WORK+2)
      PL2 = IQ(WORK+3)
      L2 = IQ(WORK+4)
      PL3 = IQ(WORK+5)
      L3 = IQ(WORK+6)
      PL4 = IQ(WORK+7)
      L4 = IQ(WORK+8)
      J1 = (PL1 - 1) * NHTMX
      HADR(1,1) = PL1
      J2 = (PL2 - 1) * NHTMX
      HADR(1,2) = PL2
      J3 = (PL3 - 1) * NHTMX
      HADR(1,3) = PL3
      J4 = (PL4 - 1) * NHTMX
      HADR(1,4) = PL4
      LD1 = IQ(HITS+J1+L1)
      JC1 = IQ(LD1+3)
      HADR(2,1) = L1
      HADR(3,1) = LD1
      LD2 = IQ(HITS+J2+L2)
      JC2 = IQ(LD2+3)
      HADR(2,2) = L2
      HADR(3,2) = LD2
      LD3 = IQ(HITS+J3+L3)
      JC3 = IQ(LD3+3)
      HADR(2,3) = L3
      HADR(3,3) = LD3
      LD4 = IQ(HITS+J4+L4)
      JC4 = IQ(LD4+3)
      HADR(2,4) = L4
      HADR(3,4) = LD4
C
C ****  Find parameters of the track passing trough 4 base hits
C
      CALL SACR4L (C(JC1+1), C(JC2+1), C(JC3+1), C(JC4+1), LINE, OK)
      IF (OK .LE. 0) GO TO 999
      CALL SADS2L (LINE, C(JC1+1), DIST2, W1, W2, OK)
      IF (OK .LE. 0 .OR. ABS(W2) .GT. C(JC1+7)) GO TO 999
      CALL SADS2L (LINE, C(JC2+1), DIST2, W1, W2, OK)
      IF (OK .LE. 0 .OR. ABS(W2) .GT. C(JC2+7)) GO TO 999
      CALL SADS2L (LINE, C(JC3+1), DIST2, W1, W2, OK)
      IF (OK .LE. 0 .OR. ABS(W2) .GT. C(JC3+7)) GO TO 999
      CALL SADS2L (LINE, C(JC4+1), DIST2, W1, W2, OK)
      IF (OK .LE. 0 .OR. ABS(W2) .GT. C(JC4+7)) GO TO 999
C
C ****  Find all hits on base track
C
      MON = 4
      DO 301 PL = 1, NPL
        IF (PL .EQ. PL1 .OR. PL .EQ. PL2) GO TO 301
        IF (PL .EQ. PL3 .OR. PL .EQ. PL4) GO TO 301
        NN = IQ(NHIT+PL)
        JJ = (PL - 1) * NHTMX
        DO LL = 1, NN
          IF (IQ(LINK+JJ+LL) .EQ. 0) THEN
            LD = IQ(HITS+JJ+LL)
            JC = IQ(LD+3)
            CALL SADS2L (LINE, C(JC+1), DIST2, W1, W2, OK)
            IF (OK .GT. 0 .AND. ABS(W2) .LE. C(JC+7) .AND.
     &          DIST2 .LT. ROAD2) THEN
              MON = MON + 1
              HADR(1,MON) = PL
              HADR(2,MON) = LL
              HADR(3,MON) = LD
              GO TO 301
            END IF
          END IF
        END DO
  301 CONTINUE
      IF (MON .LT. NPTMIN) GO TO 999
C
C ****  Fit all hits
C
  302 CONTINUE
      ZMIN = +1.0E+37
      ZMAX = -1.0E+37
      DO J = 1, MON
        LD = HADR(3,J)
        JC = IQ(LD+3)
        ZZ = C(JC+3)
        IF (ZZ .LT. ZMIN) ZMIN = ZZ
        IF (ZZ .GT. ZMAX) ZMAX = ZZ
      END DO
      W = (ZMIN - LINE(3)) / LINE(6)
      PAR(1) = LINE(1) + W * LINE(4)
      PAR(2) = LINE(2) + W * LINE(5)
      W = (ZMAX - LINE(3)) / LINE(6)
      PAR(3) = LINE(1) + W * LINE(4)
      PAR(4) = LINE(2) + W * LINE(5)
      MONITOR = MON
      DO J = 1, N_PAR
        STEP_PAR(J) = 0.5 * ROAD
      END DO
      CALL SAMNSQ (SAFCN4, MONITOR, N_PAR, FUN, PAR, STEP_PAR)
      LINE(1) = PAR(1)
      LINE(2) = PAR(2)
      LINE(3) = ZMIN
      LINE(4) = PAR(3) - PAR(1)
      LINE(5) = PAR(4) - PAR(2)
      LINE(6) = ZMAX - ZMIN
      W = 1.0 / SQRT (LINE(4)**2 + LINE(5)**2 + LINE(6)**2)
      LINE(4) = LINE(4) * W
      LINE(5) = LINE(5) * W
      LINE(6) = LINE(6) * W
C
C ****  Search new points on track after fit procedure
C
      MON_OLD = MON
      DO 303 PL = 1, NPL
        IF (PL .EQ. PL1 .OR. PL .EQ. PL2) GO TO 303
        IF (PL .EQ. PL3 .OR. PL .EQ. PL4) GO TO 303
        DO LL = 1, MON
          IF (HADR(1,LL) .EQ. PL) GO TO 303
        END DO
        NN = IQ(NHIT+PL)
        JJ = (PL - 1) * NHTMX
        DO LL = 1, NN
          IF (IQ(LINK+JJ+LL) .EQ. 0) THEN
            LD = IQ(HITS+JJ+LL)
            JC = IQ(LD+3)
            CALL SADS2L (LINE, C(JC+1), DIST2, W1, W2, OK)
            IF (OK .GT. 0 .AND. ABS(W2) .LE. C(JC+7) .AND.
     &          DIST2 .LT. ROAD2) THEN
              MON = MON + 1
              HADR(1,MON) = PL
              HADR(2,MON) = LL
              HADR(3,MON) = LD
              GO TO 303
            END IF
          END IF
        END DO
  303 CONTINUE
      IF (MON .NE. MON_OLD) GO TO 302
C
C ****  Put information to the output banks
C
      IF (MON .LT. NPTMIN) GO TO 999
      NTRK = NTRK + 1
      LT = JTR + (NTRK - 1) * 64
      IQ(LT+1) = MON
      DO J = 1, 6
        Q(LT+J+1) = LINE(J)
      END DO
      Q(LT+8) = 0.0
      NN = LT + 8
      DO J = 1, MON
        NN = NN + 1
        IQ(NN) = HADR(3,J)
        PL = HADR(1,J)
        LL = HADR(2,J)
        JJ = (PL - 1) * NHTMX + LL
        IQ(LINK+JJ) = NTRK
      END DO
      IF (NTRK .GE. NTRMX) GO TO 999
C
  999 CONTINUE
      IQ(JTR) = NTRK
      RETURN
      END
