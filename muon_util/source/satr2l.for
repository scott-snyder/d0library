C+
      SUBROUTINE SATR2L (DIR, NPTMIN, NTRK, NTRMX, KEYR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find tracks before magnet without using
C-                         time information.
C-
C-   Inputs  : DIR - direction number,
C-             NPTMIN - minimum number of hits on track.
C-             NTRMX - maximum number of tracks,
C-             KEYR = 0 for reconstruction without vertex,
C-                      else reconstruction with vertex coordinates.
C-   Outputs : NTRK - number of reconstructed tracks.
C-   Controls: none.
C-
C-   Created   2-JUN-1992   Alexander Efimov
C-   Updated  10-DEC-1993   Alexander Efimov   
C-   Updated   4-FEB-1994   Alexei Volkov   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER DIR, NPTMIN, NTRK, NTRMX, KEYR
      INTEGER NPL
      PARAMETER (NPL=12)
      INTEGER LMTRH, LSAMT, LSAHS
      INTEGER GZMTRH, GZSAMT, GZSAHS, GZSATW, GZSTNB, GZSTSB
      INTEGER HITS, NHIT, LINK, WORK, NV, IVER
      COMMON /COMSAFCN2/ ZMIN, ZMAX, ROAD, ROAD2, VERTEX(3),
     &                   MONITOR, HADR(3,NPL)
      REAL    ZMIN, ZMAX, ROAD, ROAD2, VERTEX
      INTEGER MONITOR, HADR
      REAL    LINE(6), DIST2, ZZ, W, W1, W2
      INTEGER N_PAR
      PARAMETER (N_PAR=4)
      REAL    PAR(N_PAR), STEP_PAR(N_PAR), FUN(100)
      INTEGER PL, PL1, PL2
      INTEGER N1, N2, J1, J2, L1, L2
      INTEGER K1, K2, JC1, JC2
      INTEGER LD1, LD2, JTR, LT, IERR
      INTEGER M, N, K, NN, LL, J, JJ, LD, JC, MON, MON_OLD, OK
      INTEGER NHTMX
      REAL    RDRTB, XBEAM, YBEAM, ZBEAM
      SAVE    NHTMX
      SAVE    RDRTB, XBEAM, YBEAM, ZBEAM
      EXTERNAL SAFCN2
      LOGICAL FIRST
      SAVE    FIRST
      DATA    FIRST /.TRUE./
C
C ****  Initialyzing
C
      LMTRH = GZMTRH()
      LSAMT = GZSAMT()
      LSAHS = GZSAHS()
      HITS = 1 + GZSATW()
      IF (DIR .EQ. 1) THEN
        JTR = 1 + GZSTNB()
      ELSE
        JTR = 1 + GZSTSB()
      END IF
      IF (FIRST) THEN
        CALL EZPICK ('SAMUS_UTIL_PARAM')
        CALL EZGET_i ('NHTMX', NHTMX, IERR)
        CALL EZGET ('RDRTB', RDRTB, IERR)
        IF (KEYR .EQ. 0) THEN
          CALL EZGET ('XBEAM', XBEAM, IERR)
          CALL EZGET ('YBEAM', YBEAM, IERR)
          CALL EZGET ('ZBEAM', ZBEAM, IERR)
        END IF
        CALL EZRSET
        FIRST = .FALSE.
      END IF
      LINK = HITS + NHTMX * NPL
      NHIT = LINK + NHTMX * NPL
      WORK = NHIT + NPL + 1
      IF (NTRK .GE. NTRMX) GO TO 999
      IF (NPTMIN .LT. 2) GO TO 999
      IF (KEYR .EQ. 0) THEN
        VERTEX(1) = XBEAM
        VERTEX(2) = YBEAM
        VERTEX(3) = ZBEAM
        ROAD = 2.0 * RDRTB
      ELSE
        CALL VERXYZ (IVER, VERTEX, NV)
        ROAD = RDRTB
      END IF
      ROAD2 = ROAD * ROAD
 1000 CONTINUE
C
C ****  Loop on base planes
C
      DO 101 PL1 = 1, 8
      IF (IQ(NHIT+PL1) .EQ. 0) GO TO 101
      N1 = IQ(NHIT+PL1)
      J1 = (PL1 - 1) * NHTMX
      HADR(1,1) = PL1
      DO 102 PL2 = 5, 12
      IF (PL1 .GE. 5 .AND. PL2 .LE. 8) GO TO 102
      IF (IQ(NHIT+PL2) .EQ. 0) GO TO 102
      J2 = (PL2 - 1) * NHTMX
      N2 = IQ(NHIT+PL2)
      HADR(1,2) = PL2
C
C ****  Loop on points in base planes
C
      DO 201 L1 = 1, N1
      IF (IQ(LINK+J1+L1) .NE. 0) GO TO 201
      LD1 = IQ(HITS+J1+L1)
      JC1 = IQ(LD1+3)
      HADR(2,1) = L1
      HADR(3,1) = LD1
      DO 202 L2 = 1, N2
      IF (IQ(LINK+J2+L2) .NE. 0) GO TO 202
      LD2 = IQ(HITS+J2+L2)
      JC2 = IQ(LD2+3)
      HADR(2,2) = L2
      HADR(3,2) = LD2
C
C ****  Find parameters of the track passing trough 2 base hits
C
      CALL SACRPL (VERTEX, C(JC1+1), C(JC2+1), LINE, OK)
      IF (OK .LE. 0) GO TO 202
      CALL SADS2L (LINE, C(JC1+1), DIST2, W1, W2, OK)
      IF (OK .LE. 0 .OR. ABS(W2) .GT. C(JC1+7)) GO TO 202
      CALL SADS2L (LINE, C(JC2+1), DIST2, W1, W2, OK)
      IF (OK .LE. 0 .OR. ABS(W2) .GT. C(JC2+7)) GO TO 202
C
C ****  Find all hits on base track
C
      MON = 2
      DO 301 PL = 1, NPL
        IF (PL .EQ. PL1 .OR. PL .EQ. PL2) GO TO 301
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
      IF (MON .LT. NPTMIN) THEN
        IF (MON .GT. IQ(WORK)) THEN
          IQ(WORK+1) = PL1
          IQ(WORK+2) = L1
          IQ(WORK+3) = PL2
          IQ(WORK+4) = L2
          IQ(WORK) = MON
        END IF
        GO TO 202
      END IF
C
C ****  Fit all hits
C
  302 CONTINUE
      ZMIN = VERTEX(3)
      ZMAX = ZMIN
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
      CALL SAMNSQ (SAFCN2, MONITOR+1, N_PAR, FUN, PAR, STEP_PAR)
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
      IF (MON .LT. NPTMIN) GO TO 202
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
      GO TO 1000
C
C ****  End of loops
C
  202 CONTINUE
  201 CONTINUE
  102 CONTINUE
  101 CONTINUE
C
  999 CONTINUE
      IQ(JTR) = NTRK
      RETURN
      END
