C+
      SUBROUTINE SATC4L (DIR, NPTMIN, NPTOT, NTRK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find tracks after the magnet ising 
C-                         time information.
C-
C-   Inputs  : DIR - direction number,
C-             NPTMIN - minimum number of hits on track,
C-             NPTOT - total number of tubes on track,
C-             NTRK - track number.
C-   Outputs : none.
C-   Controls: none.
C-
C-   Created   2-JUN-1992   Alexander Efimov
C-   Updated  11-FEB-1994   Alexander Efimov   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER DIR, NPTMIN, NPTOT, NTRK
      INTEGER NPL
      PARAMETER (NPL=24)
      INTEGER LMTRH, LSAMT, LSAHS
      INTEGER GZMTRH, GZSAMT, GZSAHS, GZSATW, GZSTNA, GZSTSA
      INTEGER NTRKMX, HITS, NHIT, LINK
      COMMON /COMSAFCN4/ ZMIN, ZMAX, ROAD, ROAD2, 
     &                   MONITOR, HADR(3,NPL)
      REAL    ZMIN, ZMAX, ROAD, ROAD2
      INTEGER MONITOR, HADR
      INTEGER N_PAR
      PARAMETER (N_PAR=4)
      REAL    PAR(N_PAR), STEP_PAR(N_PAR), FUN(100)
      INTEGER PL1, PL2, PL3, PL4, LD1, LD2, LD3, LD4
      INTEGER JC1, JC2, JC3, JC4, N1, N2, N3, N4
      REAL    DS1, DS2, DS3, DS4, VX1, VX2, VX3, VX4
      REAL    VY1, VY2, VY3, VY4, CX, CY
      REAL    LINE1(6), LINE2(6), LINE3(6), LINE4(6)
      INTEGER LD, JC, JTR, LT, IERR, PL, I, J, M
      REAL    LINE(6), DIST, ZZ, W, W1, W2, XI2, XI2_BEST
      INTEGER MON, MON_OLD, OK, NEXT
      INTEGER MON_BEST, HADR_BEST(3,NPL)
      REAL    LINE_BEST(6)
      REAL    RDRTMA
      SAVE    RDRTMA
      EXTERNAL SAFC4D
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
        JTR = 1 + GZSTNA()
      ELSE
        JTR = 1 + GZSTSA()
      END IF
      NTRKMX = IQ(LSAMT+1)
      LT = JTR + (NTRK - 1) * 64
      IQ(LT+33) = 0
      LINK = HITS + NPL
      NHIT = LINK + NPL
      IF (FIRST) THEN
        CALL EZPICK ('SAMUS_UTIL_PARAM')
        CALL EZGET ('RDRTMA', RDRTMA, IERR)
        CALL EZRSET
        FIRST = .FALSE.
      END IF
      IF (NPTMIN .LT. 4) GO TO 999
      ROAD = RDRTMA
      ROAD2 = ROAD**2
      MON_BEST = 0
      XI2_BEST = 1.0E+13
C
C ****  Loop on base planes
C
      DO 101 PL1 = 1, 8
      IF (IQ(NHIT+PL1) .EQ. 0) GO TO 101
      LD1 = IQ(HITS+PL1)
      JC1 = IQ(LD1+3)
      HADR(1,1) = PL1
      HADR(2,1) = JC1
      HADR(3,1) = LD1
      DS1 = Q(LD1+4)
      CX = C(JC1+4)
      CY = C(JC1+5)
      W = DS1 / SQRT (CX * CX + CY * CY)
      VX1 = - CY * W
      VY1 = + CX * W
C
      DO 102 PL2 = 5, 12
      IF (PL1 .GE. 5 .AND. PL2 .LE. 8) GO TO 102
      IF (IQ(NHIT+PL2) .EQ. 0) GO TO 102
      LD2 = IQ(HITS+PL2)
      JC2 = IQ(LD2+3)
      HADR(1,2) = PL2
      HADR(2,2) = JC2
      HADR(3,2) = LD2
      DS2 = Q(LD2+4)
      CX = C(JC2+4)
      CY = C(JC2+5)
      W = DS2 / SQRT (CX * CX + CY * CY)
      VX2 = - CY * W
      VY2 = + CX * W
C
      DO 103 PL3 = 13, 20
      IF (IQ(NHIT+PL3) .EQ. 0) GO TO 103
      LD3 = IQ(HITS+PL3)
      JC3 = IQ(LD3+3)
      HADR(1,3) = PL3
      HADR(2,3) = JC3
      HADR(3,3) = LD3
      DS3 = Q(LD3+4)
      CX = C(JC3+4)
      CY = C(JC3+5)
      W = DS3 / SQRT (CX * CX + CY * CY)
      VX3 = - CY * W
      VY3 = + CX * W
C
      DO 104 PL4 = 17, 24
      IF (PL3 .GE. 17 .AND. PL4 .LE. 20) GO TO 104
      IF (IQ(NHIT+PL4) .EQ. 0) GO TO 104
      LD4 = IQ(HITS+PL4)
      JC4 = IQ(LD4+3)
      HADR(1,4) = PL4
      HADR(2,4) = JC4
      HADR(3,4) = LD4
      DS4 = Q(LD4+4)
      CX = C(JC4+4)
      CY = C(JC4+5)
      W = DS4 / SQRT (CX * CX + CY * CY)
      VX4 = - CY * W
      VY4 = + CX * W
C
C ****  Loop on points in base planes
C
      DO 201 N1 = 1, 2
      DO I = 1, 6
        LINE1(I) = C(JC1+I)    ! tube axis parameters
      END DO
      IF (N1 .EQ. 1) THEN
        LINE1(1) = LINE1(1) + VX1
        LINE1(2) = LINE1(2) + VY1
      ELSE
        LINE1(1) = LINE1(1) - VX1
        LINE1(2) = LINE1(2) - VY1
      END IF
C
      DO 202 N2 = 1, 2
      DO I = 1, 6
        LINE2(I) = C(JC2+I)    ! tube axis parameters
      END DO
      IF (N2 .EQ. 1) THEN
        LINE2(1) = LINE2(1) + VX2
        LINE2(2) = LINE2(2) + VY2
      ELSE
        LINE2(1) = LINE2(1) - VX2
        LINE2(2) = LINE2(2) - VY2
      END IF
C
      DO 203 N3 = 1, 2
      DO I = 1, 6
        LINE3(I) = C(JC3+I)    ! tube axis parameters
      END DO
      IF (N3 .EQ. 1) THEN
        LINE3(1) = LINE3(1) + VX3
        LINE3(2) = LINE3(2) + VY3
      ELSE
        LINE3(1) = LINE3(1) - VX3
        LINE3(2) = LINE3(2) - VY3
      END IF
C
      DO 204 N4 = 1, 2
      DO I = 1, 6
        LINE4(I) = C(JC4+I)    ! tube axis parameters
      END DO
      IF (N4 .EQ. 1) THEN
        LINE4(1) = LINE4(1) + VX4
        LINE4(2) = LINE4(2) + VY4
      ELSE
        LINE4(1) = LINE4(1) - VX4
        LINE4(2) = LINE4(2) - VY4
      END IF
C
C ****  Find parameters of the track passing trough 4 base hits
C
      CALL SACR4L (LINE1, LINE2, LINE3, LINE4, LINE, OK)
      IF (OK .LE. 0) GO TO 204
      CALL SADS2L (LINE, LINE1, DIST, W1, W2, OK)
      IF (OK .LE. 0 .OR. ABS(W2) .GT. C(JC1+7)) GO TO 204
      CALL SADS2L (LINE, LINE2, DIST, W1, W2, OK)
      IF (OK .LE. 0 .OR. ABS(W2) .GT. C(JC2+7)) GO TO 204
      CALL SADS2L (LINE, LINE3, DIST, W1, W2, OK)
      IF (OK .LE. 0 .OR. ABS(W2) .GT. C(JC3+7)) GO TO 204
      CALL SADS2L (LINE, LINE4, DIST, W1, W2, OK)
      IF (OK .LE. 0 .OR. ABS(W2) .GT. C(JC4+7)) GO TO 204
C
C ****  Find all hits on base track
C
      MON = 4
      XI2 = 0.0
      DO 301 PL = 1, NPL
        IF (MON+NPL-PL+1 .LT. NPTMIN) GO TO 204
        IF (PL .EQ. PL1 .OR. PL .EQ. PL2) GO TO 301
        IF (PL .EQ. PL3 .OR. PL .EQ. PL4) GO TO 301
        IF (IQ(NHIT+PL) .EQ. 0) GO TO 301
        LD = IQ(HITS+PL)
        JC = IQ(LD+3)
        CALL SADS2L (LINE, C(JC+1), DIST, W1, W2, OK)
        IF (OK .GT. 0 .AND. ABS(W2) .LE. C(JC+7)) THEN
          W = ABS (SQRT(DIST) - Q(LD+4))
          IF (W .LT. ROAD) THEN
            MON = MON + 1
            HADR(1,MON) = PL
            HADR(2,MON) = JC
            HADR(3,MON) = LD
            XI2 = XI2 + W * W
          END IF
        END IF
  301 CONTINUE
      IF (MON .LT. NPTMIN) GO TO 204
      IF (MON .GT. 4) XI2 = XI2 / REAL(MON-4)
C
C ****  search for the best track
C
      IF (MON .LT. MON_BEST) GO TO 204
      IF (MON .EQ. MON_BEST .AND. XI2 .GT. XI2_BEST) GO TO 204
      MON_BEST = MON
      XI2_BEST = XI2
      DO I = 1, 6
        LINE_BEST(I) = LINE(I)
      END DO
      DO J = 1, MON
        DO I = 1, 3
          HADR_BEST(I,J) = HADR(I,J)
        END DO
      END DO
      IF (MON_BEST .EQ. NPTOT) GO TO 400
C
C ****  End of loops
C
  204 CONTINUE
  203 CONTINUE
  202 CONTINUE
  201 CONTINUE
  104 CONTINUE
  103 CONTINUE
  102 CONTINUE
  101 CONTINUE
C
C ****  cylinder fit
C
  400 CONTINUE
      IF (MON_BEST .LT. NPTMIN) GO TO 999
      MON = MON_BEST
      DO I = 1, 6
        LINE(I) = LINE_BEST(I)
      END DO
      DO J = 1, MON
        DO I = 1, 3
          HADR(I,J) = HADR_BEST(I,J)
        END DO
      END DO
      NEXT = 0
  402 CONTINUE
      ZMIN = +1.0E+37
      ZMAX = -1.0E+37
      DO J = 1, MON
        JC = HADR(2,J)
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
      IF (NEXT .EQ. 0) THEN
        DO J = 1, N_PAR
          STEP_PAR(J) = ROAD
        END DO
        CALL SAMNSQ (SAFC4D, MONITOR, N_PAR, FUN, PAR, STEP_PAR)
      END IF
      DO J = 1, N_PAR
        STEP_PAR(J) = 0.1 * ROAD
      END DO
      CALL SAMNSQ (SAFC4D, MONITOR, N_PAR, FUN, PAR, STEP_PAR)
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
C ****  Search for new points on track after fit procedure
C
      MON_OLD = MON
      MON = 0
      M = 0
      DO 401 PL = 1, NPL
        IF (IQ(NHIT+PL) .EQ. 0) GO TO 401
        LD = IQ(HITS+PL)
        JC = IQ(LD+3)
        CALL SADS2L (LINE, C(JC+1), DIST, W1, W2, OK)
        IF (OK .GT. 0 .AND. ABS(W2) .LE. C(JC+7)) THEN
          W = ABS (SQRT(DIST) - Q(LD+4))
          IF (W .LT. ROAD) THEN
            MON = MON + 1
            IF (HADR(1,MON) .NE. PL) M = 1
            HADR(1,MON) = PL
            HADR(2,MON) = JC
            HADR(3,MON) = LD
          END IF
        END IF
  401 CONTINUE
      IF (MON .LT. NPTMIN) GO TO 999
      IF (MON .NE. MON_OLD .OR. M .EQ. 1) THEN
        NEXT = NEXT + 1
        IF (NEXT .LT. 25) GO TO 402
      END IF
      MONITOR = MON
C
C ****  Put information to the output banks
C
      CALL SAFC4D (MONITOR, N_PAR, FUN, PAR, 1)
      XI2 = 0.0
      DO J = 1, MONITOR
        XI2 = XI2 + FUN(J)**2
      END DO
      XI2 = XI2 / REAL(MONITOR-N_PAR)
      IQ(LT+33) = MONITOR
      DO J = 1, 6
        Q(LT+J+33) = LINE(J)
      END DO
      Q(LT+40) = XI2
      I = LT + 40
      DO J = 1, MONITOR
        I = I + 1
        IQ(I) = HADR(3,J)
      END DO
C
  999 CONTINUE
      RETURN
      END
