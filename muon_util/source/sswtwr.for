C+
      SUBROUTINE SSWTWR (DIR, NSAMIN, NTRMX, NTRK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : find Tracks after the magnet  using only
C-                         the WiRes in SA and WA (delta t)
C-
C-   Inputs  : DIR - direction number: 1-> N, 2-> S
C-             NSAMIN - minimum number of SAMUS hits on track
C-             NTRMX - maximum number of tracks
C-   Outputs : NTRK - number of reconstructed tracks.
C-   Controls: none.
C-
C-   Based on Efimov's SATR2L
C-
C-   Created  30-MAY-1994   Joao R.T. de Mello Neto
C-            24-FEB-1995   Andre Sznajder ( bug fixing and clean up ) 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER DIR, NSAMIN, NTRK, NTRMX
      INTEGER NPL
      PARAMETER (NPL=12)
      INTEGER LMTRH, LSAMT, LSAHS
      INTEGER GZMTRH, GZSAMT, GZSAHS, GZSATW, GZSTNA, GZSTSA
      INTEGER LMUOH, GZMUOH
      INTEGER HITS, NHIT, LINK, WORK
      INTEGER LP, LPSAVE, IROAD, IHIT
      REAL ADISTPL(12), DISTPL
C>>>>>
      REAL WROAD2
      INTEGER NHTMX, NMAXROADS, NMAXHIT, NWAMIN, NMWPL
      INTEGER LPWAREA, LTRG2, WHIT, NPLANE
      INTEGER LPWTRK, LPTRK
      REAL POINT(3), PAV(3)
      REAL BIGNUM 
      PARAMETER (BIGNUM = 1.0E10)
      INTEGER IHTAD, WMON
      COMMON /CSSWFCN/ ZMIN, ZMAX, ROAD, ROAD2,
     &        MONITOR, HADR(3,NPL), IHTAD(12), WMON, WROAD2
      REAL    ZMIN, ZMAX, ROAD, ROAD2
      INTEGER MONITOR, HADR
      REAL    LINE(6), DIST2, ZZ, W, W1, W2
      INTEGER N_PAR
      PARAMETER (N_PAR=4)
      REAL    PAR(N_PAR), STEP_PAR(N_PAR), FUN(100)
      INTEGER PL, PL1, PL2
      INTEGER N1, N2, J1, J2, L1, L2
      INTEGER JC1, JC2
      INTEGER LD1, LD2, JTR, LT
      INTEGER K, NN, LL, J, JJ, LD, JC, MON, MON_OLD, OK
      EXTERNAL SSWFCN
      LOGICAL FIRST
      SAVE    FIRST
      DATA    FIRST /.TRUE./
      COMMON /LINEWR/ LINE
C----------------------------------------------------------------------
C
C ****  Initialyzing
C
      IF (FIRST) THEN
        NHTMX = 100      ! max. # hits in a plane
        NMAXROADS = 24   ! max. no. of roads
        NMAXHIT   = 24   ! max. no. of hits in road
        NWAMIN = 2
        NMWPL = 12       ! max. no. of points in tracks
        ROAD = 6.0       ! samus road after magnet
        ROAD2 = ROAD * ROAD
        WROAD2 =30.0*30.0 ! wamus road   
        FIRST = .FALSE.
      ENDIF
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
      LINK = HITS + NHTMX * NPL   ! Samus part of work bank
      NHIT = LINK + NHTMX * NPL
      WORK = NHIT + NPL + 1
      LMUOH=GZMUOH(0)             ! setup MUOH pointer
      LPWAREA = NHIT + NPL + 33   ! Wamus part of work bank
      LPWTRK = LPWAREA + 1 + 4*NMAXROADS + 7*NMAXROADS*NMAXHIT
      LPTRK = LPWTRK + 1
      IF (NTRK .GE. NTRMX) GO TO 999
      IF (NSAMIN .LT. 2) GO TO 999
      LP = LPWAREA + 1
C
C *** Loop over roads to get WAMUS base point 
C
      LTRG2 = IQ(LP)              ! get # of roads found by L2 SATGSW
      DO 51 IROAD = 1, LTRG2      ! loop on roads for tracking
        WHIT = IQ(LP+1)           ! get how many hits in this road
        IF (WHIT.EQ.0) THEN
          LP=LP+1
          GOTO 51
        ENDIF
        PAV(1) = Q(LP+2)          ! get WAMUS C average point to act as
        PAV(2) = Q(LP+3)          ! a base point
        PAV(3) = Q(LP+4)
        LPSAVE = LP               ! save pointer to hits in htis road
 1000   CONTINUE
C
C ****  Loop SAMUS on base planes
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
C       (wires) in SAMUS and the base point in WAMUS
C
        CALL SACRPL (PAV, C(JC1+1), C(JC2+1), LINE, OK)
        IF (OK .LE. 0) GO TO 202
        CALL SADS2L (LINE, C(JC1+1), DIST2, W1, W2, OK)
        IF (OK .LE. 0 .OR. ABS(W2) .GT. C(JC1+7)) GO TO 202
        CALL SADS2L (LINE, C(JC2+1), DIST2, W1, W2, OK)
        IF (OK .LE. 0 .OR. ABS(W2) .GT. C(JC2+7)) GO TO 202
C
C ****  Find all hits on base track
C
        WMON = 0
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
     &            DIST2 .LT. ROAD2) THEN
                MON = MON + 1
                HADR(1,MON) = PL
                HADR(2,MON) = LL
                HADR(3,MON) = LD
                GO TO 301
              END IF
            END IF
          END DO
  301   CONTINUE
        DO K=1, NMWPL  ! initialize  auxiliary arrays
          ADISTPL(K)=BIGNUM
          IHTAD(K)=0
        END DO
        LP = LPSAVE + 4
        DO IHIT = 1, WHIT  ! find Wamus hits in this road that are on this track
          NPLANE = IQ(LP+1)     ! get # of a hit plane (0->11) 
          POINT(1) = Q(LP+4)    ! get hit coordinates
          POINT(2) = Q(LP+5)
          POINT(3) = Q(LP+6)
          CALL SADSPL(POINT,LINE,DISTPL,W) ! get dist. between hit and track
          IF (DISTPL.LT.WROAD2.AND.DISTPL.LT.ADISTPL(NPLANE+1)) THEN
             ADISTPL(NPLANE+1) = DISTPL    ! save the closets hit to this track
             IHTAD(NPLANE+1) = IQ(LP + 3)  ! save this hit position in MUOH
             IQ(LP + 7) = 1                ! flag this hit as used
          ENDIF
          LP = LP + 7
        END DO
        DO IHIT =1, NMWPL ! update # planes with hits in this road (1->12)
          IF (IHTAD(IHIT).GT.0) WMON = WMON + 1
        END DO
        IF (WMON .LT. NWAMIN) GO TO 202    ! selects tracks with at least 2 hits
        IF (MON .LT. NSAMIN) THEN
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
  302   CONTINUE
        ZMIN = BIGNUM
        ZMAX = -BIGNUM
        DO J = 1, MON
          LD = HADR(3,J)
          JC = IQ(LD+3)
          ZZ = C(JC+3)
          IF (ZZ .LT. ZMIN) ZMIN = ZZ
          IF (ZZ .GT. ZMAX) ZMAX = ZZ
        END DO
        DO J =1, NMWPL           
          IF (IHTAD(J).GT.0) THEN
           ZZ = Q(LMUOH+23+28*(IHTAD(J)-1))
            IF (ZZ .LT. ZMIN) ZMIN = ZZ
            IF (ZZ .GT. ZMAX) ZMAX = ZZ
          ENDIF
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
        CALL SAMNSQ (SSWFCN,WMON+MONITOR,N_PAR,FUN,PAR,STEP_PAR)
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
C ****  Search new points (of Samus) on track after fit procedure 
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
     &            DIST2 .LT. ROAD2) THEN
                MON = MON + 1
                HADR(1,MON) = PL
                HADR(2,MON) = LL
                HADR(3,MON) = LD
                GO TO 303
              END IF
            END IF
          END DO
  303   CONTINUE
        IF (MON .NE. MON_OLD) GO TO 302
C
C ****  Put information to SAMUS output banks
C
        IF (MON .LT. NSAMIN) GO TO 202
        NTRK = NTRK + 1
        LT = JTR + (NTRK - 1) * 64    ! pointer to STNA, STSA
        IQ(LT+1) = MON                ! number of SAMUS tubes on track
        DO J = 1, 6
          Q(LT+J+1) = LINE(J)
        END DO
        Q(LT+8) = 0.0                 ! defined as zero in STNB/STSB zeb...
        NN = LT + 8
        DO J = 1, MON                 
          NN = NN + 1
          IQ(NN) = HADR(3,J)             !   tube address
          PL = HADR(1,J)
          LL = HADR(2,J)                 !   which fired tube in plane pl
          JJ = (PL - 1) * NHTMX + LL
          IQ(LINK+JJ) = NTRK             !   link fired tube to a track cand.
        END DO
C
C *** Fills WAMUS C temporary bank on hits on tracks
C
        IQ(LPTRK) = WMON
        DO J = 1, NMWPL
          IQ(LPTRK+J) = IHTAD(J)
        END DO
        LPTRK = LPTRK + NMWPL + 1
        IF (NTRK .GE. NTRMX) GO TO 999
        GO TO 1000
C
C ****  End of loops
C
  202   CONTINUE
  201   CONTINUE
  102   CONTINUE
  101   CONTINUE
   51 CONTINUE
C
  999 CONTINUE
      IQ(JTR) = NTRK                ! number of track cand in STNA/STSA
      RETURN
      END
