C+
      SUBROUTINE SATRLN (DIR, NTRACKS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : S/R for linking SAMUS tracks before
C-                         and after the magnet.
C-
C-   Inputs  : DIR - SAMUS direction number.
C-   Outputs : NTRACKS - number of linked tracks.
C-   Controls: none.
C-
C-   Created   6-SEP-1993   Alexander Efimov   
C-   Updated   9-FEB-1994   Alexander Efimov   
C-   Updated  23-MAR-1994   Alexander Efimov - correct direction of the
C-                          muon tracks.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER DIR, NTRACKS
      INTEGER N_LAYS, N_PLANES
      PARAMETER (N_LAYS=4, N_PLANES=3)
      INTEGER LENTRK
      PARAMETER (LENTRK=150)
      INTEGER GZSATN, GZSTNB, GZSTNA, GZSATS, GZSTSB, GZSTSA
      INTEGER GZSSEC, GZSAHS
      INTEGER LTRB, LTRA, LTRK, JTRK, LSSEC, LSSTG, LSAHS
      INTEGER NTRA, NTRB, IA, IB, LA, LB, MA, MB
      INTEGER IERR, I, J, L, N, K, LD, NST, NSC, IPL, IND
      INTEGER KEY_L2(2), IST, TUBE, PL0, PL, NL1, LAY
      REAL    S, SM, D, DM, XA, YA, XB, YB, PX, PY
      REAL    TET, PHI, XI2, DIST, DMIN
      CHARACTER*4 HSHAPE
      INTEGER NSPAR, IBUF, NBUF(7)
      REAL    SPAR(6), XPAR(3), ROTM(3,3)
      REAL    ZMAG
      INTEGER NTRMX
      REAL    DISCM, PLANA, CHISQ
      SAVE    DISCM, PLANA, CHISQ, NTRMX
      LOGICAL FIRST
      SAVE    FIRST
      DATA    FIRST /.TRUE./
C
C ****  initializing
C
      IF (FIRST) THEN
        CALL EZPICK ('SAMUS_UTIL_PARAM')
        CALL EZGET  ('DISCM', DISCM, IERR)
        CALL EZGET  ('PLANA', PLANA, IERR)
        CALL EZGET  ('CHISQ', CHISQ, IERR)
        CALL EZGET  ('NTRMX', NTRMX, IERR)
        IF (NTRMX .LT. 1) NTRMX = 1
        CALL EZRSET
        FIRST = .FALSE.
      END IF
      IF (DIR .EQ. 1) THEN
        LTRK = 1 + GZSATN()
        LTRB = 1 + GZSTNB()
        LTRA = 1 + GZSTNA()
      ELSE
        LTRK = 1 + GZSATS()
        LTRB = 1 + GZSTSB()
        LTRA = 1 + GZSTSA()
      END IF
      IQ(LTRK) = 0           ! reset number of tracks in direction
      NTRACKS = 0
      CALL GTSMAG (DIR, HSHAPE, NSPAR, SPAR, XPAR, ROTM,
     &             NBUF, IBUF)
      ZMAG = XPAR(3)
C
C ****  trigger keys
C
      KEY_L2(1) = 0
      KEY_L2(2) = 0
      LSAHS = GZSAHS ()
      DO IST = 1, 3
        NST = (DIR - 1) * 3 + IST
        DO IPL = 1, N_PLANES
          PL0 = (IST - 1) * N_PLANES * N_LAYS +
     &          (IPL - 1) * N_LAYS
          IND = 3 * (NST - 1) + IPL
          N = IQ(LSAHS+IND)           ! number of hits in plane
          LD = LQ(LSAHS-IND)          ! hits address
          DO I = 1, N                                 ! hits loop
            NSC  = IBITS (IQ(LD+2),  5,  5)     ! section number
            TUBE = IBITS (IQ(LD+2), 16, 16)     ! tube number
            LSSEC = GZSSEC (NST, NSC)        ! 'SSEC' bank address
            NL1 = IC(LSSEC+10)            ! first tube layer number
            LAY = MOD (TUBE-1, N_LAYS)         ! layer number
            IF (IC(LSSEC+9) .GT. 0) THEN
              LAY = NL1 + LAY
              IF (LAY .GT. N_LAYS) LAY = LAY - N_LAYS
            ELSE
              LAY = NL1 - LAY
              IF (LAY .LT.      1) LAY = LAY + N_LAYS
            END IF
            PL = PL0 + LAY
            IF (IQ(LD+1) .EQ. 2) THEN
              IF (PL .LE. 32) THEN
                K = 1
              ELSE
                K = 2
                PL = PL - 32
              END IF
              CALL SBYT (1, KEY_L2(K), PL, 1)
            END IF
            LD = LD + 13
          END DO
        END DO
      END DO
C
C ****  link tracks before and after toroid
C
      NTRB = IQ(LTRB)
      IF (NTRB .EQ. 0) GO TO 999
      NTRA = IQ(LTRA)
      IF (NTRA .EQ. 0) GO TO 999
      DO 100 IA = 1, NTRA
        LA = LTRA + (IA - 1) * 64
        IF (IQ(LA+1) .LE. 0 .OR. IQ(LA+33) .LE. 0) GO TO 100
        DIST = (ZMAG - Q(LA+36)) / Q(LA+39)
        XA = Q(LA+34) + DIST * Q(LA+37)
        YA = Q(LA+35) + DIST * Q(LA+38)
        DMIN = 1.0E+37
        MB = -1
        MA = -1
        DO 200 IB = 1, NTRB
          LB = LTRB + (IB - 1) * 64
          IF (IQ(LB+1) .LE. 0 .OR. IQ(LB+33) .LE. 0) GO TO 200
          DIST = (ZMAG - Q(LB+36)) / Q(LB+39)
          XB = Q(LB+34) + DIST * Q(LB+37)
          YB = Q(LB+35) + DIST * Q(LB+38)
          D = (XA - XB)**2 + (YA - YB)**2
          D = SQRT (D)
          IF (D .LT. DMIN) THEN
            S = Q(LB+38) * Q(LA+37) - Q(LB+37) * Q(LA+38)
            S = S / SQRT (Q(LB+37)**2 + Q(LB+38)**2)
            IF (ABS(S) .LT. PLANA) THEN
              DMIN = D
              MB = LB
              MA = LA
              DM = D
              SM = S
            END IF
          END IF
  200   CONTINUE
        IF (MB .LT. 0 .OR. MA .LT. 0) GO TO 100
        IF (DMIN .GT. DISCM)          GO TO 100
        XI2 = Q(MB+40) + Q(MA+40)
        IF (XI2 .GT. CHISQ)           GO TO 100
        N = IQ(LTRK)
        IF (N .GE. NTRMX)             GO TO 100
        IQ(LTRK) = N + 1
        JTRK = LTRK + N * LENTRK
        TET = ACOS(ABS(Q(MB+39)))
        IF (DIR .EQ. 1) TET = PI - TET
        PX = Q(MB+37)
        PY = Q(MB+38)
        IF (DIR .EQ. 1) THEN
          PX = -PX
          PY = -PY
        END IF
        IF (ABS(PX) .LT. 1.0E-13) THEN
          S = HALFPI
        ELSE
          S = ATAN (ABS(PY/PX))
        END IF
        IF      (PY .LT. 0.0 .AND. PX .GT. 0.0) THEN
          PHI = TWOPI - S
        ELSE IF (PY .LT. 0.0 .AND. PX .LT. 0.0) THEN
          PHI = PI + S
        ELSE IF (PY .GT. 0.0 .AND. PX .LT. 0.0) THEN
          PHI = PI - S
        ELSE
          PHI = S
        END IF
        IQ(JTRK+1) = 1                    ! track flag
        Q(JTRK+2) = 0.0                   ! moment
        Q(JTRK+3) = TET                   ! TET angle from vertex
        Q(JTRK+4) = PHI                   ! PHI angle from vertex
        DO J = 1, 6                          ! 6 track parameters
          Q(JTRK+J+4) = Q(MB+J+33)           ! before toroid
        END DO
        K = 0
        IF (DIR .EQ. 1 .AND. Q(JTRK+10) .GT. 0.0) K = 1   ! correct
        IF (DIR .EQ. 2 .AND. Q(JTRK+10) .LT. 0.0) K = 1   ! track
        IF (K .EQ. 1) THEN                                ! direction
          DO J = 1, 3
            Q(JTRK+J+7) = -Q(JTRK+J+7)
          END DO
        END IF
        DO J = 1, 6                          ! 6 track parameters
          Q(JTRK+J+10) = Q(MA+J+33)          ! after toroid
        END DO
        K = 0
        IF (DIR .EQ. 1 .AND. Q(JTRK+16) .GT. 0.0) K = 1   ! correct
        IF (DIR .EQ. 2 .AND. Q(JTRK+16) .LT. 0.0) K = 1   ! track
        IF (K .EQ. 1) THEN                                ! direction
          DO J = 1, 3
            Q(JTRK+J+13) = -Q(JTRK+J+13)
          END DO
        END IF
        IQ(JTRK+17) = IQ(MB+33)           ! number of hits before
        IQ(JTRK+18) = IQ(MA+33)           ! number of hits after
        Q(JTRK+19) = Q(MB+40)             ! XI2 before
        Q(JTRK+20) = Q(MA+40)             ! XI2 after
        Q(JTRK+21) = DM          ! distatnce between tracks in toroid
        Q(JTRK+22) = SM          ! planarity
        Q(JTRK+23) = 0.0         ! distance to vertex
        Q(JTRK+24) = 0.0         ! energy deposit of muon in calorimeter
        Q(JTRK+25) = 0.0         ! energy deposit of muon in toroid
        IQ(JTRK+26) = IQ(MB+1)      ! number of tubes on track before
        IQ(JTRK+27) = IQ(MA+1)      ! number of tubes on track after
        IQ(JTRK+28) = KEY_L2(1)
        IQ(JTRK+29) = KEY_L2(2)
        IQ(JTRK+30) = IQ(MB+33) + IQ(MA+33)   ! total number of hits
        L = JTRK + 30
        N = IQ(MB+33)
        DO J = 1, N                      ! hits before toroid
          LD = IQ(MB+J+40)
          K = IQ(LD+2)
          NST  = IBITS (K,  0,  5)         ! station number
          NSC  = IBITS (K,  5,  5)         ! section number
          LSSEC = GZSSEC (NST, NSC)        ! 'SSEC' bank address
          LSSTG = LC(LSSEC-1)              ! address of the bank 'SSTG'
          IQ(L+1) = IQ(LD+2)
          IQ(L+2) = IQ(LD+3) - LSSTG
          IQ(L+3) = Q(LD+4) * 1.0E+5
          L = L + 3
        END DO
        N = IQ(MA+33)
        DO J = 1, N                      ! hits after toroid
          LD = IQ(MA+J+40)
          K = IQ(LD+2)
          NST  = IBITS (K,  0,  5)         ! station number
          NSC  = IBITS (K,  5,  5)         ! section number
          LSSEC = GZSSEC (NST, NSC)        ! 'SSEC' bank address
          LSSTG = LC(LSSEC-1)              ! address of the bank 'SSTG'
          IQ(L+1) = IQ(LD+2)
          IQ(L+2) = IQ(LD+3) - LSSTG
          IQ(L+3) = Q(LD+4) * 1.0E+5
          L = L + 3
        END DO
        IQ(MA+1) = -1
        IQ(MB+1) = -1
  100 CONTINUE
      NTRACKS = IQ(LTRK)
C
  999 CONTINUE
      RETURN
      END
