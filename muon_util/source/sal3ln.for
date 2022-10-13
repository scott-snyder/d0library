C+
      SUBROUTINE SAL3LN (DIR, NTRACKS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : S/R for linking SAMUS tracks before
C-                         and after the magnet (without times).
C-                         Link only first NTRMXC (=1 on LEVEL2) track in
C-                         current road. If reco, store only newfound tracks,
C-                          on LEVEL2 store all tracks
C-
C
C-   Inputs  : DIR - SAMUS direction number.
C-   Outputs : NTRACKS - number of linked tracks.
C-   Controls: none.
C-
C-   Created   1-OCT-1993   Alexander Efimov
C-   Updated  14-DEC-1993   Alexander Efimov
C-   Updated   2-FEB-1994   Alexei Volkov
C-   Updated   8-FEB-1994   Alexander Efimov
C-   Updated  17-JUN-1994   Andrei Mayorov  change the sign of cos() in
C                           North station
C-   Updated  19-JUN-1994   Andrei Mayorov   fill hit addresses for LEVEL2
C-   Updated   1-NOV-1994   Andrei Mayorov   not reset SATN/S bank every entry
C-                                           to save found traks for LEVEL2
C-   Updated  27-JAN-1995   Andrei Mayorov   NTRACKS - number of found tracks
C-                                           during current call
C-
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER DIR, NTRACKS
      INTEGER LENTRK
      PARAMETER (LENTRK=150)
      INTEGER GZSATN, GZSATS, GZSTNB, GZSTNA, GZSTSB, GZSTSA
      INTEGER LTRB, LTRA, LTRK, JTRK
      INTEGER NTRA, NTRB, IA, IB, LA, LB, MA, MB
      REAL    S, SM, D, DM, XA, YA, XB, YB
      REAL    TET, PHI, XI2, DIST, DMIN, PXA, PYA, PL
      INTEGER NTRMXC,NTRMX,NC
      REAL    DISCM, PLANA, PPAR1, PPAR2
      SAVE    DISCM, PLANA, PPAR1, PPAR2, FIRST, NTRMXC,NTRMX
      INTEGER IERR, J, L, N
      CHARACTER*4 HSHAPE
      INTEGER NSPAR, IBUF, NBUF(7)
      REAL    SPAR(6), XPAR(3), ROTM(3,3)
      REAL    ZMAG
      LOGICAL FIRST,FILTER
      CHARACTER*4 PATH, FILT_PATH
      DATA FILT_PATH/'FILT'/
      DATA    FIRST /.TRUE./
      INTEGER K,NST,NSC,LD,LSSEC,LSSTG,GZSSEC
      EXTERNAL GZSSEC
C
C ****  initializing
C
      IF (FIRST) THEN
        CALL EZPICK ('SAMUS_UTIL_PARAM')
        CALL EZGET  ('DISCM', DISCM, IERR)
        CALL EZGET  ('PLANA', PLANA, IERR)
        CALL EZGET  ('PPAR1', PPAR1, IERR)
        CALL EZGET  ('PPAR2', PPAR2, IERR)
        CALL EZGET_i  ('NTRMX', NTRMX, IERR)

        IF (NTRMXC .LT. 1) NTRMXC = 1
        CALL EZRSET
        CALL PATHGT(PATH)
        FILTER = PATH.EQ.FILT_PATH
        IF(FILTER) THEN
          CALL EZGET_i  ('NTRMXC', NTRMXC, IERR)
        ELSE
          NTRMXC=NTRMX
        END IF
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
      IF(.NOT.FILTER) THEN
        IQ(LTRK) = 0           ! reset number of tracks in direction
      END IF
      NC=0                   ! reset number of tracks in conus
      CALL GTSMAG (DIR, HSHAPE, NSPAR, SPAR, XPAR, ROTM,
     &             NBUF, IBUF)
      ZMAG = XPAR(3)
C
C ****  tracks reconstruction before magnet
C
      NTRB = IQ(LTRB)
      IF (NTRB .EQ. 0) GO TO 999
      NTRA = IQ(LTRA)
      IF (NTRA .EQ. 0) GO TO 999
C
      DO 100 IA = 1, NTRA
        LA = LTRA + (IA - 1) * 64
        IF (IQ(LA+1) .LE. 0) GO TO 100
        DIST = (ZMAG - Q(LA+4)) / Q(LA+7)
        XA = Q(LA+2) + DIST * Q(LA+5)
        YA = Q(LA+3) + DIST * Q(LA+6)
        DMIN = 1.0E+37
        MB = -1
        MA = -1
        DO 200 IB = 1, NTRB
          LB = LTRB + (IB - 1) * 64
          IF (IQ(LB+1) .LE. 0) GO TO 200
          DIST = (ZMAG - Q(LB+4)) / Q(LB+7)
          XB = Q(LB+2) + DIST * Q(LB+5)
          YB = Q(LB+3) + DIST * Q(LB+6)
          D = (XA - XB)**2 + (YA - YB)**2
          D = SQRT (D)
          IF (D .LT. DMIN) THEN
            S = Q(LB+6) * Q(LA+5) - Q(LB+5) * Q(LA+6)
            S = S / SQRT (Q(LB+5)**2 + Q(LB+6)**2)
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
        XI2 = Q(MB+8) + Q(MA+8)
        N = IQ(LTRK)
        IF (NC .GE. NTRMXC.OR.N.GE.NTRMX)   GO TO 999
        NC=NC+1
        IQ(LTRK) = N + 1                            ! number of tracks
        JTRK = LTRK + N * LENTRK
        TET = ACOS (Q(MB+7))
        PXA = Q(MB+5)
        IF (ABS(PXA) .LT. 1.0E-6) PXA = 1.0E-6
        PYA = Q(MB+6)
        IF (DIR .EQ. 1) THEN
          PXA = -PXA
          PYA = -PYA
        END IF
        S = ATAN (ABS(PYA / PXA))
        IF (PYA .LT. 0. .AND. PXA .GT. 0.) THEN
          PHI = TWOPI - S
        ELSE IF (PYA .LT. 0. .AND. PXA .LT. 0.) THEN
          PHI = PI + S
        ELSE IF (PYA .GT. 0. .AND. PXA .LT. 0.) THEN
          PHI = PI - S
        ELSE
          PHI = S
        END IF
        S = 0.0
        DO J = 1, 3
          S = S + Q(MB+J+4) * Q(MA+J+4)
        END DO
        IF (ABS(S) .GE. 1.0) THEN
          S = 1.0E-3
        ELSE
          S = ACOS (S)
        END IF
        IF (ABS(S) .LT. 1.0E-3) S = 1.0E-3
        PL = PPAR1 + PPAR2 / SIN(S / 2.0)     ! Pl
        IQ(JTRK+1) = 1                        ! track flag
        Q(JTRK+2) = PL / ABS(COS(TET))        ! MOMENT
        Q(JTRK+3) = TET                       ! TET angle from vertex
        Q(JTRK+4) = PHI                       ! PHI angle from vertex
        DO J = 1, 6                           ! 6 track parameters
          Q(JTRK+J+4) = Q(MB+J+1)             ! before toroid
        END DO
        DO J = 1, 6                           ! 6 track parameters
          Q(JTRK+J+10) = Q(MA+J+1)            ! after toroid
        END DO
        IF(DIR.EQ.1) THEN
          DO J=1,3
            Q(JTRK+7+J)=-Q(JTRK+7+J)
            Q(JTRK+13+J)=-Q(JTRK+13+J)
          END DO
        END IF
        IQ(JTRK+17) = IQ(MB+1)                ! number of hits before
        IQ(JTRK+18) = IQ(MA+1)                ! number of hits after
        Q(JTRK+19) = Q(MB+8)                  ! XI2 before
        Q(JTRK+20) = Q(MA+8)                  ! XI2 after
        Q(JTRK+21) = DM             ! distatnce between tracks in toroid
        Q(JTRK+22) = SM             ! planarity
C--
        Q(JTRK+23) = 0.0         ! distance to vertex
        Q(JTRK+24) = 0.0         ! energy deposit of muon in calorimeter
        Q(JTRK+25) = 0.0         ! energy deposit of muon in toroid
        IF (FILTER) THEN
          IQ(JTRK+26) = IQ(MB+1)      ! number of tubes on track before
          IQ(JTRK+27) = IQ(MA+1)      ! number of tubes on track after
          IQ(JTRK+28) = 0
          IQ(JTRK+29) = 0
          L = JTRK + 30
          IQ(L) = IQ(MB+1) + IQ(MA+1)   ! total number of hits
          N = IQ(MB+1)
          DO J = 1, N                      ! hits before toroid
            LD = IQ(MB+J+8)
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
          N = IQ(MA+1)
          DO J = 1, N                      ! hits after toroid
            LD = IQ(MA+J+8)
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
        END IF
C--
        IQ(MA+1) = -1
        IQ(MB+1) = -1
        N = IQ(LTRK)
        IF (N .EQ. NTRMXC) GO TO 999
  100 CONTINUE
  999 CONTINUE
      NTRACKS = NC
C
      RETURN
      END
