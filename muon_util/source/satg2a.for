C+
      SUBROUTINE SATG2A (DIR, LTRG2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Trigger #2 for SAMUS :
C-                         search 3-points in A, B and C stations
C-                         with the same PHI polar angle.
C-
C-   Inputs  : DIR    - SAMUS direction (1 - N, 2 - S).
C-   Outputs : LTRG2  - number of the founded triggers.
C-             PT(24) - Pt  of tracks.
C-             ETA(24)- ETA of tracks.
C-   Controls: none.
C-
C-   Created  21-AUG-1992   Alexander Efimov
C-   Updated  05-NOV-1992   Alexei Volkov
C-   Updated  13-NOV-1992   Alexander Efimov: change SAMUS banks
C-                                            structure
C-   Updated  16-NOV-1992   Alexei Volkov
C-   Updated  08-DEC-1992   Alexei Volkov
C-   Updated  27-DEC-1992   Alexei Volkov
C-   Updated   2-SEP-1993   Alexander Efimov: add NTRPL parameter.
C-   Updated   7-SEP-1993   Alexander Efimov: change L2 trigger
C-                                            algorithm.
C-   Updated  17-NOV-1993   O.Eroshin  & V. Podstavkov
C-                          Add theta & phi a la J. Balderston
C-   Updated  18-NOV-1993   V. Podstavkov fix phi calculation
C-   Updated   4-FEB-1994   Alexei Volkov
C-   Updated  11-JAN-1995   Andrei Mayorov  clean up unused call to GZ*
C-                                          delete loop over all 3-hits
C-                                          during hits tagging;
C-                                          delete pt,eta,... calculation
C-                                          all this values in SATN/S bank;
C-                                          NTRTU parameter not used anymore
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
C----------------------------------------------------------------------
      INTEGER DIR, LTRG2, LTR2, LT2
      INTEGER LTRK, NTRK, NTRA, NTRB
      INTEGER NLT, NJX(3)
      INTEGER LSAHS, GZSAHS
      INTEGER GZSATN, GZSATS
      INTEGER N_PLANES
      PARAMETER (N_PLANES=3)
      REAL    WDD(24)
      INTEGER NXX(24,3), NAC(24),II
      INTEGER NST, IST, IND, INDX(3), LDA, LDB, LDC,LDX(3)
      EQUIVALENCE (LDA,LDX(1)),(LDB,LDX(2)),(LDC,LDX(3))
      INTEGER MX(3), NA, NB, NC, NWA, NWB, NWC, KEY
      INTEGER IPL, L1, N3, N, N1, NHITS, NW, NBI, NS
      REAL    RT1, RT2,  WDA, WDB, WDC, WDZ
      REAL    PXA, PYA, PXB, PYB, PXC, PYC, SBA, SBB, SBC
      REAL    RA, RB, RC, ZA, ZB, ZC, DR, RTEMP, DB
      REAL    RTA, RMIN, RMAX, TB
      REAL    XY, AMN, AMX, TBM, TAM, ZMAG
      CHARACTER*4 HSHAPE
      INTEGER NSPAR, IBUF, NBUF(7)
      REAL    SPAR(6), XPAR(3), ROTM(3,3)
      INTEGER IBITS
      LOGICAL FIRST
      INTEGER NTRPL, NTRG2
      REAL    DR2AB, TG2AB, DISCM, ZBEAM, PPAR1, PPAR2
      SAVE    NTRPL, NTRG2, FIRST
      SAVE    DR2AB, TG2AB, DISCM, ZBEAM, PPAR1, PPAR2
      INTEGER IERR
      DATA FIRST /.TRUE./
C
C ****  calculate initial parameters
C
      LTRG2 = 0
      LSAHS = GZSAHS ()
      IF (LSAHS .LE. 0) GO TO 999
      IF (FIRST) THEN
        CALL EZPICK ('SAMUS_UTIL_PARAM')
        CALL EZGET  ('NTRPL', NTRPL, IERR)
        CALL EZGET  ('NTRG2', NTRG2, IERR)
        CALL EZGET  ('TG2AB', TG2AB, IERR)
        CALL EZGET  ('DR2AB', DR2AB, IERR)
        CALL EZGET  ('DISCM', DISCM, IERR)
        CALL EZGET  ('ZBEAM', ZBEAM, IERR)
        CALL EZGET  ('PPAR1', PPAR1, IERR)
        CALL EZGET  ('PPAR2', PPAR2, IERR)
        CALL EZRSET
        FIRST = .FALSE.
      END IF
      IQ(LSAHS+30+DIR) = 0
      IF (DIR .EQ. 1) THEN
        NST = 1
      ELSE
        NST = 4
      END IF
      CALL GTSMAG (DIR, HSHAPE, NSPAR, SPAR, XPAR, ROTM,
     &             NBUF, IBUF)
      ZMAG = XPAR(3)
      ZMAG = ZMAG - ZBEAM
C
C ****  check number of 3-plets in A,B and C stations
C
      DO II=1,3
        INDX(II) = 17+II + NST
        MX(II) = IQ(LSAHS+INDX(II))       ! number of the 3-plets in station
        IF (MX(II) .LE. 0) GO TO 999
      END DO

C
      IF (DIR .EQ. 1) THEN
        LTRK = 1 + GZSATN()
      ELSE
        LTRK = 1 + GZSATS()
      END IF
      IQ(LTRK) = 0               ! reset number of tracks in direction
C
C ****  find PHI-coincidence in A, B and C stations
C
      LDB = LQ(LSAHS-INDX(2))      ! address of the 3-plets bank in B
      DO 100 NB = 1, MX(2)
C
C ****  loop on 3-plets in B station
C
        NWB = IQ(LDB+1)
        IF (NWB .GE. 0 .AND. NWB .LT. NTRPL) GO TO 100
        IF (NWB .GE. 32) NWB = 32
        PXB = Q(LDB+2)
        PYB = Q(LDB+3)
        RB = SQRT (PXB * PXB + PYB * PYB)
        ZB = Q(LDB+4)
        ZB = ZB - ZBEAM
        TB = RB / ZB
        KEY = 0
        IF (ABS(PXB) .GE. ABS(PYB)) KEY = 1
        IF (KEY .EQ. 0) THEN
          RTEMP = PXB
          PXB = PYB
          PYB = RTEMP
        END IF
        SBB = ABS(PXB / RB)
        XY = PYB / PXB
        DB = SBB * TG2AB
C
C ****  loop on 3-plets in A station
C
        LDA = LQ(LSAHS-INDX(1))    ! address of the 3-plets bank in A
        DO 200 NA = 1, MX(1)
          NWA = IQ(LDA+1)
          IF (NWA .GE. 0 .AND. NWA .LT. NTRPL) GO TO 200
          IF (NWA .GE. 32) NWA = 32
          PXA = Q(LDA+2)
          PYA = Q(LDA+3)
          ZA =  Q(LDA+4)
          ZA = ZA - ZBEAM
          RA = SQRT (PXA * PXA + PYA * PYA)
          RTA = TB * ZA
          RMIN = RTA - DR2AB
          RMAX = RTA + DR2AB
          IF (RA .LE. RMIN .OR. RA .GE. RMAX) GO TO 200
          IF (KEY .EQ. 0) THEN
            RTEMP = PXA
            PXA = PYA
            PYA = RTEMP
          END IF
          IF (PXA .GT. 0. .AND. PXB .LE. 0.) GO TO 200
          IF (PXA .LT. 0. .AND. PXB .GE. 0.) GO TO 200
          SBA = XY * PXA
          AMN = SBA - DB
          AMX = SBA + DB
          IF (PYA .LT. AMN .OR. PYA .GT. AMX) GO TO 200
C
C ****  loop on 3-plets in C station
C
          LDC = LQ(LSAHS-INDX(3))    ! address of the 3-plets bank in C
          DO 300 NC = 1, MX(3)
            NWC = IQ(LDC+1)
            IF (NWC .GE. 0 .AND. NWC .LT. NTRPL) GO TO 300
            IF (NWC .GE. 32) NWC = 32
            PXC = Q(LDC+2)
            PYC = Q(LDC+3)
            ZC =  Q(LDC+4)
            ZC = ZC - ZBEAM
            RC = SQRT (PXC * PXC + PYC * PYC)
            IF (KEY .EQ. 0) THEN
              RTEMP = PXC
              PXC = PYC
              PYC = RTEMP
            END IF
            IF (PXB .GT. 0. .AND. PXC .LE. 0.) GO TO 300
            IF (PXB .LT. 0. .AND. PXC .GE. 0.) GO TO 300
            SBC = XY * PXC
            AMN = SBC - 1.5 * DB
            AMX = SBC + 1.5 * DB
            IF (PYC .LT. AMN .OR. PYC .GT. AMX) GO TO 300
C
C ****  check cross point in the center of the magnet
C
            TBM = RA / ZA
            TAM = (RC - RB) / (ZC - ZB)
            RT1 = TBM * ZMAG
            RT2 = RC - TAM * (ZC - ZMAG)
            DR = ABS(RT1 - RT2)
            IF (DR .LT. DISCM) THEN
              WDA = (SBA - PYA) / DB
              WDC = (SBC - PYC) / (1.5 * DB)
              WDZ = DR / DISCM
              NS = NWA + NWB + NWC
              WDB = WDA * WDA + WDC * WDC + WDZ * WDZ
              LTR2 = LTRG2
  500         CONTINUE
              IF (LTR2 .GT. 0) THEN
                IF (NA .NE. NXX(LTR2,1)) THEN
                  IF (NB .NE. NXX(LTR2,2)) THEN
                    IF (NC .NE. NXX(LTR2,3)) THEN
                      LTR2 = LTR2 - 1
                      GO TO 500
                    END IF
                  END IF
                END IF
                IF (NS .LT. NAC(LTR2)) GO TO 300
                IF (NS .EQ. NAC(LTR2)) THEN
                  IF (WDB .GE. WDD(LTR2)) GO TO 300
                END IF
                LT2 = LTR2
              ELSE
                LTRG2 = LTRG2 + 1
                LT2 = LTRG2
                IF (LTRG2 .GT. NTRG2) LTRG2 = 0
                IF (LTRG2 .LE. 0) GO TO 999
              END IF
              NXX(LT2,1) = NA
              NXX(LT2,2) = NB
              NXX(LT2,3) = NC
              NAC(LT2) = NS
              WDD(LT2) = WDB
            END IF
  300     LDC = LDC + 4
  200   LDA = LDA + 4
  100 LDB = LDB + 4
      IF (LTRG2 .LE. 0) GO TO 999
      LT2 = 0
      DO II=1,3
        LDX(II) = LQ(LSAHS-INDX(II))    ! address of the 3-plets bank in A
      END DO
      DO NLT = 1, LTRG2
        DO II=1,3
          NJX(II) = LDX(II) + 4 * NXX(NLT,II) - 3
          IQ(NJX(II)) = -NLT
        END DO
C
C ****  mark "good" hits after "TRIGGER" procedure
C
        DO IST = 1,3
          N3=NXX(NLT,IST)
          NW = (N3 - 1) / 32
          IF (NW+5 .LE. 13) THEN
            NBI = N3 - NW * 32 - 1
            DO IPL = 1, N_PLANES
              IND = 3 * (IST+NST - 2) + IPL
              L1 = LQ(LSAHS-IND)          ! address of the hits
              NHITS = IQ(LSAHS+IND)
              DO N1 = 1, NHITS
                N = IBITS (IQ(L1+NW+5), NBI, 1)
                IF (N .EQ. 1) IQ(L1+1) = 2
                L1 = L1 + 13
              END DO
            END DO
          END IF
        END DO
C
C ****  tracks reconstruction without times
C
        KEY = 1
        CALL SAL3BM (DIR, NTRB)
        IF (NTRB .NE. 0) THEN
          CALL SAL3AM (DIR, NTRA)
          IF (NTRA .NE. 0) THEN
            CALL SAL3LN (DIR, NTRK)
            IF (NTRK .NE. 0) THEN
              KEY=0
              LT2=LT2+1
            END IF
          END IF
        END IF
C
C ****  mark "USED" hits
C
        DO IST = 1,3
          N3=NXX(NLT,IST)
          NW = (N3 - 1) / 32
          IF (NW+5 .LE. 13) THEN
            NBI = N3 - NW * 32 - 1
            DO IPL = 1, N_PLANES
              IND = 3 * (IST - 2+NST) + IPL
              L1 = LQ(LSAHS-IND)          ! address of the hits
              NHITS = IQ(LSAHS+IND)
              DO N1 = 1, NHITS
                IF (IQ(L1+1) .EQ. 2) IQ(L1+1) = 1
                L1 = L1 + 13
              END DO
            END DO
          END IF
        END DO
        IF (KEY .EQ. 1) THEN
          DO II=1,3
            IQ(NJX(II)) = NLT
          END DO
        END IF
      END DO
C
C ****  mark "good" hits after "RECONSTRUCTION" procedure
C
      DO II=1,3
        LDX(II) = LQ(LSAHS-INDX(II))    ! address of the 3-plets bank
      END DO
      DO NLT = 1, LTRG2
        DO II=1,3
          NJX(II) = LDX(II) + 4 * NXX(NLT,II) - 3
        END DO
        DO IST = 1,3
          N3=NXX(NLT,IST)
          NW = (N3 - 1) / 32
          IF (NW+5 .LE. 13) THEN
            NBI = N3 - NW * 32 - 1
            DO IPL = 1, N_PLANES
              IND = 3 * (IST - 2+NST) + IPL
              L1 = LQ(LSAHS-IND)          ! address of the hits
              NHITS = IQ(LSAHS+IND)
              DO N1 = 1, NHITS
                N = IBITS (IQ(L1+NW+5), NBI, 1)
                IF (N .EQ. 1) IQ(L1+1) = 2
                L1 = L1 + 13
              END DO
            END DO
          END IF
        END DO
      END DO
C
      LTRG2 = LT2
  999 CONTINUE
      IQ(LSAHS+30+DIR) = LTRG2
      RETURN
      END
