      SUBROUTINE SATG1A (NST, N3P)
C----------------------------------------------------------------------
C-
C-  Purpose and methods:   Finding of intersections of tubes from
C-                         three different planes (X, Y and U) - 3-plets
C-                         search and mark "bad" hits.
C-
C-   Inputs  : NST - station number, NA=1,  SA=4, NB=2, SB=5, NC=3, SC=6
C-
C-   Outputs : N3P - number of the founded 3p-points
C-   Controls: none.
C-
C-   Created  10-SEP-1992   Alexei Volkov
C-   Updated  28-OKT-1992   Alexei Volkov
C-   Updated  13-NOV-1992   Alexander Efimov: change SAMUS banks
C-                                            structure
C-   Updated  10-DEC-1992   Alexei Volkov
C-   Updated  27-DEC-1992   Alexei Volkov
C-   Modified 13-APR-1993   Tom Diehl: operates once per station per event.
C-   Modified 30-NOV-1993   Denisov: add A.Efimovs modifications for run Ib
C-                          SNGLH and remove half triplets number for
C-                          B ststions
C-   Updated  28-JAN-1994   Alexei Volkov
C-   Updated   4-SEP-1994   Andrei Mayorov  spead up code : IBITS - .AND.
C-                                            check if new hit has the
C-                                            same x,y as previous one
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INTEGER NST, N3P
      REAL    ROAD1
      REAL    X, Y, U, RLX, RLY
      INTEGER LSAHS, GZSAHS
      REAL    PX, PY, PZ, GX, GY, GZ, DELX, DELY
      INTEGER NT1, NT2, NT3, N1, N2, N3
      INTEGER NX, NY, NU, NWS
      INTEGER NSH, NL1, NL2, NUMIN, NUMAX
      INTEGER LD1, LD2, LD3, L3P, ND3P, M3P, I3P, IND
      INTEGER LC1, LC2, LC3
      INTEGER I, J, K, NW, NB, NP, NN
      INTEGER SNGLH
      INTEGER EVENT_NUM_LOW, LAST_EVENT_NUM_LOW
      INTEGER EVENT_NUM_HIGH, LAST_EVENT_NUM_HIGH
      DATA LAST_EVENT_NUM_LOW,EVENT_NUM_HIGH/-1,-1/
      SAVE LAST_EVENT_NUM_LOW,LAST_EVENT_NUM_HIGH
      LOGICAL DONE_PLANE(6)
      LOGICAL FIRST
      SAVE FIRST, ROAD1
      INTEGER IERR
      INTEGER NKX(3), KN(3), KX(3)
      DATA FIRST /.TRUE./
      DATA NKX /215, 215, 301/
      DATA KN /87, 87, 125/
      DATA KX /128, 128, 176/
C
      INTEGER*2 DBYTE(2),NKS(3),NS1,NS2,NS3,MASK
      EQUIVALENCE ( DBYTE(1),NSH  )
      DATA NKS /128, 192, 160/
      DATA MASK/992/
      INTEGER XYFLAG,JB,JE,JS
C
C ****  initializing
C
      EVENT_NUM_LOW = IQ(LHEAD+7)                  ! ONCE PER PLANE PER EVENT
      EVENT_NUM_HIGH = IQ(LHEAD+8)                 ! CHECK
      IF (EVENT_NUM_LOW .NE. LAST_EVENT_NUM_LOW .OR.
     $  EVENT_NUM_HIGH .NE. LAST_EVENT_NUM_HIGH) THEN
        LAST_EVENT_NUM_LOW = EVENT_NUM_LOW
        LAST_EVENT_NUM_HIGH = EVENT_NUM_HIGH
        DO I = 1, 6
          DONE_PLANE(I) = .FALSE.
        END DO
      END IF
      N3P = 0
      LSAHS = GZSAHS ()
      IF (LSAHS .LE. 0) RETURN
      IF (DONE_PLANE(NST)) THEN
        N3P = IQ(LSAHS+18+NST)
        RETURN
      END IF
      DONE_PLANE(NST) = .TRUE.
      IF (FIRST) THEN
        CALL EZPICK ('SAMUS_UTIL_PARAM')
        CALL EZGET  ('ROAD1', ROAD1, IERR)
        CALL EZGET_i  ('SNGLH', SNGLH, IERR)
        CALL EZRSET
        FIRST = .FALSE.
      END IF
C
C ****  get number of hits in planes
C
      N3P = 0
      IQ(LSAHS+18+NST) = 0
      IND = 3 * (NST - 1)
      NT1 = IQ(LSAHS+IND+1)           ! number of hits in plane 1
      NT2 = IQ(LSAHS+IND+2)           ! number of hits in plane 2
      NT3 = IQ(LSAHS+IND+3)           ! number of hits in plane 3
      IF (NT1 .EQ. 0 .OR. NT2 .EQ. 0 .OR. NT3 .EQ. 0) RETURN
      NL1 = KN(1)     ! minimum number of short tubes in X and Y plan
      NL2 = KX(1)     ! maximum number of short tubes in X and Y plan
      IF (NST .EQ. 3 .OR. NST .EQ. 6) NL1 = 78
      IF (NST .EQ. 3 .OR. NST .EQ. 6) NL2 = 137
      L3P = LQ(LSAHS-18-NST)     ! address of the 3-hits bank SAH3
      I3P = L3P
      ND3P = IQ(L3P-1)           ! number of the data nshs in bank
      M3P = L3P + ND3P
C
C ****  find isolated hits in plane
C
      IF (SNGLH .NE. 0) THEN
        DO NP = 1, 3
          NN = NT1
          IF (NP .EQ. 2) NN = NT2
          IF (NP .EQ. 3) NN = NT3
          LD1 = LQ(LSAHS-IND-NP)           ! address of the hits in plane
          LD2 = LD1
          DO N1 = 1, NN
            NSH = IQ(LD2+2)
            NX = DBYTE(WORD2)
            NS1=iand(DBYTE(WORD1),MASK)
            IF (NS1.EQ.NKS(NP)) NX = NKX(NP) - NX
            K = 0
            DO 1 N2 = 1, NN
              IF (N2 .EQ. N1) GO TO 1
              LD3 = LD1 + (N2-1) * 13
              IF (IQ(LD3+13) .EQ. 1) GO TO 1
              NSH = IQ(LD3+2)
              NY = DBYTE(WORD2)
              NS2=iand(DBYTE(WORD1),MASK)
              IF (NS2.EQ.NKS(NP)) NY = NKX(NP) - NY
              IF (IABS(NX-NY) .LE. 2) THEN
                K = K + 1
                IF (NS1 .NE. NS2) THEN
                  IF (NY .GE. KN(NP) .AND. NY .LE. KX(NP)) THEN
                    IF (NX .GE. KN(NP) .AND. NX .LE. KX(NP)) K = K - 1
                  END IF
                END IF
              END IF
    1       CONTINUE
            IF (K .EQ. 0) IQ(LD2+13) = SNGLH
            LD2 = LD2 + 13
          END DO
        END DO
      END IF
C
C ****  find 3-points, mark good hits and fill bank 'SAM3'
C
      LD1 = LQ(LSAHS-IND-1)           ! address of the hits in plane 1
      DO 101 N1 = 1, NT1
        IF (IQ(LD1+13) .EQ. 2) GO TO 101
        LC1 = IQ(LD1+3)               ! address of the tube geometry
        PX = C(LC1+1)
        NSH = IQ(LD1+2)
        NX = DBYTE(WORD2)
        NS1=iand(DBYTE(WORD1),MASK)
        IF (NS1.EQ.NKS(1)) NX = NKX(1) - NX
        X = NX
        LD2 = LQ(LSAHS-IND-2)         ! address of the hits in plane 2
        DO 102 N2 = 1, NT2
          IF (IQ(LD2+13) .EQ. 2) GO TO 102
          IF (IQ(LD1+13) .EQ. 1 .AND. IQ(LD2+13) .EQ. 1) GO TO 102
          LC2 = IQ(LD2+3)             ! address of the tube geometry
          PY = C(LC2+2)
          IF (ABS(C(LC1+2)-PY) .GT. C(LC1+7)) GO TO 102
          IF (ABS(C(LC2+1)-PX) .GT. C(LC2+7)) GO TO 102
 
          NSH = IQ(LD2+2)
          NY = DBYTE(WORD2)
          NS2=iand(DBYTE(WORD1),MASK)
          IF (NS2.EQ.NKS(2)) NY = NKX(2) - NY
          IF (NX .GE. NL1 .AND. NX .LE. NL2) THEN
            IF (NY .GE. NL1 .AND. NY .LE. NL2) GO TO 102
          END IF
          Y = NY
          PZ = (C(LC1+3) + C(LC2+3)) / 2.0
          U = (X + Y) / SQRT(2.0) - 1.5
          NU = U
          NUMIN = NU - 4
          NUMAX = NU + 4
          LD3 = LQ(LSAHS-IND-3)       ! address of the hits in plane 3
          XYFLAG=0
          DO 103 N3 = 1, NT3
            IF (IQ(LD3+13) .EQ. 2) GO TO 103
            IF (IQ(LD1+13) .EQ. 1 .AND. IQ(LD3+13) .EQ. 1) GO TO 103
            IF (IQ(LD2+13) .EQ. 1 .AND. IQ(LD3+13) .EQ. 1) GO TO 103
            NSH = IQ(LD3+2)
            NU = DBYTE(WORD2)
            NS3=iand(DBYTE(WORD1),MASK)
            IF (NS3.EQ.NKS(3)) NU = NKX(3) - NU
            IF (NU .LT. NUMIN .OR. NU .GT. NUMAX) GO TO 103
            IF (NU .GE. KN(3) .AND. NU .LE. KX(3)) THEN
              IF (X .GT. Y) THEN
                IF (NS3 .NE.NKS(3)) GO TO 103
              ELSE
                IF (NS3 .EQ. NKS(3)) GO TO 103
              END IF
            END IF
            LC3 = IQ(LD3+3)           ! address of the tube geometry
            IF (L3P+4 .GT. M3P) THEN
              IQ(LSAHS+18+NST) = 0
              N3P = 0
              RETURN
            END IF
            IF(XYFLAG.EQ.0) THEN
              JB=1
              JE=N3P
              I = I3P
            ELSE
              JB=JS
              JE=JS
              I=XYFLAG
            END IF                              ! find
            DO J = JB, JE                       ! the same
              GX = Q(I+2)                       ! 3-hit
              GY = Q(I+3)
              DELX = ABS(GX-PX)
              DELY = ABS(GY-PY)
              NW = IQ(I+1)       ! add current 3-hit to the existing
              RLX = ROAD1
              RLY = ROAD1
              IF (IQ(LD1+13) .EQ. 1) RLX = 4.0
              IF (IQ(LD2+13) .EQ. 1) RLY = 4.0
              IF (DELX .LT. RLX) THEN
                IF (DELY .LT. RLY) THEN
                  GZ = Q(I+4)
                  NWS = 1
                  IF (SNGLH .EQ. 1) NWS = 4
                  IF (IQ(LD1+13) .EQ. 1) NWS = 1
                  IF (IQ(LD2+13) .EQ. 1) NWS = 1
                  IF (IQ(LD3+13) .EQ. 1) NWS = 1
                  IQ(I+1) = NW + NWS
                  Q(I+2) = (REAL(NWS) * PX +
     *                     GX * REAL(NW)) / REAL(NW + NWS)
                  Q(I+3) = (REAL(NWS) * PY +
     *                     GY * REAL(NW)) / REAL(NW + NWS)
                  Q(I+4) = (REAL(NWS) * PZ +
     *                     GZ * REAL(NW)) / REAL(NW + NWS)
                  GO TO 104
                END IF
              END IF
              I = I + 4
            END DO
            NWS = 1
            IF (SNGLH .EQ. 1) NWS = 4
            IF (IQ(LD1+13) .EQ. 1) NWS = 1
            IF (IQ(LD2+13) .EQ. 1) NWS = 1
            IF (IQ(LD3+13) .EQ. 1) NWS = 1
            IQ(L3P+1) = NWS        ! 3-hit flag
            Q(L3P+2) = PX          ! X coordinate of the 3-hit
            Q(L3P+3) = PY          ! Y coordinate of the 3-hit
            Q(L3P+4) = PZ          ! Z coordinate of the 3-hit
            N3P = N3P + 1
            L3P = L3P + 4
            J = N3P
  104       CONTINUE
            JS=J
            XYFLAG=I
            IQ(LD1+1) = 1
            IQ(LD2+1) = 1
            IQ(LD3+1) = 1
            NW = (J - 1) / 32
            NB = J - NW * 32
            IF (NW+5 .LE. 12) THEN
              CALL SBYT (1, IQ(LD1+NW+5), NB, 1)
              CALL SBYT (1, IQ(LD2+NW+5), NB, 1)
              CALL SBYT (1, IQ(LD3+NW+5), NB, 1)
            END IF
  103     LD3 = LD3 + 13
  102   LD2 = LD2 + 13
  101 LD1 = LD1 + 13
      IQ(LSAHS+18+NST) = N3P
      RETURN
      END
