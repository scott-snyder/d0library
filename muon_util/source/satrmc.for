C+
      SUBROUTINE SATRMC (DIR,MT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : S/R for SAMUS tracks momentum calculation.
C-
C-   Inputs  : DIR - SAMUS direction number.
C-   Outputs : MT  - number of good tracks
C-   Controls: none.
C-
C-   Created   6-SEP-1993   Alexander Efimov
C-   Updated  30-JAN-1994   Alexander Efimov - add momentum correction
C-                          with the energy loss in calorimeter.
C-   Updated   7-FEB-1994   Alexander Efimov
C-   Updated  19-FEB-1994   Alexander Efimov  add BDL calculation
C-   Updated  12-DEC-1994   Andrei Mayorov  add MT to output list
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER DIR
      INTEGER N_STATION
      PARAMETER (N_STATION=3)
      COMMON /COMSAFCPF/ JTRK, ZMAG, ZMAG1, ZMAG2, VERTEX(3),
     &                   XM(4), YM(4), ZM(4), CHARGE,
     &                   POUT, DVERTX, ZVERTX, BDL
      REAL    ZMAG, ZMAG1, ZMAG2, VERTEX, XM, YM, ZM, CHARGE
      REAL    POUT, DVERTX, ZVERTX, BDL
      INTEGER JTRK
      INTEGER LENTRK
      PARAMETER (LENTRK=150)
      INTEGER LTRK, GZSATN, GZSATS, NTRK, NEXT
      REAL    CENTER(3), ANGLES(3), SIZE(3), HOLE(3)
      INTEGER IVER, NV, NT, MT, NST, J, JT, MTRK, INSERT(4)
      REAL    X1, Y1, X2, Y2, XS, YS, ZS, ELFE, ELCAL
      REAL    WEIGHT(4), PMAG, W, ZZ, XI2, R, A, B
      CHARACTER*4 HSHAPE
      INTEGER NSPAR, IBUF, NBUF(7)
      REAL    SPAR(6), XPAR(3), ROTM(3,3)
      REAL    PAR, STEP_PAR
      EXTERNAL SAFCPF
      LOGICAL FIRST
      SAVE    FIRST
      REAL    DISVX
      SAVE    DISVX
      INTEGER IERR
      DATA FIRST /.TRUE./
C
C ****  initializing
C
      IF (FIRST) THEN
        CALL EZPICK ('SAMUS_UTIL_PARAM')
        CALL EZGET  ('DISVX', DISVX, IERR)
        CALL EZRSET
        FIRST = .FALSE.
      END IF
      IF (DIR .EQ. 1) THEN
        LTRK = GZSATN ()
      ELSE
        LTRK = GZSATS ()
      END IF
      LTRK = LTRK + 1
      NTRK = IQ(LTRK)
      IF (NTRK .EQ. 0) GO TO 999
      CALL GTSMAG (DIR, HSHAPE, NSPAR, SPAR, XPAR, ROTM,
     &             NBUF, IBUF)
      ZMAG = XPAR(3)
      IF (ZMAG .GT. 0.0) THEN
        ZMAG1 = ZMAG - SPAR(6)
        ZMAG2 = ZMAG + SPAR(6)
      ELSE
        ZMAG1 = ZMAG + SPAR(6)
        ZMAG2 = ZMAG - SPAR(6)
      END IF
      CALL VERXYZ (IVER, VERTEX, NV)
C
C ****  tracks loop
C
      DO NT = 1, NTRK
        JTRK = LTRK + (NT - 1) * LENTRK
C
C ****  moment calculation without energy loses in SAMUS toroids
C
        JT = JTRK + 4
        ZZ = VERTEX(3)
        W = (ZZ - Q(JT+3)) / Q(JT+6)
        XM(1) = Q(JT+1) + W * Q(JT+4)
        YM(1) = Q(JT+2) + W * Q(JT+5)
        ZM(1) = ZZ
        WEIGHT(1) = 1.0
        INSERT(1) = 0
        DO J = 1, N_STATION
          NST = N_STATION * (DIR - 1) + J
          CALL SAGSTA (NST, CENTER, ANGLES, SIZE, HOLE)
          ZZ = CENTER(3)
          IF (J .EQ. 1) THEN
            JT = JTRK + 4
          ELSE
            JT = JTRK + 10
          END IF
          W = (ZZ - Q(JT+3)) / Q(JT+6)
          XM(J+1) = Q(JT+1) + W * Q(JT+4)
          YM(J+1) = Q(JT+2) + W * Q(JT+5)
          ZM(J+1) = ZZ
          WEIGHT(J+1) = 1.0
          INSERT(J+1) = 0
        END DO
        INSERT(2) = 50
        CALL SAPGEV (4, XM, YM, ZM, WEIGHT, INSERT, PMAG, IERR)
        IF (IERR .NE. 0) THEN
          INSERT(2) = 10
          CALL SAPGEV (4, XM, YM, ZM, WEIGHT, INSERT, PMAG, IERR)
        END IF
        IF (PMAG .GT. 0.0) THEN
          CHARGE = +1.0
        ELSE
          CHARGE = -1.0
        END IF
        PMAG = ABS(PMAG)
C
C ****  calculate energy loss in iron and calorimeter
C
        W = (ZMAG - Q(JTRK+7)) / Q(JTRK+10)
        X1 = Q(JTRK+5) + W * Q(JTRK+8)
        Y1 = Q(JTRK+6) + W * Q(JTRK+9)
        W = (ZMAG - Q(JTRK+13)) / Q(JTRK+16)
        X2 = Q(JTRK+11) + W * Q(JTRK+14)
        Y2 = Q(JTRK+12) + W * Q(JTRK+15)
        XS = 0.5 * (X1 + X2)
        YS = 0.5 * (Y1 + Y2)
        ZS = ZMAG
        CALL MUELOS (12+DIR, PMAG, XS, YS, ZS,
     &               Q(JTRK+8), Q(JTRK+9), Q(JTRK+10),
     &               Q(JTRK+14), Q(JTRK+15), Q(JTRK+16),
     &               ELFE, ELCAL)
        PMAG = PMAG + ELFE
C
C ****  moment calculation with energy loses in SAMUS toroids
C
        PAR = PMAG
        A = 1.0
        B = 1001.0
        STEP_PAR = 0.1 * PAR
        IF (PAR-STEP_PAR .LT. A) THEN
          STEP_PAR = (PAR - A) / 2.0
        END IF
        IF (PAR+STEP_PAR .GT. B) THEN
          STEP_PAR = (B - PAR) / 2.0
        END IF
        CALL MINVAR (PAR, XI2, R, 0.001, STEP_PAR, 1000, A, B, SAFCPF)
        CALL MUELOS (12+DIR, POUT, XS, YS, ZS,
     &               Q(JTRK+8), Q(JTRK+9), Q(JTRK+10),
     &               Q(JTRK+14), Q(JTRK+15), Q(JTRK+16),
     &               ELFE, ELCAL)
        IF (POUT .GT. 0.0) THEN
          POUT = POUT + ELCAL
        ELSE
          POUT = POUT - ELCAL
        END IF
        Q(JTRK+2) = POUT
        Q(JTRK+23) = DVERTX
        Q(JTRK+24) = ELCAL
        Q(JTRK+25) = ELFE
        Q(JTRK+149) = BDL * 1.0E-3
      END DO
C
C ****  delete "bad" tracks
C
      MT = 0
      DO NT = 1, NTRK
        JTRK = LTRK + (NT - 1) * LENTRK
        DVERTX = Q(JTRK+23)
        IF (DVERTX .LT. DISVX) THEN
          MT = MT + 1
          MTRK = LTRK + (MT - 1) * LENTRK
          DO J = 1, LENTRK
            Q(MTRK+J) = Q(JTRK+J)
          END DO
        END IF
      END DO
      IQ(LTRK) = MT
C
  999 CONTINUE
      RETURN
      END
