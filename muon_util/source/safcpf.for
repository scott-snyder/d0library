C+
      REAL FUNCTION SAFCPF (PAR, FLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate the sum of distance between
C-                         track and hits.
C-
C-   Returned value  : summary deviation.
C-   Inputs  : PAR - input parameter,
C-             FLAG - does not used.
C-   Outputs : none.
C-   Controls: none.
C-
C-   Created  18-DEC-1992   Alexander Efimov
C-   Updated  19-DEC-1992   Alexander Efimov
C-   Updated  15-JAN-1993   Dmitri Denisov - updated VERXYZ
C-   Corrected 16-JAN -1993 Dmitri Denisov - corrected sign for PGEV
C-   Updated  30-JAN-1994   Alexander Efimov - add corrections with
C-                          the multiple scattering in the calorimeter
C-   Updated  11-FEB-1994   Alexander Efimov   
C-   Updated  19-FEB-1994   Alexander Efimov  add BDL calculation 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER FLAG
      REAL    PAR
      COMMON /COMSAFCPF/ JTRK, ZMAG, ZMAG1, ZMAG2, VERTEX(3),
     &                   XM(4), YM(4), ZM(4), CHARGE,
     &                   POUT, DVERTX, ZVERTX, BDL
      REAL    ZMAG, ZMAG1, ZMAG2, VERTEX, XM, YM, ZM, CHARGE
      REAL    POUT, DVERTX, ZVERTX, BDL
      INTEGER JTRK
      REAL    PGEV, VECT(6), F(3), STEP, DIST, P, DP
      REAL    XX, YY, ZZ, W, W1, W2, F1, F2, SGMUL
      REAL    BEAM(6)
      REAL    MUFEDE
      EXTERNAL MUFEDE
      REAL    PMIN
      REAL    RDRTMB, SXVER, SYVER, SZVER, PGSTP, SMULS
      SAVE    RDRTMB, SXVER, SYVER, SZVER, PGSTP, SMULS
      INTEGER J, IERR
      LOGICAL FIRST
      SAVE    FIRST
      DATA FIRST /.TRUE./
      DATA    PMIN /1.0/
C
C ****  initializing
C
      IF (FIRST) THEN
        CALL EZPICK ('SAMUS_UTIL_PARAM')
        CALL EZGET  ('RDRTMB', RDRTMB, IERR)
        CALL EZGET  ('SXVER', SXVER, IERR)
        CALL EZGET  ('SYVER', SYVER, IERR)
        CALL EZGET  ('SZVER', SZVER, IERR)
        CALL EZGET  ('PGSTP', PGSTP, IERR)
        CALL EZGET  ('SMULS', SMULS, IERR)
        CALL EZRSET
        FIRST = .FALSE.
      END IF
C
C ****  move track through magnetic field
C
      PGEV = - PAR * CHARGE
      DO J = 1, 6
        VECT(J) = Q(JTRK+10+J)
      END DO
      DIST = (ZMAG2 - VECT(3)) / VECT(6)
      DO J = 1, 3
        VECT(J) = VECT(J) + DIST * VECT(J+3)
      END DO
      IF (ZMAG2*VECT(6) .GT. 0.0) THEN
        DO J = 4, 6
          VECT(J) = - VECT(J)
        END DO
      END IF
      STEP = PGSTP
      BDL = 0.0
    1 CONTINUE
      CALL SATRNT (PGEV, STEP, VECT)
      P = ABS(PGEV)
      DP = STEP * MUFEDE(P)
      IF (PGEV .GT. 0.0) THEN
        PGEV = PGEV + DP
      ELSE
        PGEV = PGEV - DP
      END IF
      CALL SAFLD (VECT, F)
      W1 = F(1) * VECT(4) + F(2) * VECT(5) + F(3) * VECT(6)
      W2 = (F(1) - W1 * VECT(4))**2 + (F(2) - W1 * VECT(5))**2 +
     &     (F(3) - W1 * VECT(6))**2
      BDL = BDL + STEP * SQRT(W2)
      IF (ABS(VECT(3)) .GT. ABS(ZMAG1) .AND.
     &    ABS(VECT(3)) .LT. ABS(ZMAG2)) GO TO 1
      POUT = - PGEV
C
C ****  calculate summury derivation of the hits before magnet
C
      SGMUL = SMULS / ABS(POUT)
      CALL SADSPL (VERTEX, VECT, DIST, W)
      XX = VECT(1) + W * VECT(4)
      YY = VECT(2) + W * VECT(5)
      ZZ = VECT(3) + W * VECT(6)
      XX = (XX - VERTEX(1))**2
      YY = (YY - VERTEX(2))**2
      ZZ = (ZZ - VERTEX(3))**2
      XX = XX / (SXVER**2 + SGMUL**2)
      YY = YY / (SYVER**2 + SGMUL**2)
      ZZ = ZZ / (SZVER**2 + SGMUL**2)
      F1 = XX + YY + ZZ
      DIST = (ZM(2) - VECT(3)) / VECT(6)
      XX = VECT(1) + DIST * VECT(4)
      YY = VECT(2) + DIST * VECT(5)
      DIST = (XX - XM(2))**2 + (YY - YM(2))**2
      W = RDRTMB / 6.0
      F2 = DIST / (W**2)
      SAFCPF = SQRT (F1 + F2)
C
C ****  calculate distance from track to the beam line
C
      DO J = 1, 3
        BEAM(J) = VERTEX(J)
        BEAM(J+3) = 0.0
      END DO
      BEAM(6) = 1.0
      CALL SADS2L (VECT, BEAM, DIST, W1, W2, IERR)
      DVERTX = SQRT (DIST)
      ZVERTX = W2 - VERTEX(3)
C
      RETURN
      END
