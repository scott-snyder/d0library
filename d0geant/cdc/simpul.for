      SUBROUTINE SIMPUL(DTTYPE,TDRIFT,AREA,ITYPE,PULSE,DIGFLG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : simulate pulse shape based on the Rayleigh
C-             function; the position of the weighted center of gravity
C-             of the positive part of the differential pulse corresponds
C-             to the drift time
C-
C-   Inputs  :
C-             DTTYPE:  detector type (0:VTX, 1:CDC, 2:FDC)
C-             TDRIFT:  drift time
C-               AREA:  pulse area
C-              ITYPE:  =0 for sense wire, =1 for delay line
C-   Outputs : PULSE: simulated pulse data
C-             DIGFLG: =1 digitized, =0 not digitized
C-
C-   Created  27-MAR-1989   Qizhong Li-Demarteau
C-   Updated   7-AUG-1989   Jeffrey Bantly : limit filling PULSE to 512 ch.
C-   Updated  13-DEC-1989   Qizhong Li-Demarteau   add a check at ITMIN
C-   Updated  25-FEB-1990   Peter Grudberg - fix COEF arrays, fadc length (256)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZPULPR.INC/LIST'
C
      REAL    FADCFR
      REAL    TRISE, TWIDTH
      REAL    TMIN, FADC1, FADC(20), B(20), SUM, SUMX
      REAL    AMP, AMP1, COEFF(20,0:2), COEF2(20,0:2)
      REAL    TDRIFT, AREA, PULSE(0:255)
      INTEGER ITMIN, ITYPE, DIGFLG, DTTYPE, TYPE
      INTEGER I, J, ID, IWIDTH, INIT
C
C PARTR and PARTW are fitted parameters from real data (x is DTTYPE)
C
C   Rise time = PARTR(1,x) * SQRT(Tdrift) + PARTR(2,x)
C               (for uniform drift region: Tdrift > TRLIMT(x))
C             = PARTR(3,x) * Tdrift ** 2 + PARTR(4,x) * Tdrift + PARTR(5,x)
C               (for the non-uniform drift region: 0 < Tdrift < TRLIMT(x))
C
C Total width = PARTW(1,x) * SQRT(Tdrift) + PARTW(2,x)
C               (for uniform drift region: Tdrift > TWLIMT(x))
C             = PARTW(3,x) * Tdrift ** 2 + PARTW(4,x) * Tdrift + PARTW(5,x)
C               (for the non-uniform drift region: 0 < Tdrift < TWLIMT(x))
C
      DATA  INIT/0/
C----------------------------------------------------------------------
C
      IF (INIT.EQ.0) THEN
        INIT = 1
        NBPBIN = 1000.0 / 106.0              ! 1 FADC bin = 9.4 ns
        DO TYPE = 0, 2
          COEFF(1,TYPE) = 1.
          COEF2(1,TYPE) = 1.
          DO 301 I = 2, 20
            COEFF(I,TYPE) = COEFF(I-1,TYPE) * PULWEI(TYPE)
            COEF2(I,TYPE) = COEFF(I,TYPE) * FLOAT(I)
  301     CONTINUE
        ENDDO
        IF (SCALE(DTTYPE) .LE. 0.0) SCALE(DTTYPE) = 1.0
      ENDIF
C
      DIGFLG = 0
      IF (TDRIFT.LE.0.0) GOTO 999
      CALL VZERO(FADC,20)
      IF (TDRIFT .LE. TRLIMT(DTTYPE)) THEN
        TRISE = PARTR(3,DTTYPE) * TDRIFT ** 2 +
     &          PARTR(4,DTTYPE) * TDRIFT + PARTR(5,DTTYPE)
      ELSE
        TRISE = PARTR(1,DTTYPE) * SQRT(TDRIFT) + PARTR(2,DTTYPE)
      ENDIF
      IF (TDRIFT .LE. TWLIMT(DTTYPE)) THEN
        TWIDTH = PARTW(3,DTTYPE) * TDRIFT ** 2 +
     &          PARTW(4,DTTYPE) * TDRIFT + PARTW(5,DTTYPE)
      ELSE
        TWIDTH = PARTW(1,DTTYPE) * SQRT(TDRIFT) + PARTW(2,DTTYPE)
      ENDIF
      IF (TWIDTH .LT. MINWID(DTTYPE)) TWIDTH = MINWID(DTTYPE)
      IF (TRISE .LT. MINRS(DTTYPE)) TRISE = MINRS(DTTYPE)
C
      IF (ITYPE .NE. 0) THEN
        TRISE = TRISE * DLSCAL(DTTYPE)
        TWIDTH = TWIDTH * DLSCAL(DTTYPE)
      ENDIF
C
      IWIDTH = NINT(TWIDTH)
      DO 100 I = 0, IWIDTH
        AMP = FADCFR(I, TWIDTH, TRISE)
C
C correct pulse shape of the leading edge
C
        IF (I.EQ.0) THEN
          AMP1 = AMP/4.
          FADC1 = AMP1 * AREA * SCALE(DTTYPE) / (1 + AMP1)
          FADC(1) = FADC1
        ENDIF
C
        AMP = AMP * AREA * SCALE(DTTYPE) / (1 + AMP1)
        FADC(2+I) = AMP
  100 CONTINUE
C
C      find the center of gravity of the positive part of
C      the differential pulse
C
      B(1) = 0.
      SUM = 0.
      SUMX = 0.
      DO 201 J = 2, 20
        B(J) = FADC(J) - FADC(J-1)
        IF (B(J) .LE. 0.) GOTO 202
        SUM = SUM + B(J) * COEFF(J-1,DTTYPE)
        SUMX = SUMX + B(J) * COEF2(J-1,DTTYPE)
  201 CONTINUE
  202 IF (SUM .EQ. 0.) THEN
        SUM = 1.
      ENDIF
      TMIN = TDRIFT/NBPBIN - SUMX/SUM
C
C  fill the raw data array
C
      ITMIN = NINT(TMIN)
      IF (ITMIN .LT. 0) ITMIN = 0
      IF( ITMIN+IWIDTH .GE. 255 ) IWIDTH = 255 - ITMIN - 1
      DO 203 I = 0, IWIDTH + 1
        PULSE(ITMIN+I) = PULSE(ITMIN+I) + FADC(I+1)
  203 CONTINUE
      DIGFLG = 1
  999 CONTINUE
C
      RETURN
      END
