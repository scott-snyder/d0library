C DEC/CMS REPLACEMENT HISTORY, Element JARC.FOR
C *4    21-MAR-1989 15:35:49 ABACHI ""
C *3    18-JAN-1989 01:48:59 ABACHI ""
C *2    11-JAN-1989 22:21:46 ABACHI ""
C *1     6-DEC-1988 13:24:56 ABACHI "EAS EMULATOR ROUTINES"
C DEC/CMS REPLACEMENT HISTORY, Element JARC.FOR
      SUBROUTINE JARC (XCEN, YCEN, ZCEN, RAD, NSTEP, A0, A1)
C
C    Purpose:
CD   This module draws a arc as a graphics primitive. The parameters
CD   passed are the center of the arc (XCEN, YCEN, ZCEN), the radius
CD   of the arc (RAD), and the number of segments used to draw the
CD   arc (NSTEP). The arc is drawn from angles A0 to A1; these angles are
CD   measured with respect to the plus X-axis in degrees. If the number
CD   of segments is less than 1 then an internal algorithym is used to
CD   calculate the number of segments based on the radius specified and
CD   the world coordinates. The arc is drawn such that the normal to the
CD   center is parallel to the Z-axis.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 20-Jan-1989
CH   History:
CH      06-JUL-91  Nobuaki Oshima(Fix a bug on integer stepping. )
CH      20-JAN-89  ATV  Changed the do loop to integer stepping.
CH      19-JAN-89  SA   Direction of drawing corrected.
CH      18-JAN-89  SA   Algorithm for step size modified.
CH      10-JAN-89  ATV  Corrected step algorithm for smoothness.
CH      15-AUG-88  ATV  Rename NSEGx to NSTEP to avoid conflicts with
CH                      the overhauled segments
CH      30-JUN-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      GRFPAR-R, SEGINF-R, PRIMVR-W
C
C    Calls:
CC      ERROR, KQUEV
C
C    Next is the declaration of parameters passed to the subroutine/function.
      REAL XCEN, YCEN, ZCEN, RAD, A0, A1
      INTEGER NSTEP
C
C    Then local declarations of variables (non-common variables).
C
      REAL XPT, YPT, ZPT, XRAT, YRAT, STEP, THETA, DANG
      REAL ASTRT, AEND, FRAC
      INTEGER NSTEPS, I
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:PRIMVR.INC/LIST'
C
C    Then executable code follows
C
      IF (.NOT. SEGOPN) THEN
        CALL ERROR('NO SEGMENT IS OPEN')
      ENDIF
      DANG = A1 - A0
      IF (ABS(DANG) .GT. 360.01) THEN
        CALL ERROR('ABS ANG EXCEEDS 360 DEGREES')
      ENDIF
      IF (NSTEP .LT. 1) THEN
        XRAT = RAD * SCV(1)
        YRAT = RAD * SCV(2)
        FRAC = ABS(DANG) / 360.0
        NSTEPS = MIN ( INT(100.0 * FRAC),
     +     MAX( INT(100.0 * FRAC * AMAX1(XRAT, YRAT)),
     +     INT(20.0 * FRAC) ) )
      ELSE
        NSTEPS = NSTEP
      ENDIF
      IF (NSTEPS .LT. 1)   NSTEPS = 1
      ASTRT = A0
      AEND  = A1
      STEP = DANG / FLOAT(NSTEPS)
      ZPT = ZCEN
      XPT = RAD * COSD(ASTRT) + XCEN
      YPT = RAD * SIND(ASTRT) + YCEN
      CALL KQUEV(XPT, YPT, ZPT, 'MOVE')
      THETA = ASTRT
      DO 10 I=1,NSTEPS + 1
        THETA = THETA + STEP
        XPT = RAD * COSD(THETA) + XCEN
        YPT = RAD * SIND(THETA) + YCEN
        CALL KQUEV(XPT, YPT, ZPT, 'DRAW')
   10 CONTINUE
      CPX = XCEN
      CPY = YCEN
      CPZ = ZCEN
      RETURN
      END
