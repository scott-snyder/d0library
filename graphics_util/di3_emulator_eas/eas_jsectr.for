        SUBROUTINE JSECTR (XCEN, YCEN, ZCEN, RAD, NSTEP, A0, A1)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
CD   This module draws a sector as a graphics primitive. The
CD   parameters passed are the center of the sector (XCEN, YCEN,
CD   ZCEN), the radius of the sector (RAD), and the number of
CD   segments used to draw the sector (NSTEP). The sector is drawn from
CD   angles A0 to A1; these angles are measured with respect to the
CD   plus X-axis in degrees. If the number of segments is less than 1
CD   then an internal algorithym is used to calculate the number of
CD   segments based on the radius specified and the world coordinates.
CD   The sector is drawn such that the normal to the center is
CD   parallel to the Z-axis. Lines are also drawn to the center point.
C-
C-   Inputs  : XCEN, YCEN, ZCEN, RAD, NSTEP, A0, A1
C-   Outputs : nstep
C-   Controls: 
C-
C-   Created  30-JUN-1988   A. VIRGO
C-   UPDATED  15-MAR-1989   S. ABACHI    Algorithm for NSTEPS modified.
C-   UPDATED  10-MAy-1990   S. ABACHI    Real polygons were used
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL XCEN, YCEN, ZCEN, RAD, A0, A1
      INTEGER I, NSTEP
      REAL XPT, YPT, ZPT, XRAT, YRAT, STEP, THETA, ABSANG
      REAL ASTRT, AEND, INTENS, FRAC
      INTEGER NSTEPS, NVERP(1000)
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:PRIMVR.INC/LIST'
      INCLUDE 'D0$INC:LINATT.INC/LIST'
      INCLUDE 'D0$INC:PLGATT.INC/LIST'
C
      IF (.NOT. SEGOPN) THEN
         CALL ERROR('JSECTR: NO SEGMENT IS OPEN')
      ENDIF
      IF (ABS(A1-A0) .GT. 360.0) THEN
         CALL ERROR('JSECTR: ABS ANG EXCEEDS 360 DEGREES')
      ENDIF
      IF (NSTEP .LT. 1) THEN
         XRAT = RAD * SCV(1)
         YRAT = RAD * SCV(2)
         FRAC = ABS(A1-A0)/360.0
         NSTEPS = MIN ( INT(100.0 * FRAC),
     +                MAX( INT(100.0 * FRAC * AMAX1(XRAT, YRAT)),
     +                INT(20.0 * FRAC) ) )
      ELSE
         NSTEPS = NSTEP
      ENDIF
      ASTRT = MIN(A0,A1)
      AEND  = MAX(A0,A1)
      ABSANG = ABS(A1 - A0)
      STEP = ABSANG / FLOAT(NSTEPS)
      ZPT = ZCEN
      INTENS = FLOAT(CINTEN) / 32767.0
      NVERT = 1
      VERTIC(1,NVERT) = XCEN
      VERTIC(2,NVERT) = YCEN
      VERTIC(3,NVERT) = ZCEN
      VERTIC(4,NVERT) = INTENS
      THETA = ASTRT
      DO 10 I=1, NSTEPS + 1
         NVERT = NVERT + 1
         XPT = RAD * COSD(THETA) + XCEN
         YPT = RAD * SIND(THETA) + YCEN
         VERTIC(1,NVERT) = XPT
         VERTIC(2,NVERT) = YPT
         VERTIC(3,NVERT) = ZPT
         VERTIC(4,NVERT) = INTENS
         THETA = THETA + STEP
   10 CONTINUE
      NVERT = NVERT + 1
      VERTIC(1,NVERT) = XCEN
      VERTIC(2,NVERT) = YCEN
      VERTIC(3,NVERT) = ZCEN
      VERTIC(4,NVERT) = INTENS
      NVERP(1) = NSTEPS
      CALL KPLGN(1,NVERP)
      CPX = XCEN
      CPY = YCEN
      CPZ = ZCEN
      RETURN
      END
