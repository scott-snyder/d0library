      SUBROUTINE JCIRCL(XCEN, YCEN, ZCEN, RAD, NSTEP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
CD   This module draws a circle as a graphics primitive. The parameters
CD   passed are the center of the circle (XCEN, YCEN, ZCEN), the radius
CD   of the circle (RADIUS), and the number of segments used to draw the
CD   circle (NSTEP). If the number of segments is less than 3 then an
CD   internal algorithym is used to calculate the number of segments
CD   based on the radius specified and the world coordinates. The circle
CD   is drawn such that the normal to the center is parallel to the
CD   Z-axis.
C-
C-   Inputs  : XCEN, YCEN, ZCEN, RAD, NSTEP
C-   Outputs : NSTEP
C-   Controls: 
C-
C-   Created  09-JAN-1989   A. VIRGO, S. ABACHI
C-   UPDATED  18-JAN-1989   S. ABACHI      Step algorthm modified
C-   UPDATED  11-JUN-1990   S. ABACHI      Real polygons were used
C    Updated  24-MAR-2004   sss - compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL XCEN, YCEN, ZCEN, RAD
      INTEGER NSTEP
C
      REAL XRAT, YRAT, STEP, XPT, YPT, ZPT, THETA, INTENS
      INTEGER NSTEPS, I, MAXN, NVERP(1000)
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:LINATT.INC/LIST'
      INCLUDE 'D0$INC:PLGATT.INC/LIST'
      INCLUDE 'D0$INC:PRIMVR.INC/LIST'
      INCLUDE 'D0$INC:pi.def'
      DATA MAXN /50/
C
      IF (.NOT. SEGOPN) THEN
        CALL ERROR('JCIRCL: NO SEGMENT IS OPEN')
      ENDIF
      IF (NSTEP .LT. 3) THEN
        XRAT = RAD * SCV(1)
        YRAT = RAD * SCV(2)
        NSTEPS = MIN(100, MAX(INT(FLOAT(MAXN)*AMAX1(XRAT, YRAT)), 20))
      ELSE
        NSTEPS = NSTEP
      ENDIF
      IF(NSTEPS .GT. MAXN) NSTEPS = MAXN
      STEP = 360.0 / FLOAT(NSTEPS)
      ZPT = ZCEN*RIGHT
      INTENS = FLOAT(CINTEN) / 32767.0
      NVERT = 0
      THETA = 0.0
      DO 10 I=1, NSTEPS + 1
        NVERT = NVERT + 1
        XPT = RAD * COS(THETA*pi/180) + XCEN
        YPT = RAD * SIN(THETA*pi/180) + YCEN
        VERTIC(1,NVERT) = XPT
        VERTIC(2,NVERT) = YPT
        VERTIC(3,NVERT) = ZPT
        VERTIC(4,NVERT) = INTENS
        THETA = THETA + STEP
   10 CONTINUE
      NVERP(1) = NSTEPS
      CALL KPLGN(1,NVERP)
      CPX = XCEN
      CPY = YCEN
      CPZ = ZCEN
      RETURN
      END
