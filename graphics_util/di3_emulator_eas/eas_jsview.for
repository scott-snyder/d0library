      SUBROUTINE JSVIEW(XVRP, YVRP, ZVRP, DIST, YANG, XZAN, VANG)
C
C    Purpose:
CD   This module provides for the changing of the viewing transfomation 
CD   with one call.  The parameters passed in are the (X, Y, Z) view
CD   reference point (XVRP, YVRP, ZVRP), the spherical eyepoint (DIST,
CD   YANG, XZAN) (where YANG is the angle of rotation about the Y-axis 
CD   from the negative Z-axis and XZAN is the angle of rotation either
CD   above or below the X-Z plane of the eyepoint), and a viewing angle
CD   (VANGLE). All parameters are Reals and input. 
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 09-Jan-1989
CH   History:
CH      27-MAR-89  SA   NORML was correctly normalized.
CH      26-MAR-89  SA   Formula for DZ, XP, PERSP, WSF corrected.
CH      14-MAR-89  SA   Direction of NORML was corrected.
CH      14-MAR-89  SA   Third component of vupnt and norml were corrected.
CH      09-JAN-88  ATV  Correct window scaling.
CH      20-DEC-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      GRFPAR-W, SEGINF-R
C
C    Calls:
CC      ERROR.
C
      EXTERNAL ERRHND
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
      REAL XVRP, YVRP, ZVRP, DIST, YANG, XZAN, VANG
C
C    Then local declarations of variables (non-common variables).
C
      REAL EPS, DX, DY, DZ, RHO, XP, YP, ZP, WSF
      INTEGER I
      DATA EPS /1.0E-9/
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
C
C    Then executable code follows
C
      IF (SEGOPN) THEN 
         CALL ERROR('JSVIEW: A SEGMENT IS OPEN')
      ENDIF
      IF (VANG .GT. 179.0) THEN
         CALL ERROR('JSVIEW: VANGLE NOT IN RANGE (VANGLE > 179.0)')
      ENDIF
      IF (DIST .LT. EPS) THEN
         CALL ERROR ('JSVIEW: DISTANCE BETWEEN EYE & VIEW NOT > 0')
      ENDIF
      RHO = DIST * COSD(XZAN)
      XP = RHO * SIND(YANG) * RIGHT  ! CAUTION: Remains in left handed system.
      YP = DIST * SIND(XZAN)
      ZP = RHO * (-COSD(YANG))
      DX = XVRP - XP
      DY = YVRP - YP
      DZ = (ZVRP * RIGHT - ZP) * RIGHT
      VUPNT(1) = XVRP
      VUPNT(2) = YVRP
      VUPNT(3) = ZVRP
      NORML(1) = DX / DIST
      NORML(2) = DY / DIST
      NORML(3) = DZ / DIST
      PRJTYP   = PTPERS
      PERSP    = DIST
      IF (VANG .GT. 0.0) THEN
         WSF = DIST * TAND(VANG / 2.0)
         DO 10 I=1,3,2
            UWIND(I)     = -WSF
            UWIND(I + 1) =  WSF
   10    CONTINUE
      ENDIF
      RETURN
      END
