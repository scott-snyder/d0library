      SUBROUTINE MTCL2_ZIMPACT( PPNT,PCOS, cdist,cpnt,ierr)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : find the point on the input line that is
C-    closest to the z-axis within a cylinder of radius 16.2 cm
C-    (the outer radius of the central detector).
C-    The closest point is the average location of the two points
C-    of intersection of a line with a cylinder centered on z-axis.
C-
C-   Inputs  : ppnt(3) - x,y,z coordinate of a point on the input line
C-             pcos(3) - direction cosines of the input line
C-
C-   Outputs : cdist   - perpendicular distance from cpnt(3) to z-axis
C-                       (=-1 if distance is greater than cal outer rad)
C-             cpnt(3) - x,y,z coordinate of the point on the input
C-                       line closest to the z-axis
C-             ierr    = -2 - line does not enter CC cal outer radius
C-                     = -1 - input line parallel to z-axis
C-                     =  0 - special case - line || to x or y axis
C-                     = +1 - line passes inside vertex chamber
C-                     = +2 - line passes inside TRD (outside VTX)
C-                     = +3 - line passes inside central drift chamber
C-                     = +4 - line passes inside calorimeter (outside CD)
C-                     
C-
C-   Created  25-OCT-1994 Elizabeth Gallas - All special cases accounted
C-                        for.  Should never have divide by zero errors.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C- Input
      REAL    ppnt(3),pcos(3)
C- Output
      REAL    cdist,cpnt(3)
      INTEGER ierr
C- outer radius of VTX, TRD, CD and CAL (in cm)
      INTEGER irad
      REAL    cdrad(4)
      DATA    cdrad/ 16.2,47.0,71.9,218.7/
C- intersection points of the line on the cylinder outer radious
      REAL    pnt1(3),pnt2(3)
C- local
      REAL*8  a,b,c,D,slp
C----------------------------------------------------------------------
C- first look for special cases
      IF(pcos(1).EQ.0. .OR. pcos(2).EQ.0.) THEN
        IF(pcos(1).EQ.0. .AND. pcos(2).EQ.0.) THEN
C- line is parallel to z-axis
          cpnt(1) = ppnt(1)
          cpnt(2) = ppnt(2)
          cpnt(3) = ppnt(3)
          cdist   = sqrt (cpnt(1)**2 + cpnt(2)**2)
          ierr    = -1
C- line is perpendicular to x-axis, but not to y-axis
        ELSE IF(pcos(1).EQ.0.) THEN
          cpnt(1) = ppnt(1)
          cpnt(2) = 0.
          cpnt(3) = (pcos(3)/pcos(2)) * (-ppnt(2)) + ppnt(3)
          cdist   = ppnt(1)
          ierr    = 0
C- line is perpendicular to y-axis, but not to x-axis
        ELSE IF(pcos(2).EQ.0.) THEN
          cpnt(1) = 0.
          cpnt(2) = ppnt(2)
          cpnt(3) = (pcos(3)/pcos(1)) * (-ppnt(1)) + ppnt(3)
          cdist   = ppnt(2)
          ierr    = 0
        ENDIF
        RETURN
      ENDIF
C- Solve the quadratic formula to find the 2 points in which the input
C- line passes through a cylinder with radius cdrad centered on the z=axis.
      slp = pcos(2) / pcos(1)
      a   = slp**2 + 1.
      b   = +2. * slp * (ppnt(2) - ppnt(1)*slp)

C- loop over VTX, TRD, CD radii finding smallest where discriminant>0
C- a complex solution (d<0) indicates the line and cylinder do not intersect
      DO 17 irad=1,4
        C = (ppnt(1)*slp - ppnt(2))**2 - (cdrad(irad))**2
        D = b**2 - 4.*a*c
        ierr = irad
        IF(D.GE.0.) go to 18
   17 CONTINUE
      cpnt(1) = -100.
      cpnt(2) = -100.
      cpnt(3) = -100.
      cdist   = -1.
      ierr    = -2
      RETURN

   18 CONTINUE
C- Solve for x points
      pnt1(1) = (-b + dsqrt(D) ) / (2.*a)
      pnt2(1) = (-b - dsqrt(D) ) / (2.*a)
C- Now get y and z coordinates of the 2 intersection points
      pnt1(2) = (pnt1(1) - ppnt(1))*slp + ppnt(2)
      pnt2(2) = (pnt2(1) - ppnt(1))*slp + ppnt(2)
      IF(pcos(3).NE.0.) THEN
        slp     = pcos(3) / pcos(1)
        pnt1(3) = (pnt1(1) - ppnt(1))*slp + ppnt(3)
        pnt2(3) = (pnt2(1) - ppnt(1))*slp + ppnt(3)
      ELSE
        pnt1(3) = ppnt(3)
        pnt2(3) = ppnt(3)
      ENDIF
C- average position of the 2 intersect points is the closest approach
      cpnt(1) = (pnt1(1) + pnt2(1)) / 2.
      cpnt(2) = (pnt1(2) + pnt2(2)) / 2.
      cpnt(3) = (pnt1(3) + pnt2(3)) / 2.
C- get the perpendicular distance from cpnt(3) to the z axis
      cdist   = sqrt (cpnt(1)**2 + cpnt(2)**2)
C----------------------------------------------------------------------
  999 RETURN
      END
