      SUBROUTINE CLNRD(VTX,DIR,RADIUS,NS,S)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find the intersections of a line with a
C-                         cylinder of given radius
C-
C-   Inputs  : VTX(3)  A point on the line
C-             DIR(3)  The direction cosines of the line
C-             RADIUS  The radius of the cylinder (axis = z-axis)
C-   Outputs : NS      The number of s-values returned (0 or 2)
C-             S(2)    The list of s-values
C-   Controls: 
C-
C-   Created  24-JUL-1989   Michael W. Peters
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      REAL VTX(3),DIR(3),RADIUS
      INTEGER NS
      REAL S(2)
      DOUBLE PRECISION A,B,C,RT
C
        A=DIR(1)**2+DIR(2)**2
        B=DIR(1)*VTX(1)+DIR(2)*VTX(2)
        C=VTX(1)**2+VTX(2)**2-RADIUS**2
        RT=B**2-A*C
        IF(A.EQ.0..OR.RT.LT.0.) THEN
          NS=0
        ELSE
          RT=SQRT(RT)
          S(1)=(-B+RT)/A
          S(2)=(-B-RT)/A
          NS=2
        ENDIF
  999 RETURN
      END
